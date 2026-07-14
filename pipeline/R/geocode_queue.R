# Stage 8 (tasks 2/4): the persistent pending-geocode queue. Deliberately
# lives outside the pure targets DAG - see _targets.R, which tracks
# data/geocoding/queue.rds with a format = "file" target so anything
# downstream reruns whenever the file's content changes (e.g. after a
# manual geocode batch updates row statuses).
#
# Existing "done"/"failed" status is always preserved across reruns: this
# function only appends genuinely new queue_keys, it never resets progress
# that run_geocode_batch() has already made. Rows that are still "pending"
# (or "not_geocodable") but whose queue_key no longer appears in the
# current unmatched set - i.e. the cleaning logic changed and produced a
# different AddressLine - are dropped, so stale keys don't sit in the queue
# wasting quota (audit F10).

# Classify whether an address is worth a paid geocode call (audit F2).
# Returns "ok" or a reason string. Anything not "ok" is stored in the queue
# with status "not_geocodable" + queue_reason, so it can be reviewed, and
# is never picked up by run_geocode_batch().
classify_geocodability <- function(address, postcode) {
  address <- ifelse(is.na(address), "", address)
  has_pc <- !is.na(postcode) & postcode != ""
  n_char <- nchar(address)

  legalese_rx <- paste0(
    "filed at the registry|filed plan|registered under title|",
    "title number|deed dated|acts? (of )?(18|19|20)[0-9]{2}|",
    "perpetuity|easement|rentcharge|rent charge"
  )
  words <- stringi::stri_count_regex(address, "\\S+")
  stop_words <- stringi::stri_count_regex(
    address,
    "\\b(the|of|and|to|as|is|for|in|by|with|on|or|any|all|such|so|much)\\b",
    opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
  )

  # later assignments override earlier ones, so this runs from the broadest
  # diagnosis to the most specific
  reason <- rep("ok", length(address))
  reason[words > 8 & stop_words / pmax(words, 1) > 0.45] <- "legalese"
  reason[grepl(legalese_rx, address, ignore.case = TRUE) & n_char > 90] <- "legalese"
  reason[grepl("^Properties at\\b", address, ignore.case = TRUE)] <- "multi_property_list"
  # a bare house number is fine WITH a postcode (number + postcode is a
  # perfectly good geocode query) but hopeless without one
  bare_number <- grepl("^[0-9]+[A-Za-z]?$", trimws(address))
  no_letters <- !grepl("[A-Za-z]", address)
  reason[n_char < 5 & !has_pc] <- "too_short"
  reason[(bare_number | no_letters) & !has_pc] <- "bare_number_no_postcode"
  reason[grepl("@[A-Za-z]+", address)] <- "residual_tag"
  reason[n_char == 0] <- "empty"
  reason
}

build_geocode_queue <- function(unmatched, queue_path = "data/geocoding/queue.rds") {
  new_rows <- unmatched
  new_rows$queue_key <- paste(new_rows$`Title Number`, new_rows$AddressLine, sep = "||")
  new_rows <- new_rows[!duplicated(new_rows$queue_key), ]
  new_rows$queue_reason <- classify_geocodability(new_rows$AddressLine, new_rows$PostalCode)
  new_rows$status <- ifelse(new_rows$queue_reason == "ok", "pending", "not_geocodable")
  new_rows$attempts <- 0L
  new_rows$last_attempt_date <- as.Date(NA)

  if (file.exists(queue_path)) {
    existing <- readRDS(queue_path)
    if (!"queue_reason" %in% names(existing)) {
      existing$queue_reason <- NA_character_
    }
    # keep every row that has actually been sent to Azure (its spend is
    # real, whatever the current cleaning produces); drop unsent rows whose
    # key isn't in the current unmatched set (stale cleaning output)
    spent <- existing[existing$status %in% c("done", "failed"), ]
    unspent <- existing[!existing$status %in% c("done", "failed"), ]
    stale <- !unspent$queue_key %in% new_rows$queue_key
    kept <- unspent[!stale, ]
    to_add <- new_rows[!new_rows$queue_key %in% c(spent$queue_key, kept$queue_key), ]
    combined <- dplyr::bind_rows(spent, kept, to_add)
    message(
      nrow(to_add), " new addresses added to the geocode queue (",
      nrow(spent), " already geocoded, ", nrow(kept), " still queued, ",
      sum(stale), " stale unsent rows dropped)."
    )
  } else {
    combined <- new_rows
    message(nrow(combined), " addresses added to a new geocode queue.")
  }

  n_gated <- sum(combined$status == "not_geocodable", na.rm = TRUE)
  if (n_gated > 0) {
    message(
      n_gated, " addresses held back as not_geocodable (",
      paste(names(table(combined$queue_reason[combined$status == "not_geocodable"])),
        table(combined$queue_reason[combined$status == "not_geocodable"]),
        sep = ": ", collapse = ", "
      ), ")."
    )
  }

  dir.create(dirname(queue_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(combined, queue_path)
  combined
}
