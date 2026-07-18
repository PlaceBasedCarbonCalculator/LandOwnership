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

# --- Flat grouping: one paid geocode per block of flats ---------------------
#
# "Flats 1-70 Constantine House, 14 Boulevard Drive, London (NW9 5XD)" is
# split upstream into 70 rows ("1 Constantine House", "2 Constantine House",
# ...) that all geocode to the SAME point - the building. Sending all 70 to
# Azure wastes 69 paid calls. Rows that are flats of one building (same
# Title Number, same postcode, same base address once the leading flat
# number is stripped) are grouped: one representative is left "pending" and
# the rest are parked as "deferred_flat", which run_geocode_batch() never
# sends. After the representative is geocoded, infer_flat_locations()
# (pipeline/R/geocode_flat_infer.R) copies its location to the deferred
# members - but only after verifying against the UPRN data that at least
# group-size UPRNs share an identical location at that point (70 flats =>
# >= 70 co-located UPRNs). If that check fails the members are released
# back to "pending" and geocoded individually, so a wrong grouping costs
# nothing but a delay.

# Base address of a flat row: strip the leading flat number (and any
# flat/apartment designator that survived cleaning) and require what's left
# to start with a building-style name ("Constantine House", "Kings Court").
# Returns NA where the row doesn't look like a flat in a named building -
# "5 Mill Fold, Ripponden" is a house on a street, not a flat, and each
# house has its own location, so street-suffix bases are never grouped.
flat_base_address <- function(address_line) {
  x <- ifelse(is.na(address_line), "", address_line)
  base <- stringi::stri_replace_first_regex(
    x, "^\\s*(flats?|apartments?|apt|unit)?\\s*,?\\s*[0-9]+[a-z]?\\s*(,\\s*|\\s+)", "",
    opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
  )
  changed <- base != x & base != ""
  first_seg <- toupper(trimws(sub(",.*$", "", base)))
  building_rx <- paste0(
    "\\b(HOUSE|COURT|LODGE|TOWERS?|MANSIONS?|HEIGHTS|POINT|HALL|",
    "APARTMENTS|BUILDINGS?|BLOCK|CHAMBERS)$"
  )
  ok <- changed & grepl(building_rx, first_seg)
  ifelse(ok, toupper(trimws(gsub("\\s+", " ", base))), NA_character_)
}

# Assign flat groups across the whole queue and park non-representative
# members as "deferred_flat". Idempotent - safe to re-run on every rebuild:
#   - spent rows (done / failed / inferred) never change status;
#   - a group whose UPRN check already failed (flat_check == "failed") is
#     never re-deferred - its members geocode individually;
#   - if the current representative failed, the next member is promoted so
#     the group gets another attempt.
# min_group = 3: pairs are as likely to be two houses as two maisonettes,
# and deferring them saves a single call - not worth the false-positive risk.
assign_flat_groups <- function(queue, min_group = 3) {
  for (col in c("flat_group", "flat_rep_key", "flat_check")) {
    if (!col %in% names(queue)) queue[[col]] <- NA_character_
  }
  if (!"flat_group_n" %in% names(queue)) queue$flat_group_n <- NA_integer_

  base <- flat_base_address(queue$AddressLine)
  loc <- ifelse(!is.na(queue$PostalCode) & queue$PostalCode != "",
    queue$PostalCode, paste0("D:", queue$District)
  )
  gk <- ifelse(is.na(base), NA_character_,
    paste(queue$`Title Number`, base, loc, sep = "||")
  )

  # groups disproved by the UPRN check are permanently exempt
  failed_gk <- unique(gk[!is.na(queue$flat_check) & queue$flat_check == "failed"])
  failed_gk <- failed_gk[!is.na(failed_gk)]

  geocodable <- queue$status %in% c("pending", "deferred_flat", "done", "failed", "inferred")
  candidate <- !is.na(gk) & geocodable & !gk %in% failed_gk
  tab <- table(gk[candidate])
  big <- names(tab)[tab >= min_group]

  # release stale deferrals (group shrank below min_group, cleaning changed,
  # or the group has since failed its UPRN check)
  stale <- queue$status == "deferred_flat" & (!candidate | !gk %in% big)
  queue$status[stale] <- "pending"

  # wipe grouping bookkeeping except the failed-check audit trail
  keep_audit <- !is.na(queue$flat_check) & queue$flat_check == "failed"
  wipe <- (!candidate | !gk %in% big) & !keep_audit
  queue$flat_group[wipe] <- NA_character_
  queue$flat_group_n[wipe] <- NA_integer_
  queue$flat_rep_key[wipe] <- NA_character_

  for (g in big) {
    m <- which(gk == g & candidate)
    queue$flat_group[m] <- g
    queue$flat_group_n[m] <- length(m)

    st <- queue$status[m]
    if (any(st == "done")) {
      # a member already geocoded anchors the group
      rep_i <- m[which(st == "done")[1]]
    } else {
      unsent <- m[st %in% c("pending", "deferred_flat")]
      if (length(unsent) == 0) next # everything failed/inferred already
      num <- suppressWarnings(as.numeric(
        stringi::stri_extract_first_regex(queue$AddressLine[unsent], "[0-9]+")
      ))
      rep_i <- unsent[order(num, queue$queue_key[unsent])][1]
    }
    queue$flat_rep_key[m] <- queue$queue_key[rep_i]
    if (queue$status[rep_i] == "deferred_flat") queue$status[rep_i] <- "pending"
    others <- setdiff(m, rep_i)
    park <- others[queue$status[others] == "pending"]
    queue$status[park] <- "deferred_flat"
  }
  queue
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
    # keep every row that has actually been sent to Azure or already carries
    # an inferred location (its spend/result is real, whatever the current
    # cleaning produces); drop unsent rows whose key isn't in the current
    # unmatched set (stale cleaning output)
    spent <- existing[existing$status %in% c("done", "failed", "inferred"), ]
    unspent <- existing[!existing$status %in% c("done", "failed", "inferred"), ]
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

  # flat grouping runs on the merged queue so groups can span new rows and
  # rows carried over from earlier runs (including an already-done rep)
  combined <- assign_flat_groups(combined)
  n_deferred <- sum(combined$status == "deferred_flat", na.rm = TRUE)
  if (n_deferred > 0) {
    n_groups <- length(unique(combined$flat_group[combined$status == "deferred_flat"]))
    message(
      n_deferred, " flat addresses deferred across ", n_groups,
      " buildings - only each building's representative flat will be sent ",
      "to Azure; run infer_flat_locations() after geocoding to fill the rest."
    )
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
