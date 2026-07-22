# Stage 8 (tasks 2/4): the persistent pending-geocode queue. Deliberately
# lives outside the pure targets DAG - see _targets.R, which tracks
# data/geocoding/queue.rds with a format = "file" target so anything
# downstream reruns whenever the file's content changes (e.g. after a
# manual geocode batch updates row statuses).
#
# Existing "done"/"failed"/"inferred" rows are always preserved as-is across
# reruns: this function never resets progress that run_geocode_batch() or
# infer_flat_locations() has already made. Rows that are still "pending",
# "deferred_flat" or "not_geocodable" get their derived columns (PostalCode,
# District, category, status, queue_reason, ...) refreshed from the current
# cleaning output whenever their queue_key survives into the new unmatched
# set - a surviving key means AddressLine/Title Number didn't change, but
# other columns can still improve (see the refresh comment in
# build_geocode_queue() - 403 rows found stuck with a stale PostalCode NA
# from before a cleaning fix landed). Only real progress/audit state
# (attempts, last_attempt_date, flat_group/flat_rep_key/flat_check/
# flat_group_n) survives the refresh untouched. Rows whose queue_key no
# longer appears in the current unmatched set at all - i.e. the cleaning
# logic changed and produced a different AddressLine - are dropped, so
# stale keys don't sit in the queue wasting quota (audit F10).

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

# --- Ambiguous-street deprioritisation ---------------------------------------
#
# "55 Prospect Terrace" with no postcode isn't safely geocodable by name
# alone if Britain has several unrelated streets called "Prospect Terrace" -
# Azure has no more disambiguating context than we do, so a paid call is a
# coin toss. These rows are still queued (a human/Azure might resolve them
# with more context than this pipeline has), just sorted BEHIND every
# ordinary row - see queue_priority below and its use in
# geocode_batch_runner.R's run_geocode_batch().

# How many distinct districts (nationally, per usrn_street_names -
# uprn_infill.R) share each normalised street name. A name that only ever
# occurs in one district is safe to treat as unique even with no postcode;
# one spread across several is the "common name" shape that's ambiguous
# without a postcode to pin it down.
build_street_ambiguity_lookup <- function(usrn_street_names) {
  x <- usrn_street_names[!is.na(usrn_street_names$street), ]
  dt <- data.table::data.table(
    street_norm = normalise_name(x$street), district = x$district
  )
  agg <- dt[, .(n_district = data.table::uniqueN(district[!is.na(district)])), by = street_norm]
  as.data.frame(agg)
}

# TRUE when a row has NO postcode AND its extracted street name is shared
# by `min_district` or more districts nationally (default 3 - allows a
# little incidental name reuse, e.g. every town having *a* "Church Lane",
# without flagging every street that merely isn't perfectly unique).
# Rows whose street can't be extracted at all (extract_street_name()
# requires a leading house number) are never flagged here - they're not
# "ambiguous", they're just not this kind of row.
flag_ambiguous_street <- function(address, postcode, street_ambiguity, min_district = 3) {
  has_pc <- !is.na(postcode) & postcode != ""
  street_norm <- normalise_name(extract_street_name(address))
  ni <- match(street_norm, street_ambiguity$street_norm)
  n_district <- street_ambiguity$n_district[ni]
  !has_pc & !is.na(street_norm) & !is.na(n_district) & n_district >= min_district
}

# `street_ambiguity` (optional - build_street_ambiguity_lookup() above) flags
# rows for queue_priority; omit to leave every row at the default priority
# (e.g. in tests, or if usrn_street_names isn't available for some reason).
build_geocode_queue <- function(unmatched, queue_path = "data/geocoding/queue.rds",
                                street_ambiguity = NULL) {
  new_rows <- unmatched
  new_rows$queue_key <- paste(new_rows$`Title Number`, new_rows$AddressLine, sep = "||")
  new_rows <- new_rows[!duplicated(new_rows$queue_key), ]
  new_rows$queue_reason <- classify_geocodability(new_rows$AddressLine, new_rows$PostalCode)
  new_rows$status <- ifelse(new_rows$queue_reason == "ok", "pending", "not_geocodable")
  new_rows$attempts <- 0L
  new_rows$last_attempt_date <- as.Date(NA)

  # queue_priority: 0 = normal, 1 = deprioritised (ambiguous street, no
  # postcode - see flag_ambiguous_street() above). Never affects status/
  # queue_reason - these rows are still "ok"/"pending", just sorted last by
  # run_geocode_batch().
  new_rows$queue_priority <- 0L
  if (!is.null(street_ambiguity) && nrow(street_ambiguity) > 0) {
    ambiguous <- flag_ambiguous_street(new_rows$AddressLine, new_rows$PostalCode, street_ambiguity)
    new_rows$queue_priority[ambiguous] <- 1L
    n_amb <- sum(ambiguous & new_rows$status == "pending")
    if (n_amb > 0) {
      message(
        n_amb, " pending addresses deprioritised: no postcode and a street ",
        "name shared by several districts nationally (ambiguous without ",
        "more context than this pipeline or Azure has)."
      )
    }
  }

  if (file.exists(queue_path)) {
    existing <- readRDS(queue_path)
    if (!"queue_reason" %in% names(existing)) {
      existing$queue_reason <- NA_character_
    }
    if (!"queue_priority" %in% names(existing)) {
      existing$queue_priority <- 0L
    }
    # keep every row that has actually been sent to Azure or already carries
    # an inferred location (its spend/result is real, whatever the current
    # cleaning produces); drop unsent rows whose key isn't in the current
    # unmatched set (stale cleaning output)
    spent <- existing[existing$status %in% c("done", "failed", "inferred"), ]
    unspent <- existing[!existing$status %in% c("done", "failed", "inferred"), ]
    stale <- !unspent$queue_key %in% new_rows$queue_key
    kept_old <- unspent[!stale, ]

    # A surviving queue_key means AddressLine/Title Number are unchanged,
    # but that's not the same as "nothing about this row changed" - other
    # derived columns (PostalCode, District, category, ...) come from the
    # SAME cleaning code and can still improve between runs (e.g. the audit
    # F1 fix that backfills PostalCode from the registry Postcode column for
    # simple_short/simple_long titles). Before this fix, kept rows were
    # carried forward completely untouched, so any cleaning improvement
    # landing after a row first joined the queue could never reach it -
    # confirmed against a live queue: 403 pending rows still had PostalCode
    # NA despite the registry Postcode being present and the F1 backfill
    # already live in split_addresses.R (2026-07-21 Kirklees audit
    # follow-up). Take the fresh new_rows version of every column - status
    # and queue_reason included, since a fixed PostalCode can turn a
    # not_geocodable row into a geocodable one - except the columns below,
    # which are real progress/audit state that only run_geocode_batch(),
    # infer_flat_locations() or assign_flat_groups() itself should change.
    bookkeeping_cols <- intersect(
      c("attempts", "last_attempt_date", "flat_group", "flat_rep_key", "flat_check", "flat_group_n"),
      names(kept_old)
    )
    kept <- new_rows[match(kept_old$queue_key, new_rows$queue_key), ]
    kept[bookkeeping_cols] <- kept_old[bookkeeping_cols]

    pc_changed <- !is.na(kept_old$PostalCode) != !is.na(kept$PostalCode) |
      (!is.na(kept_old$PostalCode) & !is.na(kept$PostalCode) & kept_old$PostalCode != kept$PostalCode)
    n_refreshed <- sum(pc_changed, na.rm = TRUE)

    to_add <- new_rows[!new_rows$queue_key %in% c(spent$queue_key, kept$queue_key), ]
    combined <- dplyr::bind_rows(spent, kept, to_add)
    message(
      nrow(to_add), " new addresses added to the geocode queue (",
      nrow(spent), " already geocoded, ", nrow(kept), " still queued, ",
      n_refreshed, " of those picked up a PostalCode change from cleaning fixes, ",
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
