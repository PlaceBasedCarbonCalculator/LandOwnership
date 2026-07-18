# Coverage checks for the UPRN/USRN address pipeline (uprn_infill.R,
# uprn_master.R). Cheap, read-only summaries computed at each key stage so a
# regression in address coverage (e.g. the extract_street_name() comma bug,
# or the price-paid PAON-only street bug - see git history) shows up as a
# number in `tar_read(*_coverage)` instead of only being noticed
# anecdotally in the size of the geocode queue. Expectation, by the end of
# the pipeline: almost every UPRN should have a real or inferred address.

# One row of coverage stats for a table of address-ish rows: how many have
# a house number / street / non-empty postcode / VALIDLY FORMATTED postcode
# (is_valid_postcode(), utils.R). `label` just tags the row so several
# stages/sources can be stacked into one data frame for easy comparison.
address_coverage_stats <- function(df, label, house_col = "house_number",
                                   street_col = "street", postcode_col = "postcode") {
  n <- nrow(df)
  has_val <- function(col) {
    if (!col %in% names(df)) {
      return(NA_integer_)
    }
    sum(!is.na(df[[col]]) & df[[col]] != "")
  }
  n_pc <- has_val(postcode_col)
  n_pc_valid <- if (postcode_col %in% names(df)) sum(is_valid_postcode(df[[postcode_col]])) else NA_integer_

  data.frame(
    stage = label,
    n_rows = n,
    n_house_number = has_val(house_col),
    pct_house_number = round(has_val(house_col) / pmax(n, 1), 4),
    n_street = has_val(street_col),
    pct_street = round(has_val(street_col) / pmax(n, 1), 4),
    n_postcode = n_pc,
    pct_postcode = round(n_pc / pmax(n, 1), 4),
    n_postcode_valid = n_pc_valid,
    pct_postcode_malformed = round((n_pc - n_pc_valid) / pmax(n_pc, 1), 4),
    stringsAsFactors = FALSE
  )
}

# Stage: known_uprn_addresses (EPC/Price-Paid/DEC/2022-geocode) - the
# foundation everything else (USRN naming, gap-guessing, free-matching) is
# built from. Broken out by address_source as well as an overall total,
# since a parsing regression usually hits only one source (e.g. price_paid)
# and would be diluted into invisibility in a single blended number.
audit_known_uprn_addresses <- function(known_uprn_addresses) {
  by_source <- split(known_uprn_addresses, known_uprn_addresses$address_source)
  per_source <- dplyr::bind_rows(lapply(names(by_source), function(s) {
    address_coverage_stats(by_source[[s]], s)
  }))
  overall <- address_coverage_stats(known_uprn_addresses, "TOTAL")
  out <- dplyr::bind_rows(per_source, overall)

  message(
    "known_uprn_addresses coverage: ", nrow(known_uprn_addresses), " rows, ",
    sprintf("%.1f%%", 100 * overall$pct_street), " have a parsed street ",
    "(", sprintf("%.1f%%", 100 * overall$pct_postcode_malformed),
    " of present postcodes are malformed)."
  )
  out
}

# Stage: USRN street naming (build_usrn_street_names()) - what fraction of
# ALL USRNs an infill candidate could sit on (the uprn_usrn universe)
# actually got a name, and how many of those also got a district/postcode.
# usrn_street_names itself only ever contains NAMED usrns (every row has a
# non-NA street by construction), so the denominator has to come from
# uprn_usrn - checking usrn_street_names alone would trivially show 100%.
audit_usrn_street_names <- function(uprn_usrn, usrn_street_names) {
  n_usrn_total <- length(unique(uprn_usrn$USRN))
  n_usrn_named <- nrow(usrn_street_names)
  n_district <- sum(!is.na(usrn_street_names$district))
  has_pc_col <- "postcode" %in% names(usrn_street_names)
  n_postcode <- if (has_pc_col) sum(!is.na(usrn_street_names$postcode)) else NA_integer_
  n_postcode_valid <- if (has_pc_col) sum(is_valid_postcode(usrn_street_names$postcode)) else NA_integer_

  out <- data.frame(
    n_usrn_total = n_usrn_total,
    n_usrn_named = n_usrn_named,
    pct_usrn_named = round(n_usrn_named / pmax(n_usrn_total, 1), 4),
    n_usrn_with_district = n_district,
    pct_usrn_with_district = round(n_district / pmax(n_usrn_named, 1), 4),
    n_usrn_with_postcode = n_postcode,
    n_usrn_postcode_valid = n_postcode_valid,
    stringsAsFactors = FALSE
  )
  message(
    out$n_usrn_named, " of ", out$n_usrn_total, " USRNs named (",
    sprintf("%.1f%%", 100 * out$pct_usrn_named), "); ",
    out$n_usrn_with_district, " (", sprintf("%.1f%%", 100 * out$pct_usrn_with_district),
    ") of those also have a district."
  )
  out
}

# Stage: UPRN infill (build_uprn_infill()) - what fraction of
# infill_candidates (UPRNs with no EPC/Price-Paid/DEC/2022-geocode address)
# gained ANY inferred information at all, plus house-number/street/postcode
# coverage among the rows that did.
audit_uprn_infill <- function(infill_candidates, uprn_infill) {
  n_candidates <- nrow(infill_candidates)
  n_gained <- length(unique(uprn_infill$UPRN))
  out <- address_coverage_stats(uprn_infill, "uprn_infill")
  out$n_candidates <- n_candidates
  out$n_gained_any_info <- n_gained
  out$pct_gained_any_info <- round(n_gained / pmax(n_candidates, 1), 4)

  message(
    n_gained, " of ", n_candidates, " infill candidates (",
    sprintf("%.1f%%", 100 * out$pct_gained_any_info),
    ") gained some inferred address information."
  )
  out
}

# Final stage: the published master table (uprn_master.R). The headline
# check - by the end of the pipeline almost every UPRN should have SOME
# usable address, either a real one (best_address) or an inferred one
# (infill_street / infill_house_number). Postcode is coalesced across the
# three places it can come from (a real address, the infill, or NSUL) since
# any one of them is enough to geocode against.
audit_uprn_all_addresses <- function(uprn_all_addresses) {
  n <- nrow(uprn_all_addresses)
  has_real <- !is.na(uprn_all_addresses$best_address)
  has_infill_street <- !is.na(uprn_all_addresses$infill_street)
  addressed <- has_real | has_infill_street

  has_any_street <- !is.na(uprn_all_addresses$best_street) | has_infill_street
  has_any_number <- !is.na(uprn_all_addresses$best_house_number) |
    !is.na(uprn_all_addresses$infill_house_number)

  pc_values <- dplyr::coalesce(
    uprn_all_addresses$best_postcode,
    uprn_all_addresses$infill_postcode,
    uprn_all_addresses$postcode_nsul
  )
  n_pc_present <- sum(!is.na(pc_values))
  n_pc_valid <- sum(is_valid_postcode(pc_values))

  out <- data.frame(
    n_uprn = n,
    n_real_address = sum(has_real),
    pct_real_address = round(sum(has_real) / n, 4),
    n_addressed_any = sum(addressed),
    pct_addressed_any = round(sum(addressed) / n, 4),
    n_with_street = sum(has_any_street),
    pct_with_street = round(sum(has_any_street) / n, 4),
    n_with_house_number = sum(has_any_number),
    pct_with_house_number = round(sum(has_any_number) / n, 4),
    n_with_postcode = n_pc_present,
    pct_with_postcode = round(n_pc_present / n, 4),
    n_postcode_valid = n_pc_valid,
    pct_postcode_malformed = round((n_pc_present - n_pc_valid) / pmax(n_pc_present, 1), 4),
    n_unaddressed = sum(!addressed),
    stringsAsFactors = FALSE
  )
  message(
    "FINAL COVERAGE: ", out$n_addressed_any, " of ", out$n_uprn, " UPRNs (",
    sprintf("%.2f%%", 100 * out$pct_addressed_any),
    ") have a real or inferred street; ", out$n_unaddressed,
    " remain completely unaddressed. ", out$n_postcode_valid, " of ",
    out$n_with_postcode, " present postcodes are well-formed ",
    "(", sprintf("%.2f%%", 100 * out$pct_postcode_malformed), " malformed)."
  )
  out
}
