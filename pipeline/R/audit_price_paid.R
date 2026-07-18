# Stage 6c: how much Price Paid data is (still) missing a UPRN.
#
# land_registry_add_uprn() leans on the UBDC transaction<->UPRN linkage,
# which only covers sales up to 2022 (see rematch_price_paid_unmatched(),
# price_paid.R) - so the match rate is expected to fall for 2023+ years
# until the rematch pass (run against the richer address infrastructure
# built later in the DAG) recovers some of it. Broken out by year so that
# cliff, and how much of it gets closed, is visible instead of being
# averaged away into one headline percentage.

audit_price_paid_uprn_match <- function(house_price_lr_uprn, house_price_lr_rematch) {
  hp <- house_price_lr_uprn
  hp$year <- lubridate::year(hp$date)
  hp$matched_initial <- !is.na(hp$uprn)
  hp$matched_rematch <- hp$transactionid %in% house_price_lr_rematch$matched$transactionid

  by_year <- hp |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_matched_initial = sum(matched_initial),
      n_matched_rematch = sum(matched_rematch),
      .groups = "drop"
    ) |>
    dplyr::arrange(year) |>
    dplyr::mutate(year = as.character(year))

  overall <- data.frame(
    year = "TOTAL",
    n = nrow(hp),
    n_matched_initial = sum(hp$matched_initial),
    n_matched_rematch = sum(hp$matched_rematch),
    stringsAsFactors = FALSE
  )

  out <- dplyr::bind_rows(by_year, overall)
  out$n_matched_total <- out$n_matched_initial + out$n_matched_rematch
  out$n_unmatched <- out$n - out$n_matched_total
  out$pct_matched_initial <- round(out$n_matched_initial / out$n, 4)
  out$pct_matched_total <- round(out$n_matched_total / out$n, 4)

  overall_row <- out[out$year == "TOTAL", ]
  message(
    "Price Paid -> UPRN match: ", overall_row$n_matched_initial, " of ", overall_row$n,
    " (", sprintf("%.1f%%", 100 * overall_row$pct_matched_initial),
    ") matched before the rematch pass; rematch recovered ",
    overall_row$n_matched_rematch, " more (",
    sprintf("%.1f%%", 100 * overall_row$pct_matched_total),
    " total); ", overall_row$n_unmatched, " transactions remain unmatched."
  )
  out
}
