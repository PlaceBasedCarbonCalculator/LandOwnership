# Stage 4 (task 3): coverage/failure diagnostics for the clean+split stage,
# comparable to the counts documented in the README's category table so we
# can see how the 2026 data behaves against the same categorisation.

audit_split_addresses <- function(categorised, split_result) {
  titles_by_category <- categorised |>
    dplyr::count(dataset, category, name = "n_titles")

  rows_by_category <- split_result |>
    dplyr::filter(parse_ok) |>
    dplyr::count(dataset, category, name = "n_addresses")

  # Source rows that never produced a single split row - i.e.
  # split_numbers_try() threw on them. nopc_complex rows are excluded, they
  # were deliberately set aside (substation/"part of"), not a parse failure.
  parsed_ids <- split_result$orig_row_id[split_result$parse_ok]
  parse_failures <- categorised[
    !categorised$orig_row_id %in% parsed_ids & categorised$category != "nopc",
  ]
  # (nopc rows are handled separately below since some are deliberately
  # routed to nopc_complex rather than failing to parse)
  nopc_parsed_or_complex <- split_result$orig_row_id[
    split_result$category %in% c("nopc", "nopc_complex")
  ]
  nopc_failures <- categorised[
    categorised$category == "nopc" & !categorised$orig_row_id %in% nopc_parsed_or_complex,
  ]
  parse_failures <- dplyr::bind_rows(parse_failures, nopc_failures)

  long_residual <- split_result[
    split_result$parse_ok & nchar(split_result$AddressLine) > 90,
  ]

  list(
    titles_by_category = titles_by_category,
    addresses_by_category = rows_by_category,
    n_titles_total = nrow(categorised),
    n_addresses_total = sum(split_result$parse_ok),
    n_nopc_complex = sum(split_result$category == "nopc_complex", na.rm = TRUE),
    n_parse_failures = nrow(parse_failures),
    parse_failure_examples = utils::head(
      parse_failures[, c("Title Number", "Property Address", "category")], 50
    ),
    long_residual_rate = round(nrow(long_residual) / sum(split_result$parse_ok), 4),
    long_residual_sample = utils::head(
      long_residual[, c("Title Number", "AddressLine", "category")], 50
    )
  )
}
