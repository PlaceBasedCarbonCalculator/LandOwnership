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
  # were deliberately set aside ("part of" descriptions with no usable
  # coordinate), not a parse failure. nopc_substation rows (electricity
  # substations - see substations.R) are routed through the ordinary
  # boilerplate pipeline instead, so they show up as parse successes, not
  # failures or nopc_complex.
  parsed_ids <- split_result$orig_row_id[split_result$parse_ok]
  parse_failures <- categorised[
    !categorised$orig_row_id %in% parsed_ids & categorised$category != "nopc",
  ]
  # (nopc rows are handled separately below since some are deliberately
  # routed to nopc_complex, or to nopc_substation, rather than failing to
  # parse)
  nopc_parsed_or_complex <- split_result$orig_row_id[
    split_result$category %in% c("nopc", "nopc_complex", "nopc_substation")
  ]
  nopc_failures <- categorised[
    categorised$category == "nopc" & !categorised$orig_row_id %in% nopc_parsed_or_complex,
  ]
  parse_failures <- dplyr::bind_rows(parse_failures, nopc_failures)

  long_residual <- split_result[
    split_result$parse_ok & nchar(split_result$AddressLine) > 90,
  ]

  # Cleaning-quality metrics (audit recommendation 10): these are the
  # regressions that previously only showed up as wasted Azure quota.
  ok <- split_result[split_result$parse_ok, ]
  al <- ok$AddressLine
  quality_metrics <- list(
    n_tag_leakage = sum(grepl("@[A-Za-z]+", al)),
    n_bare_number = sum(grepl("^[0-9]+[A-Za-z]?$", trimws(al))),
    n_leading_glue = sum(grepl(
      "^\\s*[,;:.)]|^(and|of|to|being|the site of|site of)\\b", al,
      ignore.case = TRUE
    )),
    postcode_presence_rate = round(
      mean(!is.na(ok$PostalCode) & ok$PostalCode != ""), 4
    ),
    n_identical_to_raw = sum(al == ok$`Property Address`, na.rm = TRUE),
    n_residual_boilerplate = sum(grepl(
      "\\b(filed plan|filed at the registry|deed dated|edged red|inclusive)\\b",
      al,
      ignore.case = TRUE
    )),
    n_unbalanced_paren = sum(
      stringi::stri_count_fixed(al, "(") != stringi::stri_count_fixed(al, ")")
    )
  )

  list(
    quality_metrics = quality_metrics,
    titles_by_category = titles_by_category,
    addresses_by_category = rows_by_category,
    n_titles_total = nrow(categorised),
    n_addresses_total = sum(split_result$parse_ok),
    n_nopc_complex = sum(split_result$category == "nopc_complex", na.rm = TRUE),
    n_nopc_substation = sum(split_result$category == "nopc_substation", na.rm = TRUE),
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
