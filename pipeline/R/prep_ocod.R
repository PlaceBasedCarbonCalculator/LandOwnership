# Stage 1: raw import + categorisation of the Overseas ownership (OCOD)
# data. Adapts R/prep_oversees_owners.R. Same by-name-column / stable-key
# audit fixes as prep_ccod.R.

ocod_columns <- c(
  "Title Number", "Tenure", "Property Address", "District", "County",
  "Region", "Postcode", "Proprietor Name (1)", "Company Registration No. (1)",
  "Proprietorship Category (1)", "Country Incorporated (1)"
)

import_ocod_raw <- function(zip_path) {
  tmp <- tempfile("ocod")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  utils::unzip(zip_path, exdir = tmp)
  csv <- list.files(tmp, pattern = "\\.csv$", full.names = TRUE)[1]
  lr <- readr::read_csv(csv, lazy = FALSE, show_col_types = FALSE)

  missing_cols <- setdiff(ocod_columns, names(lr))
  if (length(missing_cols) > 0) {
    stop(
      "OCOD schema has changed - missing expected column(s): ",
      paste(missing_cols, collapse = ", "),
      ". Update ocod_columns in pipeline/R/prep_ocod.R."
    )
  }

  lr <- lr[, ocod_columns]
  lr <- lr[!is.na(lr$`Title Number`), ]
  lr[!duplicated(lr$`Title Number`), ]
}

# Unlike CCOD freehold, OCOD isn't split into short/long/land/nopc
# sub-categories in the original pipeline - every title goes through the
# same "boilerplate" cleaning treatment (mines/compass/land/flats phrase
# tagging) in split_addresses.R. Just tags n_postcode and a single category.
categorise_ocod <- function(ocod) {
  lr <- ocod[!is.na(ocod$`Property Address`), ]
  lr$n_postcode <- stringi::stri_count_regex(lr$`Property Address`, postcode_rx)
  lr$category <- "overseas"
  lr$dataset <- "ocod"
  lr
}
