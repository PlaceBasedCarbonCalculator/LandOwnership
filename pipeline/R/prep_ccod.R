# Stage 1: raw import + categorisation of the UK company ownership (CCOD)
# data. Adapts R/import_UK_owners.R and R/prep_UK_leashold_owners.R.
#
# Audit findings fixed vs the 2022 scripts (task 3):
#   - Columns are now selected by NAME, not position (the 2022 script used
#     `freehold[,c(1,3:7,36)]`; a positional index silently breaks if the
#     Land Registry adds/reorders CSV columns between releases, which is
#     exactly the kind of thing that can happen across a 2022->2026 gap).
#   - `Id <- seq_len(nrow(lr))` is dropped. It was never a stable key -
#     re-running the import, or importing the 2022 vs 2026 extract, gives
#     completely different row orders, so the old Id can't be used to
#     compare or carry forward results across runs. `Title Number` (unique
#     per freehold/leasehold title) is the real key.
#   - Ownership/proprietor fields are kept from the start (previously only
#     pulled back in at the very end, in map_geocoded_data.R, by a second
#     unzip+read of the raw file) so the pipeline only reads each zip once.

ccod_columns <- c(
  "Title Number", "Tenure", "Property Address", "District", "County",
  "Region", "Postcode", "Proprietor Name (1)", "Company Registration No. (1)",
  "Proprietorship Category (1)"
)

# Read one CCOD_FULL_*.zip and return the raw rows with a stable key
# (Title Number) and a stripped-down, by-name column selection.
import_ccod_raw <- function(zip_path) {
  tmp <- tempfile("ccod")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  utils::unzip(zip_path, exdir = tmp)
  csv <- list.files(tmp, pattern = "\\.csv$", full.names = TRUE)[1]
  lr <- readr::read_csv(csv, lazy = FALSE, show_col_types = FALSE)

  missing_cols <- setdiff(ccod_columns, names(lr))
  if (length(missing_cols) > 0) {
    stop(
      "CCOD schema has changed - missing expected column(s): ",
      paste(missing_cols, collapse = ", "),
      ". Update ccod_columns in pipeline/R/prep_ccod.R."
    )
  }

  lr <- lr[, ccod_columns]
  lr <- lr[!is.na(lr$`Title Number`), ]
  lr[!duplicated(lr$`Title Number`), ] # CCOD titles are one row each; guard anyway
}

# Categorise freehold titles by how hard they'll be to geocode, mirroring
# R/import_UK_owners.R's category scheme, but as one long data frame
# (`category` column) instead of six separately-saved .Rds files with
# duplicated splitting code - easier to audit and to feed into a single
# downstream cleaning stage.
categorise_ccod_freehold <- function(ccod) {
  lr <- ccod[ccod$Tenure == "Freehold", ]
  lr <- lr[!is.na(lr$`Property Address`), ] # drops the handful with no address at all

  lr$n_postcode <- stringi::stri_count_regex(lr$`Property Address`, postcode_rx)
  lr$land <- grepl("\\bland\\b", lr$`Property Address`, ignore.case = TRUE)

  lr_pc <- lr[!is.na(lr$Postcode), ]
  lr_nopc <- lr[is.na(lr$Postcode), ]

  lr_pc_land <- lr_pc[lr_pc$land, ]
  lr_pc <- lr_pc[!lr_pc$land, ]
  lr_nopc_land <- lr_nopc[lr_nopc$land, ]
  lr_nopc <- lr_nopc[!lr_nopc$land, ]

  lr_pc_multi <- lr_pc[lr_pc$n_postcode >= 2, ]
  lr_pc_single <- lr_pc[lr_pc$n_postcode <= 1, ]

  # Single-postcode: split short vs long the same way as the original
  # (short addresses are the common case and split cleanly; long ones need
  # split_numbers_try applied before we know if they're "simple")
  lr_pc_single$AddressLine <- purrr::map2_chr(
    lr_pc_single$`Property Address`, lr_pc_single$Postcode,
    function(x, y) {
      x <- gsub(y, "", x, fixed = TRUE)
      gsub("()", "", x, fixed = TRUE)
    }
  )
  n_char <- nchar(lr_pc_single$AddressLine)
  lr_pc_single_long <- lr_pc_single[n_char > 54, ]
  lr_pc_single_short <- lr_pc_single[n_char <= 54, ]

  categorised <- dplyr::bind_rows(
    dplyr::mutate(lr_pc_single_short, category = "simple_short"),
    dplyr::mutate(lr_pc_single_long, category = "simple_long"),
    dplyr::mutate(lr_pc_multi, category = "multi_postcode"),
    dplyr::mutate(lr_pc_land, category = "land_pc"),
    dplyr::mutate(lr_nopc, category = "nopc"),
    dplyr::mutate(lr_nopc_land, category = "nopc_land")
  )
  categorised$dataset <- "ccod_freehold"
  categorised
}

# Leasehold titles aren't split by category in the original pipeline (they
# just get the multi-postcode split + standard cleaning) - kept the same
# here, tagged with a single "leasehold" category.
categorise_ccod_leasehold <- function(ccod) {
  lr <- ccod[ccod$Tenure == "Leasehold", ]
  lr <- lr[!is.na(lr$`Property Address`), ]
  lr$n_postcode <- stringi::stri_count_regex(lr$`Property Address`, postcode_rx)
  lr$category <- "leasehold"
  lr$dataset <- "ccod_leasehold"
  lr
}
