# Stage 3: clean + split the categorised CCOD/OCOD rows into one row per
# property. Replaces prep_address_long.R, prep_address_mulitple_postcode.R,
# prep_address_no_postcode.R, prep_land_postcode.R and
# prep_land_no_postcode.R with one consistent pair of treatments.
#
# Audit findings fixed vs the 2022 scripts (task 3):
#   - prep_land_postcode.R and prep_land_no_postcode.R each carried their
#     own hand-inlined, independently-evolved copy of the mines/compass/land
#     regex logic instead of calling R/text_cleaning.R's clean_mines() /
#     clean_compass() / clean_land() (which were clearly extracted from one
#     of them later but never back-ported to the other). They'd drifted:
#     e.g. prep_land_postcode.R had no `clean_flats()`/`clean_airspace()`
#     pass at all. All four "boilerplate" categories (land_pc, nopc_land,
#     leasehold, overseas) now go through the same
#     clean_mines/spelling/compass/land/flats/airspace/phrases pipeline.
#   - clean_airspace() was previously only called for CCOD leasehold titles;
#     it's applied uniformly now (airspace boilerplate turns up in freehold
#     land titles and overseas titles too).

source("R/address_functions.R")
source("R/text_cleaning.R")

# Explode a data frame so each row's `address_col` becomes one row per
# individual property (e.g. "10-14 Example Street" -> 3 rows). Rows that
# fail to parse (split_numbers_try() returns NULL) are dropped - they still
# exist in the pre-split categorised data if anyone wants to inspect them.
split_and_explode <- function(df, address_col = "AddressLine") {
  if (nrow(df) == 0) {
    df$AddressLine <- character(0)
    return(df)
  }
  addr_list <- purrr::map(df[[address_col]], split_numbers_try)
  reps <- lengths(addr_list)
  out <- df[rep(seq_len(nrow(df)), times = reps), ]
  out$AddressLine <- unlist(addr_list)
  out
}

# The standard boilerplate-stripping pipeline shared by every "hard"
# address category. `long_text_rem` (data/long_strings.xlsx) is an extra,
# very-long-boilerplate removal pass only needed for nopc_land titles.
clean_boilerplate_address <- function(x, text_rem, long_text_rem = NULL) {
  if (!is.null(long_text_rem)) {
    x <- remove_strings(x, long_text_rem$term)
  }
  x <- clean_mines(x)
  x <- clean_spelling(x)
  x <- clean_compass(x)
  x <- clean_land(x)
  x <- clean_flats(x)
  x <- clean_airspace(x)
  x <- clean_phrases(x, text_rem)
  x
}

# Freehold-no-postcode titles that are substations or "part of" descriptions
# aren't geocodable from the address text alone - set aside as `nopc_complex`
# rather than fed through split_numbers_try (mirrors prep_address_no_postcode.R).
split_nopc_complex <- function(df) {
  is_complex <- grepl("substation|sub-station|sub station|part of",
    df$`Property Address`,
    ignore.case = TRUE
  )
  list(simple = df[!is_complex, ], complex = df[is_complex, ])
}

finish_boilerplate <- function(df, text_rem, long_text_rem = NULL) {
  if (nrow(df) == 0) {
    df$AddressLine <- character(0)
    return(split_and_explode(df))
  }

  has_multi <- !is.na(df$n_postcode) & df$n_postcode >= 2
  df_single <- df[!has_multi, ]
  df_single$AddressLine <- df_single$`Property Address`
  df_single$PostalCode <- df_single$Postcode

  if (any(has_multi)) {
    df_multi <- split_multi_postcode(df[has_multi, ], "Property Address")
    df <- dplyr::bind_rows(df_single, df_multi)
  } else {
    df <- df_single
  }

  df$AddressLine <- clean_boilerplate_address(df$AddressLine, text_rem, long_text_rem)
  df$AddressLine <- stringi::stri_replace_all_regex(df$AddressLine, postcode_rx, "")
  df$AddressLine <- stringi::stri_replace_all_fixed(df$AddressLine, "()", "")

  split_and_explode(df)
}

# Top-level orchestrator: takes the bound-together categorised CCOD
# freehold + leasehold + OCOD rows (see prep_ccod.R / prep_ocod.R) and
# returns one row per property, ready for diffing/matching/geocoding.
build_split_addresses <- function(categorised, text_rem, long_text_rem = NULL) {
  # `orig_row_id` (added to the `all_categorised` target itself, see
  # _targets.R) is preserved through every rep()/bind_rows() below so
  # audit_cleaning.R can compare back to the pre-split data and see exactly
  # which source rows never made it into the final split output
  # (split_numbers_try() failures). Add it defensively if it's missing
  # (e.g. calling this function directly, outside the targets pipeline).
  if (!"orig_row_id" %in% names(categorised)) {
    categorised$orig_row_id <- seq_len(nrow(categorised))
  }

  nopc_rows <- categorised[categorised$category == "nopc", ]
  nopc_split <- split_nopc_complex(nopc_rows)
  nopc_complex <- nopc_split$complex
  nopc_simple <- nopc_split$simple
  nopc_simple$AddressLine <- nopc_simple$`Property Address`

  simple_categories <- c("simple_short", "simple_long", "multi_postcode")
  simple_df <- categorised[categorised$category %in% simple_categories, ]

  needs_multi_split <- simple_df$category == "multi_postcode"
  if (any(needs_multi_split)) {
    already_done <- simple_df[!needs_multi_split, ]
    to_split <- split_multi_postcode(simple_df[needs_multi_split, ], "Property Address")
    simple_df <- dplyr::bind_rows(already_done, to_split)
  }

  simple_df <- dplyr::bind_rows(simple_df, nopc_simple)
  simple_result <- split_and_explode(simple_df, "AddressLine")

  boilerplate_categories <- c("land_pc", "nopc_land", "leasehold", "overseas")
  boilerplate_df <- categorised[categorised$category %in% boilerplate_categories, ]
  long_boilerplate <- boilerplate_df[boilerplate_df$category == "nopc_land", ]
  other_boilerplate <- boilerplate_df[boilerplate_df$category != "nopc_land", ]

  boilerplate_result <- dplyr::bind_rows(
    finish_boilerplate(long_boilerplate, text_rem, long_text_rem),
    finish_boilerplate(other_boilerplate, text_rem)
  )

  result <- dplyr::bind_rows(simple_result, boilerplate_result)
  result$AddressLine <- stringr::str_squish(result$AddressLine)
  result <- result[!is.na(result$AddressLine) & nchar(result$AddressLine) > 0, ]
  result$parse_ok <- TRUE

  nopc_complex$AddressLine <- NA_character_
  nopc_complex$parse_ok <- FALSE

  dplyr::bind_rows(result, nopc_complex)
}
