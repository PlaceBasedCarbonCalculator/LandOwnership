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
# split_numbers_try() is a pure per-row function (its own try/catch, no
# shared state), and this runs it over up to ~2M rows - a textbook
# furrr::future_map() candidate. Relies on the caller (build_split_addresses())
# having already set a `future::plan()`; falls back to sequential (identical
# to the old purrr::map()) if none is set, so this is still safe to call on
# its own outside the pipeline.
split_and_explode <- function(df, address_col = "AddressLine") {
  if (nrow(df) == 0) {
    df$AddressLine <- character(0)
    return(df)
  }
  addr_list <- furrr::future_map(df[[address_col]], split_numbers_try)
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
  x <- clean_roof_basement(x)
  x <- clean_airspace(x)
  x <- clean_phrases(x, text_rem)
  x
}

# Freehold-no-postcode titles that are substations or "part of" descriptions
# aren't geocodable from a bare AddressLine the way nopc_simple titles are -
# set aside from split_numbers_try (mirrors prep_address_no_postcode.R).
# Substations get their own bucket: unlike "part of ..." titles (still
# hopeless without a coordinate - see `other_complex`), a substation can be
# resolved via OSM/UPRN matching (see pipeline/R/substations.R), so it's
# routed through the ordinary boilerplate-stripping pipeline instead of
# being dropped outright.
split_nopc_complex <- function(df) {
  is_complex <- grepl("substation|sub-station|sub station|part of",
    df$`Property Address`,
    ignore.case = TRUE
  )
  is_sub <- is_complex & is_substation_address(df$`Property Address`)
  list(
    simple = df[!is_complex, ],
    substation = df[is_sub, ],
    other_complex = df[is_complex & !is_sub, ]
  )
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
build_split_addresses <- function(categorised, text_rem, long_text_rem = NULL, workers = NULL) {
  # `orig_row_id` (added to the `all_categorised` target itself, see
  # _targets.R) is preserved through every rep()/bind_rows() below so
  # audit_cleaning.R can compare back to the pre-split data and see exactly
  # which source rows never made it into the final split output
  # (split_numbers_try() failures). Add it defensively if it's missing
  # (e.g. calling this function directly, outside the targets pipeline).
  if (!"orig_row_id" %in% names(categorised)) {
    categorised$orig_row_id <- seq_len(nrow(categorised))
  }

  # split_and_explode() (per-row split_numbers_try()) and split_multi_postcode()
  # (per-row postcode-boundary cut) below both run over up to ~2M rows one R
  # function call at a time - the dominant cost of this stage. Both are pure,
  # independent per-row work, so a multisession plan set once here (same
  # convention as load_inspire_clean() in inspire_uprn_lookup.R) parallelises
  # every call below without paying repeated cluster start-up cost.
  if (is.null(workers)) {
    workers <- min(8, max(1, future::availableCores() - 1))
  }
  future::plan("multisession", workers = workers)
  on.exit(future::plan("sequential"), add = TRUE)

  nopc_rows <- categorised[categorised$category == "nopc", ]
  nopc_split <- split_nopc_complex(nopc_rows)
  nopc_complex <- nopc_split$other_complex
  nopc_complex$category <- "nopc_complex" # was silently left as "nopc" - audit_cleaning.R counts this category explicitly
  nopc_simple <- nopc_split$simple
  nopc_substation <- nopc_split$substation
  nopc_substation$category <- "nopc_substation"
  # nopc titles have no postcode, so the address text is all the geocoder
  # gets - give them the light-touch spelling pass (the heavier boilerplate
  # lists stay boilerplate-category-only); leading glue like "the site of"
  # is trimmed by final_address_tidy() below.
  nopc_simple$AddressLine <- clean_spelling(nopc_simple$`Property Address`)

  simple_categories <- c("simple_short", "simple_long", "multi_postcode")
  simple_df <- categorised[categorised$category %in% simple_categories, ]

  needs_multi_split <- simple_df$category == "multi_postcode"
  if (any(needs_multi_split)) {
    already_done <- simple_df[!needs_multi_split, ]
    to_split <- split_multi_postcode(simple_df[needs_multi_split, ], "Property Address")
    simple_df <- dplyr::bind_rows(already_done, to_split)
  }

  simple_df <- dplyr::bind_rows(simple_df, nopc_simple)

  # simple_short / simple_long rows had their postcode stripped out of the
  # text at categorisation but PostalCode was never populated (audit F1) -
  # without it they can neither free-match nor be sent to Azure with a
  # postcode. Carry the registry Postcode across wherever it's missing.
  if (!"PostalCode" %in% names(simple_df)) {
    simple_df$PostalCode <- NA_character_
  }
  fill_pc <- is.na(simple_df$PostalCode) & !is.na(simple_df$Postcode)
  simple_df$PostalCode[fill_pc] <- simple_df$Postcode[fill_pc]

  simple_result <- split_and_explode(simple_df, "AddressLine")

  boilerplate_categories <- c("land_pc", "nopc_land", "leasehold", "overseas")
  boilerplate_df <- categorised[categorised$category %in% boilerplate_categories, ]
  # nopc_substation shares nopc_land's shape (freehold, no postcode) and its
  # boilerplate is often the same easement/equipment legalese (see
  # data/long_strings.xlsx), so it gets the same long_text_rem treatment
  # rather than a separate finish_boilerplate() call.
  long_boilerplate <- dplyr::bind_rows(
    boilerplate_df[boilerplate_df$category == "nopc_land", ],
    nopc_substation
  )
  other_boilerplate <- boilerplate_df[boilerplate_df$category != "nopc_land", ]

  boilerplate_result <- dplyr::bind_rows(
    finish_boilerplate(long_boilerplate, text_rem, long_text_rem),
    finish_boilerplate(other_boilerplate, text_rem)
  )

  result <- dplyr::bind_rows(simple_result, boilerplate_result)
  # Keep the tagged intermediate (it encodes what kind of title the text
  # described - @MNS, @ASP, ... - useful for grading results later), then
  # guarantee the geocoder-facing AddressLine is free of tags, empty
  # brackets, leading legal glue and dangling punctuation (audit F8/F9).
  result$AddressLine_tagged <- result$AddressLine
  result$AddressLine <- final_address_tidy(result$AddressLine)
  result <- result[!is.na(result$AddressLine) & nchar(result$AddressLine) > 0, ]
  result$parse_ok <- TRUE

  # rep() so this also works when nopc_complex has zero rows (scalar
  # assignment into a 0-row base data.frame is an error)
  nopc_complex$AddressLine <- rep(NA_character_, nrow(nopc_complex))
  nopc_complex$parse_ok <- rep(FALSE, nrow(nopc_complex))

  dplyr::bind_rows(result, nopc_complex)
}
