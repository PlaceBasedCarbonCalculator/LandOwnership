# Stage 2 (task 2): compare the 2026 CCOD/OCOD extract to the 2022 one so
# titles whose address text hasn't changed can skip geocoding entirely and
# carry forward their 2022 Bing result instead.
#
# Matching key: `Title Number`. It's the Land Registry's own stable
# identifier for a title, unlike the row-sequence `Id` the 2022 scripts
# used (see the audit note in prep_ccod.R) - that's what makes a 2022-to-
# 2026 diff possible at all.

# Tag each 2026 raw row (post prep_ccod.R/prep_ocod.R import, pre
# categorisation - so this must run before build_split_addresses()) with
# whether its Title Number is new, or its `Property Address` text is
# unchanged/changed versus the 2022 extract.
diff_titles <- function(new_raw, old_raw) {
  old <- old_raw[, c("Title Number", "Property Address")]
  names(old) <- c("Title Number", "old_property_address")
  old <- old[!duplicated(old$`Title Number`), ]

  out <- dplyr::left_join(new_raw, old, by = "Title Number")
  out$diff_status <- dplyr::case_when(
    is.na(out$old_property_address) ~ "new",
    out$`Property Address` == out$old_property_address ~ "unchanged",
    TRUE ~ "changed"
  )
  out$old_property_address <- NULL
  out
}

# Read the graded 2022 Bing results (data/bing_final/*.Rds). Only the
# usable grades (good/medium/low - i.e. everything except fail/nola/
# wrongla) are worth carrying forward; a result we already know is wrong is
# not a shortcut worth taking.
load_2022_final_results <- function(path = "data/bing_final") {
  fls <- list.files(path, pattern = "^bing_geocoded_(good|medium|low)\\.Rds$", full.names = TRUE)
  res <- lapply(fls, function(f) {
    x <- readRDS(f)
    coords <- sf::st_coordinates(x)
    x <- sf::st_drop_geometry(x)
    x$longitude <- coords[, "X"]
    x$latitude <- coords[, "Y"]
    x
  })
  res <- dplyr::bind_rows(res)
  res <- res[, c(
    "Title.Number", "Property.Address", "addressLine.x", "formattedAddress",
    "locality", "postalCode.y", "confidence", "entityType", "matchCodes",
    "longitude", "latitude"
  )]
  names(res) <- c(
    "Title Number", "old_property_address", "old_address_line",
    "formattedAddress", "locality", "postalCode", "confidence", "entityType", "matchCodes",
    "longitude", "latitude"
  )
  res$`old_address_line` <- stringr::str_squish(res$old_address_line)
  res
}

# For split 2026 addresses whose title is `unchanged`, attach the matching
# 2022 result by (Title Number, AddressLine). Titles/addresses that are
# `new` or `changed` are returned untouched (still needing geocoding), and
# any `unchanged` row that doesn't find a match (e.g. the 2022 run never
# geocoded it, or split it slightly differently) falls back to needing
# geocoding too rather than being silently dropped.
carry_forward_unchanged <- function(split_addresses, results_2022) {
  split_addresses$AddressLine <- stringr::str_squish(split_addresses$AddressLine)

  carried <- dplyr::inner_join(
    split_addresses[split_addresses$diff_status == "unchanged", ],
    results_2022,
    by = c("Title Number", "AddressLine" = "old_address_line", "Property Address" = "old_property_address")
  )
  carried$source <- "carried_forward_2022"

  matched_keys <- paste(carried$`Title Number`, carried$AddressLine)
  needs_geocode <- split_addresses[
    split_addresses$diff_status != "unchanged" |
      !paste(split_addresses$`Title Number`, split_addresses$AddressLine) %in% matched_keys,
  ]

  list(carried_forward = carried, needs_geocode = needs_geocode)
}
