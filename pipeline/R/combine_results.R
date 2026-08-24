# Stage 11: merge every source of location information into one final,
# tidy table - this is the 2026 equivalent of
# R/process_geocodeing_results.R + R/map_geocoded_data.R, but built once
# instead of two separate hand-run passes, and tagging where each row's
# location actually came from.

# Blank strings and NA mean the same thing here (readr leaves empty CSV cells
# as "" in some sources and NA in others); collapse them to NA so downstream
# tests only have to check one of them.
blank_to_na <- function(x) {
  x <- trimws(as.character(x))
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

# Country of registration for the map. OCOD carries an explicit
# "Country Incorporated (1)"; CCOD has none because every proprietor on it is
# UK-registered, so prep_ccod.R fills it with "UK" at import. This is the
# backstop for any row that reaches here without the column anyway (a cached
# result from an older run, say) - without it, CCOD rows land on the map with
# an empty country and "UK" never shows up as the most common value.
resolve_country <- function(df) {
  country <- if ("Country Incorporated (1)" %in% names(df)) {
    blank_to_na(df$`Country Incorporated (1)`)
  } else {
    rep(NA_character_, nrow(df))
  }
  from_uk_dataset <- !is.na(df$dataset) & startsWith(as.character(df$dataset), "ccod")
  country[is.na(country) & from_uk_dataset] <- "UK"
  country
}

# Tenure is the Land Registry's own Freehold/Leasehold flag and is present on
# both CCOD and OCOD rows, so carry it through rather than re-deriving it
# downstream from `dataset` - that turned every OCOD title into "Overseas
# owned", which is not a tenure and hid whether the title was freehold or
# leasehold. Overseas ownership stays visible via `dataset` and the country.
resolve_tenure <- function(df) {
  if ("Tenure" %in% names(df)) blank_to_na(df$Tenure) else rep(NA_character_, nrow(df))
}

# Common output schema every source gets normalised into before binding.
normalise_result_columns <- function(df) {
  data.frame(
    title_number = df$`Title Number`,
    dataset = df$dataset,
    category = df$category,
    tenure = resolve_tenure(df),
    property_address = df$`Property Address`,
    address_line = df$AddressLine,
    district = df$District,
    postcode = df$PostalCode,
    proprietor_name = df$`Proprietor Name (1)`,
    company_registration_no = df$`Company Registration No. (1)`,
    proprietorship_category = df$`Proprietorship Category (1)`,
    country_incorporated = resolve_country(df),
    uprn = as.character(df$uprn),
    latitude = as.numeric(df$latitude),
    longitude = as.numeric(df$longitude),
    match_quality = as.character(df$match_quality),
    source = df$source,
    stringsAsFactors = FALSE
  )
}

combine_final <- function(carried_forward, free_matched, azure_results, queue,
                           uprn_historical, uprn_inspire_lookup = NULL) {
  cf <- carried_forward
  cf$uprn <- NA_character_
  cf$match_quality <- cf$confidence

  fm <- free_matched
  fm$uprn <- as.character(fm$UPRN)
  fm$latitude <- fm$LATITUDE
  fm$longitude <- fm$LONGITUDE
  if (!"match_quality" %in% names(fm)) {
    fm$match_quality <- NA_character_ # older free_match outputs had no quality tag
  }

  geocoded <- dplyr::inner_join(queue, azure_results, by = "queue_key")
  geocoded <- snap_geocoded_to_uprn(geocoded, uprn_historical) # handles nrow(geocoded) == 0 internally
  geocoded$uprn <- as.character(geocoded$UPRN)
  geocoded$match_quality <- geocoded$confidence

  combined <- dplyr::bind_rows(
    normalise_result_columns(cf),
    normalise_result_columns(fm),
    normalise_result_columns(geocoded)
  )

  if (!is.null(uprn_inspire_lookup) && nrow(uprn_inspire_lookup) > 0) {
    inspire_join <- uprn_inspire_lookup[, c("UPRN", "INSPIREID", "n_uprn", "single_uprn_parcel")]
    inspire_join$UPRN <- as.character(inspire_join$UPRN)
    combined <- dplyr::left_join(combined, inspire_join, by = c("uprn" = "UPRN"))
  } else {
    combined$INSPIREID <- NA_character_
    combined$n_uprn <- NA_integer_
    combined$single_uprn_parcel <- NA
  }

  combined
}
