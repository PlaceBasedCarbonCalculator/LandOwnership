# Land Registry Price Paid data and the UBDC transaction->UPRN linkage.
#
# Ported from the PlaceBasedCarbonCalculator/build repo (R/house_prices_ubdc.R
# and R/house_prices_address.R) as part of moving all UPRN / address handling
# into this repo. The build repo copies are left untouched for now; it will
# later be updated to read these targets from this repo's store instead of
# rebuilding them. Function and target names are kept identical to ease that
# migration. Code is copied as-is apart from:
#   - the `parameters$path_data` defaults are gone - every path is passed in
#     from _targets.R;
#   - the EPC .Rds paths point at the outputs of the sibling EPC repo
#     (../inputdata/epc/GB_*.Rds), which is now the canonical EPC source.

#' Load the UBDC price-paid-to-UPRN linkage table
#'
#' Unzips and reads the Urban Big Data Centre lookup that links Land Registry
#' price-paid transaction IDs to UPRNs and USRNs. Used by the
#' `house_prices_ubdc` target, which lets `land_registry_add_uprn()` geocode
#' transactions.
load_ubdc_house_prices = function(path){
  dir.create(file.path(tempdir(),"ubdc"))
  unzip(path, exdir = file.path(tempdir(),"ubdc"))

  dat = readr::read_csv(file.path(tempdir(),"ubdc","ppdid_uprn_usrn.csv"))

  dat
}


#' Load Land Registry price paid data
#'
#' Reads every Land Registry price-paid CSV in `path` (headerless annual
#' extracts), de-duplicates on transaction ID and converts the categorical
#' columns to factors. Used by the `house_price_lr` target.
load_lr_price_paid = function(path){
  fls = list.files(path)

  pp = list()

  for(i in 1:length(fls)){
     sub = readr::read_csv(file.path(path,fls[i]),
                              col_names = c("transactionid","price","date","postcode",
                                            "property_type","new_build","freehold",
                                            "address1","address2","address3","address4","town","la","county",
                                            "record_status","transaction_category"))
     sub = sub[!duplicated(sub$transactionid),]

      pp[[i]] = sub
  }

  pp = dplyr::bind_rows(pp)

  pp$property_type = as.factor(pp$property_type)
  pp$new_build = as.factor(pp$new_build)
  pp$freehold = as.factor(pp$freehold)
  pp$record_status = as.factor(pp$record_status)
  pp$transaction_category = as.factor(pp$transaction_category)


  pp
}

#' Geocode Land Registry transactions via UPRNs, EPC addresses and postcodes
#'
#' Attaches a UPRN and 2021 LSOA to every price-paid transaction through a
#' cascade of matches: (1) the UBDC transaction-UPRN lookup; (2) other
#' transactions at the same address that did match; (3-6) normalised
#' address+postcode matching against the domestic then non-domestic EPC
#' registers (two address formats each, newest EPC wins on duplicates); (7)
#' any remainder gets an LSOA from its postcode only. Matched transactions
#' get coordinates from `uprn_historical` and an LSOA by spatial join. Used
#' by the `house_price_lr_uprn` target.
land_registry_add_uprn = function(house_price_lr,
                                  house_prices_ubdc,
                                  uprn_historical,
                                  lookup_postcode_OA_LSOA_MSOA_2021,
                                  bounds_lsoa_GB_full,
                                  path_epc,
                                  path_epc_nondom){

  # Load Data
  house_price_lr = house_price_lr[!duplicated(house_price_lr$transactionid),]
  epc = readRDS(path_epc)
  epc = st_drop_geometry(epc)
  epc = epc[,c("UPRN","addr","ADDRESS2","ADDRESS3","POSTCODE","year")]
  epc_nondom = readRDS(path_epc_nondom)
  epc_nondom = st_drop_geometry(epc_nondom)
  epc_nondom = epc_nondom[,c("UPRN","adr1","adr2","adr3","postcode","year")]

  uprn_historical = st_as_sf(uprn_historical, coords = c("X_COORDINATE","Y_COORDINATE"), crs = 27700)
  lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[,c("pcds","lsoa21cd")]
  names(lookup_postcode_OA_LSOA_MSOA_2021) = c("pcds","LSOA21CD")

  # Join On ubdc data
  house_price_lr = dplyr::left_join(house_price_lr, house_prices_ubdc, by = "transactionid")

  # Split out with(out) UPRN
  lr_withuprn = house_price_lr[!is.na(house_price_lr$uprn),]
  lr_nouprn = house_price_lr[is.na(house_price_lr$uprn),]
  # Small number of transactionid in the UBDC data are missing in the LR data? About 29439

  # Check for matching addresses
  unique_address = lr_withuprn[,c("postcode","property_type",
                                  "address1","address2","address3","address4",
                                  "town","la","county","uprn","parentuprn","usrn")]

  unique_address = unique_address[!duplicated(unique_address[,c("postcode","property_type",
                                                                "address1","address2","address3",
                                                                "address4","town","la","county")]),]

  # Match Based on Address
  lr_nouprn$uprn = NULL
  lr_nouprn$usrn = NULL
  lr_nouprn$parentuprn = NULL

  lr_nouprn = dplyr::left_join(lr_nouprn,
                               unique_address,
                               by = c("postcode","property_type",
                                      "address1","address2","address3",
                                      "address4","town","la","county"))

  lr_withuprn = rbind(lr_withuprn, lr_nouprn[!is.na(lr_nouprn$uprn),])
  lr_nouprn = lr_nouprn[is.na(lr_nouprn$uprn),]
  lr_nouprn$uprn = NULL
  lr_nouprn$parentuprn = NULL
  lr_nouprn$usrn = NULL

  #nrow(lr_nouprn) + nrow(lr_withuprn) == nrow(house_price_lr) # TRUE
  rm(unique_address)
  # Now Try to match based on EPC Address
  #summary(epc$UPRN %in% lr_withuprn$uprn)
  # 9,865,292 UPRNs with EPC but no Land Registry Data (many may be Scotland)

  # Clean Addresses for Joining
  lr_nouprn$address1[is.na(lr_nouprn$address1)] = ""
  lr_nouprn$address2[is.na(lr_nouprn$address2)] = ""
  lr_nouprn$address3[is.na(lr_nouprn$address3)] = ""
  lr_nouprn$address4[is.na(lr_nouprn$address4)] = ""

  # Check addresses against EPCs
  lr_nouprn$join_address <- trimws(gsub("\\s+", " ",
                                                      paste(lr_nouprn$address1,
                                                            lr_nouprn$address2,
                                                            lr_nouprn$address3)))

  # Two slightly differnt approaches to addresses are being harmonised
  lr_nouprn$join_address2 <- trimws(gsub("\\s+", " ",
                                                       paste(lr_nouprn$address1,
                                                             lr_nouprn$address2,
                                                             lr_nouprn$address3,
                                                             lr_nouprn$address4)))

  epc$addr[is.na(epc$addr)] = ""
  epc$ADDRESS2[is.na(epc$ADDRESS2)] = ""
  epc$ADDRESS3[is.na(epc$ADDRESS3)] = ""

  epc$join_address <- toupper(trimws(gsub("\\s+", " ",
                                          paste(epc$addr,
                                                epc$ADDRESS2,
                                                epc$ADDRESS3))))
  epc$join_address <- gsub(",","",epc$join_address) # Some addresses have commas

  # Small number of duplicated addresses with different UPRNs
  # All very close to each other. Possible multiple UPRN for same address?
  # Take the newest one as definitive version
  epc = epc[,c("UPRN","join_address","POSTCODE","year")]
  names(epc) = c("uprn","join_address","POSTCODE","year")
  epc = epc[order(epc$year, decreasing = TRUE),]
  epc$year = NULL
  epc = epc[!duplicated(epc[,c("join_address","POSTCODE")]),]

  lr_nouprn = dplyr::left_join(lr_nouprn, epc, by = c("join_address" = "join_address", "postcode" = "POSTCODE"))

  lr_nouprn_good = lr_nouprn[!is.na(lr_nouprn$uprn),] # 1,000,739
  lr_nouprn_bad = lr_nouprn[is.na(lr_nouprn$uprn),] #1,938,708

  lr_withuprn = dplyr::bind_rows(lr_withuprn, lr_nouprn_good)
  rm(lr_nouprn_good, lr_nouprn)

  lr_nouprn_bad$uprn = NULL

  lr_nouprn_bad2 = dplyr::left_join(lr_nouprn_bad, epc, by = c("join_address2" = "join_address", "postcode" = "POSTCODE"))

  lr_nouprn_good2 = lr_nouprn_bad2[!is.na(lr_nouprn_bad2$uprn),] # 470,692
  lr_nouprn_bad2 = lr_nouprn_bad2[is.na(lr_nouprn_bad2$uprn),] # 1,468,016

  lr_withuprn = dplyr::bind_rows(lr_withuprn, lr_nouprn_good2)

  lr_nouprn_bad2$uprn = NULL
  rm(lr_nouprn_bad, lr_nouprn_good2)

  # Now Try Non-Dom EPCs
  epc_nondom$adr1[is.na(epc_nondom$adr1)] = ""
  epc_nondom$adr2[is.na(epc_nondom$adr2)] = ""
  epc_nondom$adr3[is.na(epc_nondom$adr3)] = ""

  epc_nondom$join_address <- toupper(trimws(gsub("\\s+", " ",
                                          paste(epc_nondom$adr1,
                                                epc_nondom$adr2,
                                                epc_nondom$adr3))))
  epc_nondom$join_address <- gsub(",","",epc_nondom$join_address)

  epc_nondom = epc_nondom[,c("UPRN","join_address","postcode","year")]
  names(epc_nondom) = c("uprn","join_address","postcode","year")
  epc_nondom = epc_nondom[order(epc_nondom$year, decreasing = TRUE),]
  epc_nondom$year = NULL
  epc_nondom = epc_nondom[!duplicated(epc_nondom[,c("join_address","postcode")]),]


  lr_nouprn_bad2 = dplyr::left_join(lr_nouprn_bad2, epc_nondom,
                               by = c("join_address" = "join_address", "postcode" = "postcode"))
  lr_nouprn_bad2_good = lr_nouprn_bad2[!is.na(lr_nouprn_bad2$uprn),]
  lr_nouprn_bad2_bad = lr_nouprn_bad2[is.na(lr_nouprn_bad2$uprn),]

  lr_withuprn = dplyr::bind_rows(lr_withuprn, lr_nouprn_bad2_good)
  rm(lr_nouprn_bad2_good, lr_nouprn_bad2)

  lr_nouprn_bad2_bad$uprn = NULL

  lr_nouprn_bad3 = dplyr::left_join(lr_nouprn_bad2_bad, epc_nondom,
                         by = c("join_address2" = "join_address", "postcode" = "postcode"))

  lr_nouprn_bad3_good = lr_nouprn_bad3[!is.na(lr_nouprn_bad3$uprn),]
  lr_nouprn_bad3_bad = lr_nouprn_bad3[is.na(lr_nouprn_bad3$uprn),]

  lr_withuprn = dplyr::bind_rows(lr_withuprn, lr_nouprn_bad3_good)
  rm(lr_nouprn_bad3_good, lr_nouprn_bad3)

  lr_nouprn_bad3_bad$uprn = NULL

  # Add LSOA from postcode
  #summary(lr_nouprn_bad3_bad$postcode %in% lookup_postcode_OA_LSOA_MSOA_2021$pcds)
  # Mode   FALSE    TRUE
  # logical   57800 1373229
  # Mostly NA postcodes

  lr_nouprn_bad3_bad = dplyr::left_join(lr_nouprn_bad3_bad,
                                    lookup_postcode_OA_LSOA_MSOA_2021,
                                    by = c("postcode" = "pcds"))

  lr_nouprn_bad3_bad$join_address = NULL
  lr_nouprn_bad3_bad$join_address2 = NULL

  lr_withuprn$LATITUDE = NULL
  lr_withuprn$LONGITUDE = NULL
  lr_withuprn$join_address = NULL
  lr_withuprn$join_address2 = NULL

  lr_withuprn = dplyr::left_join(lr_withuprn, uprn_historical, by = c("uprn" = "UPRN"))
  lr_withuprn = sf::st_as_sf(lr_withuprn)
  lr_withuprn = sf::st_join(lr_withuprn, bounds_lsoa_GB_full)
  lr_withuprn = sf::st_drop_geometry(lr_withuprn)

  lr_final = dplyr::bind_rows(lr_withuprn, lr_nouprn_bad3_bad)

  lr_final

}

#' Nowcast each property's value to 2025 prices
#'
#' Estimates a 2025 value for every property from its most recent sale:
#' median prices per local authority, year and property type give a growth
#' multiple to 2025 (type "O"/other uses the all-type LA median as its
#' transactions are too sparse), which is applied to the last sale price and
#' rounded to the nearest 1,000 pounds. Used by the `house_prices_nowcast`
#' target, which combine_uprn_epc_lr() needs for its per-UPRN price columns.
house_price_extrapolate = function(house_price_lr_uprn, lsoa_admin){

  lsoa_admin = lsoa_admin[,c("LSOA21CD","LAD25CD")]

  house_price_lr_uprn = dplyr::left_join(house_price_lr_uprn, lsoa_admin, by = "LSOA21CD")
  house_price_lr_uprn$year = lubridate::year(house_price_lr_uprn$date)

  house_price_la = house_price_lr_uprn |>
    dplyr::group_by(LAD25CD, year, property_type) |>
    dplyr::summarise(price_median = median(price),
                     transactions = dplyr::n())

  # Not enough transactions per year for the O type
  house_price_la_general = house_price_lr_uprn |>
    dplyr::group_by(LAD25CD, year) |>
    dplyr::summarise(price_median = median(price),
                     transactions = dplyr::n())

  house_price_la = house_price_la[!is.na(house_price_la$LAD25CD),]
  house_price_la = house_price_la[house_price_la$property_type != "O",]

  house_price_la_O = house_price_la_general
  house_price_la_O = house_price_la_O[!is.na(house_price_la_O$LAD25CD),]
  house_price_la_O$property_type = "O"
  house_price_la$property_type = as.character(house_price_la$property_type)

  house_price_la = rbind(house_price_la, house_price_la_O)
  house_price_la = dplyr::ungroup(house_price_la)

  # Get change to 2025

  # 1) Build a lookup of 2025 prices for each LAD25CD + property_type
  price_2025_lookup <- house_price_la |>
    dplyr::filter(year == 2025) |>
    dplyr::select(
      LAD25CD,
      property_type,
      price_median_2025 = price_median
    )

  # 2) Join back to the full table
  house_price_la_w_growth <- house_price_la |>
    dplyr::left_join(price_2025_lookup, by = c("LAD25CD", "property_type")) |>
    # 3) Compute growth multiple: "how many times the price has increased"
    dplyr::mutate(
      growth_multiple = dplyr::case_when(
        !is.na(price_median_2025) & price_median > 0 ~ price_median_2025 / price_median,
        TRUE ~ NA_real_
      )
    )



  uprn_latest = house_price_lr_uprn[order(house_price_lr_uprn$date, decreasing = TRUE),]
  uprn_latest = uprn_latest[!duplicated(uprn_latest$uprn),] # 15 million properties

  uprn_latest$year = lubridate::year(uprn_latest$date)

  uprn_latest = dplyr::left_join(uprn_latest,
                                 house_price_la_w_growth[,c("LAD25CD","year","property_type","growth_multiple")],
                                 by = c("LAD25CD","year","property_type")
                                 )

  uprn_latest$price_2025 = round(uprn_latest$price * uprn_latest$growth_multiple/1000,0) * 1000

  uprn_latest

}

#' Recover UPRNs for Price Paid transactions the early matching missed
#'
#' `land_registry_add_uprn()` runs early in the DAG and can only lean on the
#' UBDC transaction<->UPRN linkage (`house_prices_ubdc`, which only covers
#' sales to 2022) plus an exact-string match against the EPC registers - so
#' every transaction since 2022 that isn't an exact EPC address match is
#' left with no UPRN at all (dropped from `known_uprn_addresses` and
#' everything built on it). By the time this target runs, the richer
#' address infrastructure built later in the DAG - `epc_lookup` /
#' `price_paid_lookup` (looser postcode+house-number keys than the exact
#' string join), `building_lookup` (named, non-numeric PAONs), `street_lookup`
#' (known addresses keyed by street), `infill_lookup` (OSM/USRN-inferred
#' UPRNs) and `postcode_singleton_lookup` - all exist, so a second pass can
#' recover some of that residual. Reuses match_stage() from
#' match_free_sources.R, keyed off Price Paid's own structured PAON
#' (`address1`)/Street (`address3`) columns rather than parsing free text.
#' `postcode_district` supplies the CCOD/OCOD-style district naming that
#' `street_lookup`/`infill_lookup` street keys were built with, matching how
#' `build_street_lookup()` itself derives district from postcode. Used by
#' the `house_price_lr_rematch` target.
rematch_price_paid_unmatched = function(house_price_lr_uprn, postcode_district,
                                        epc_lookup, price_paid_lookup,
                                        building_lookup = NULL, street_lookup = NULL,
                                        infill_lookup = NULL, postcode_singleton_lookup = NULL) {
  orig_cols <- names(house_price_lr_uprn)
  rem <- house_price_lr_uprn[is.na(house_price_lr_uprn$uprn), ]
  n_start <- nrow(rem)
  # postcode_district's key is normalise_postcode()'d (build_postcode_district_lookup()),
  # unlike Price Paid's raw "AB1 2CD"-with-space postcode column - join on a
  # normalised copy so the district lookup actually hits.
  rem$postcode_norm <- normalise_postcode(rem$postcode)
  rem <- dplyr::left_join(rem, postcode_district, by = c("postcode_norm" = "postcode"))

  num_key <- function(df) normalise_match_key(df$address1, df$postcode)
  bld_key <- function(df) normalise_building_key(df$address1, df$postcode)
  str_key <- function(df) street_number_key(extract_house_number(df$address1), df$address3, df$district)

  matched <- list()
  run <- function(rem, keys, lookup, quality) {
    st <- match_stage(rem, keys, lookup, quality)
    matched[[length(matched) + 1]] <<- st$matched
    st$remaining
  }

  rem <- run(rem, num_key(rem), epc_lookup, "high")
  rem <- run(rem, num_key(rem), price_paid_lookup, "high")
  rem <- run(rem, bld_key(rem), building_lookup, "high")
  rem <- run(rem, str_key(rem), street_lookup, "medium")
  rem <- run(rem, num_key(rem), infill_lookup, NA_character_) # quality carried per-row
  rem <- run(rem, str_key(rem), infill_lookup, NA_character_)
  rem <- run(rem, normalise_postcode(rem$postcode), postcode_singleton_lookup, "medium")

  matched <- dplyr::bind_rows(matched)
  # LATITUDE/LONGITUDE are already columns of house_price_lr_uprn (all NA on
  # these never-matched rows) - match_stage() overwrites them in place, so
  # only UPRN/match_source/match_quality are genuinely new columns here.
  extra_cols <- c("UPRN", "match_source", "match_quality")
  for (col in extra_cols[!extra_cols %in% names(matched)]) {
    matched[[col]] <- rep(NA_character_, nrow(matched))
  }
  matched$uprn <- matched$UPRN # keep the original lower-case column name
  matched <- matched[, c(orig_cols, "match_source", "match_quality")]
  unmatched <- rem[, orig_cols]

  message(
    nrow(matched), " of ", n_start,
    " previously-unmatched Price Paid transactions gained a UPRN in the rematch pass (",
    nrow(unmatched), " still unmatched). Quality: ",
    paste(names(table(matched$match_quality, useNA = "ifany")),
      table(matched$match_quality, useNA = "ifany"),
      sep = "=", collapse = ", "
    )
  )

  list(matched = matched, unmatched = unmatched)
}

#' Fold rematch results back into the full Price Paid dataset
#'
#' `house_price_lr_rematch` only carries the *change* (newly recovered
#' matches + the residual still without a UPRN). This rebuilds the full
#' table - every original UBDC/EPC match plus whatever the rematch pass
#' added - so it can drive a second nowcast/known-address pass in
#' _targets.R (Stage 6d): `house_prices_nowcast_final`,
#' `uprn_historical_epc_lr_final`, `known_uprn_addresses_final`, which feed
#' the published `uprn_all_addresses` table. That second pass can't reuse
#' the original `known_uprn_addresses`/`street_lookup`/`infill_lookup`
#' targets - those are inputs to the rematch itself, so feeding the rematch
#' output back into them would be circular.
#'
#' The newly-recovered rows only have a UPRN + coordinates from the rematch
#' lookups - not the LSOA21CD that `land_registry_add_uprn()` attaches via a
#' spatial join against `uprn_historical`/`bounds_lsoa_GB_full`. That join
#' is repeated here for just those rows (same logic, same inputs) - without
#' it they'd have a UPRN but silently drop out of
#' `house_price_extrapolate()`'s local-authority price modelling for lack
#' of an LSOA/LAD. Coordinates are also refreshed from `uprn_historical`
#' (authoritative) rather than kept from whichever lookup stage matched -
#' same as the original UBDC/EPC-matched rows.
combine_price_paid_rematch = function(house_price_lr_uprn, house_price_lr_rematch,
                                      uprn_historical, bounds_lsoa_GB_full) {
  already_matched <- house_price_lr_uprn[!is.na(house_price_lr_uprn$uprn), ]

  newly_matched <- house_price_lr_rematch$matched
  if (nrow(newly_matched) > 0) {
    newly_matched$LSOA21CD <- NULL
    newly_matched$LATITUDE <- NULL
    newly_matched$LONGITUDE <- NULL
    uprn_sf <- sf::st_as_sf(uprn_historical, coords = c("X_COORDINATE", "Y_COORDINATE"), crs = 27700)
    newly_matched <- dplyr::left_join(newly_matched, uprn_sf, by = c("uprn" = "UPRN"))
    newly_matched <- sf::st_as_sf(newly_matched)
    newly_matched <- sf::st_join(newly_matched, bounds_lsoa_GB_full)
    newly_matched <- sf::st_drop_geometry(newly_matched)
    newly_matched$date_first <- NULL
    newly_matched$date_last <- NULL
  }

  out <- dplyr::bind_rows(already_matched, newly_matched, house_price_lr_rematch$unmatched)
  message(
    nrow(already_matched), " originally matched + ",
    nrow(newly_matched), " recovered by the rematch pass + ",
    nrow(house_price_lr_rematch$unmatched), " still unmatched = ",
    nrow(out), " total Price Paid transactions (", nrow(house_price_lr_uprn), " rows in the original table)."
  )
  out
}
