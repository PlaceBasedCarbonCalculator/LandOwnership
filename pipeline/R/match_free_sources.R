# Stage 6 (tasks 5/6): before ever sending an address to the paid Azure
# geocoder, try to resolve it for free. Matching runs in stages, cheapest
# and most trustworthy first; every match is tagged with match_source and
# match_quality so downstream users can filter by how much they trust it:
#
#   1. (postcode, house number) vs EPC            - quality "high"
#   2. (postcode, house number) vs Price Paid     - quality "high"
#   3. (postcode, building name) vs EPC/PP        - quality "high"
#   4. (district, street, number) vs known addrs  - quality "medium"
#      (for rows with no postcode - street name + district instead)
#   5. (postcode|street keys) vs infilled UPRNs   - quality "medium" for
#      OSM-tagged addresses, "guess" for gap-guessed house numbers
#   6. postcode-singleton                         - quality "medium"
#      (per NSUL the postcode contains exactly one UPRN, so a row carrying
#      that postcode can only be that property)
#   7. (district, street) -> USRN street centroid - quality "street"
#      (no UPRN - a point on the named street, for numberless land titles)
#
# Known limitation (documented in the sibling repo's own code comments):
# house_price_lr_uprn's address-matching cascade leaves a large residual
# with no UPRN, and ~0.07% of domestic UPRNs have a coordinate outside
# their claimed postcode area. All sources here are "probably right, not
# guaranteed" - hence the per-row source/quality tags.

lookup_dedupe <- function(lookup) {
  lookup <- lookup[!is.na(lookup$key), ]
  ambiguous <- lookup$key[duplicated(lookup$key)]
  lookup[!lookup$key %in% ambiguous, ]
}

build_epc_lookup <- function(uprn_historical_epc_lr) {
  dom <- uprn_historical_epc_lr$domestic
  nondom <- uprn_historical_epc_lr$nondomestic

  lookup <- dplyr::bind_rows(
    data.frame(
      key = normalise_match_key(dom$addr, dom$POSTCODE),
      UPRN = dom$UPRN, LATITUDE = dom$LATITUDE, LONGITUDE = dom$LONGITUDE,
      match_source = "epc_domestic"
    ),
    data.frame(
      key = normalise_match_key(nondom$adr1, nondom$postcode),
      UPRN = nondom$UPRN, LATITUDE = nondom$LATITUDE, LONGITUDE = nondom$LONGITUDE,
      match_source = "epc_nondomestic"
    )
  )
  lookup_dedupe(lookup)
}

build_price_paid_lookup <- function(house_price_lr_uprn) {
  hp <- house_price_lr_uprn[!is.na(house_price_lr_uprn$uprn), ]
  lookup <- data.frame(
    key = normalise_match_key(hp$address1, hp$postcode),
    UPRN = hp$uprn, LATITUDE = hp$LATITUDE, LONGITUDE = hp$LONGITUDE,
    match_source = "price_paid"
  )
  lookup_dedupe(lookup)
}

# (postcode, building name) for addresses that start with a name rather
# than a number ("Ivy Cottage, Ackton Lane") - previously these could never
# free-match at all.
build_building_lookup <- function(uprn_historical_epc_lr, house_price_lr_uprn) {
  dom <- uprn_historical_epc_lr$domestic
  nondom <- uprn_historical_epc_lr$nondomestic
  hp <- house_price_lr_uprn[!is.na(house_price_lr_uprn$uprn), ]

  lookup <- dplyr::bind_rows(
    data.frame(
      key = normalise_building_key(dom$addr, dom$POSTCODE),
      UPRN = dom$UPRN, LATITUDE = dom$LATITUDE, LONGITUDE = dom$LONGITUDE,
      match_source = "epc_domestic_building"
    ),
    data.frame(
      key = normalise_building_key(nondom$adr1, nondom$postcode),
      UPRN = nondom$UPRN, LATITUDE = nondom$LATITUDE, LONGITUDE = nondom$LONGITUDE,
      match_source = "epc_nondomestic_building"
    ),
    data.frame(
      key = normalise_building_key(hp$address1, hp$postcode),
      UPRN = hp$uprn, LATITUDE = hp$LATITUDE, LONGITUDE = hp$LONGITUDE,
      match_source = "price_paid_building"
    )
  )
  lookup_dedupe(lookup)
}

# (district, street, house number) over the known EPC/PP addresses - the
# workhorse for the ~two-thirds of the queue that has no postcode. District
# comes from the LR-derived postcode->district lookup so both sides of the
# join use the Land Registry's own district naming.
build_street_lookup <- function(known_uprn_addresses, postcode_district) {
  known <- known_uprn_addresses[
    !is.na(known_uprn_addresses$house_number) & !is.na(known_uprn_addresses$street),
  ]
  known <- dplyr::left_join(known, postcode_district, by = "postcode")
  lookup <- data.frame(
    key = street_number_key(known$house_number, known$street, known$district),
    UPRN = known$UPRN, LATITUDE = known$LATITUDE, LONGITUDE = known$LONGITUDE,
    match_source = paste0(known$address_source, "_street")
  )
  lookup_dedupe(lookup)
}

# Keys over the infilled (OSM / USRN-inferred / gap-guessed) UPRN addresses.
# Both key styles are emitted; quality reflects the provenance flags set by
# build_uprn_infill().
build_infill_lookup <- function(uprn_infill) {
  inf <- uprn_infill
  quality <- ifelse(inf$number_guessed, "guess",
    ifelse(inf$address_source == "osm_building", "medium", "low")
  )
  base <- data.frame(
    UPRN = inf$UPRN, LATITUDE = inf$LATITUDE, LONGITUDE = inf$LONGITUDE,
    match_source = paste0("infill_", inf$address_source,
      ifelse(inf$number_guessed, "_gap_guess", "")
    ),
    match_quality = quality,
    stringsAsFactors = FALSE
  )

  pc_keys <- base
  pc_keys$key <- {
    k <- paste(inf$postcode, toupper(inf$house_number), sep = "|")
    k[is.na(inf$postcode) | is.na(inf$house_number)] <- NA_character_
    k
  }
  street_keys <- base
  street_keys$key <- street_number_key(inf$house_number, inf$street, inf$district)

  lookup <- dplyr::bind_rows(pc_keys, street_keys)
  lookup_dedupe(lookup)
}

# Postcodes that genuinely contain exactly one UPRN, per the National
# Statistics UPRN Lookup - NOT merely one EPC record. Any Land Registry row
# carrying such a postcode can only be that UPRN, so this is solid
# ("medium") evidence; before NSUL this stage used EPC/PP coverage alone
# and had to be graded "low". Coordinates come from the OS Open UPRN
# release (uprn_historical).
build_postcode_singleton_lookup <- function(nsul, uprn_historical) {
  dt <- data.table::as.data.table(nsul[, c("UPRN", "postcode")])
  dt <- dt[!is.na(postcode)]
  singles <- dt[, .(n = .N, UPRN = UPRN[1]), by = postcode]
  singles <- singles[n == 1]
  uh <- data.table::as.data.table(uprn_historical[, c("UPRN", "LATITUDE", "LONGITUDE")])
  singles <- merge(singles, uh, by = "UPRN")
  data.frame(
    key = singles$postcode,
    UPRN = singles$UPRN, LATITUDE = singles$LATITUDE, LONGITUDE = singles$LONGITUDE,
    match_source = "postcode_singleton_nsul",
    stringsAsFactors = FALSE
  )
}

# One matching pass: join `keys` (aligned with rows of `remaining`) against
# `lookup`; rows that hit move to matched with source/quality tags.
match_stage <- function(remaining, keys, lookup, quality_default = NA_character_) {
  if (is.null(lookup) || nrow(lookup) == 0 || nrow(remaining) == 0) {
    return(list(matched = remaining[0, ], remaining = remaining))
  }
  idx <- match(keys, lookup$key)
  hit <- !is.na(idx) & !is.na(keys)
  matched <- remaining[hit, ]
  if (nrow(matched) > 0) {
    li <- idx[hit]
    matched$UPRN <- lookup$UPRN[li]
    matched$LATITUDE <- lookup$LATITUDE[li]
    matched$LONGITUDE <- lookup$LONGITUDE[li]
    matched$match_source <- lookup$match_source[li]
    matched$match_quality <- if ("match_quality" %in% names(lookup)) {
      lookup$match_quality[li]
    } else {
      quality_default
    }
  }
  list(matched = matched, remaining = remaining[!hit, ])
}

# `needs_geocode` is the output of carry_forward_unchanged()$needs_geocode
# (split addresses still needing a location). Returns `matched` (tagged
# with UPRN/coords/match_source/match_quality) and `unmatched` (same
# columns as the input, ready for the geocode queue). Lookups beyond the
# first two are optional so the function still works standalone.
match_free_sources <- function(needs_geocode, epc_lookup, price_paid_lookup,
                               building_lookup = NULL,
                               street_lookup = NULL,
                               infill_lookup = NULL,
                               postcode_singleton_lookup = NULL,
                               street_centroid_lookup = NULL) {
  orig_cols <- names(needs_geocode)
  rem <- needs_geocode

  num_key <- function(df) normalise_match_key(df$AddressLine, df$PostalCode)
  bld_key <- function(df) normalise_building_key(df$AddressLine, df$PostalCode)
  str_key <- function(df) {
    street_number_key(
      extract_house_number(df$AddressLine),
      extract_street_name(df$AddressLine),
      df$District
    )
  }

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
  # NSUL-based: the postcode truly contains one UPRN, so "medium" not "low"
  rem <- run(rem, normalise_postcode(rem$PostalCode), postcode_singleton_lookup, "medium")

  # street-centroid fallback: only rows with NO house number (a numbered
  # address deserves a real geocode, not a street midpoint)
  if (!is.null(street_centroid_lookup) && nrow(street_centroid_lookup) > 0 && nrow(rem) > 0) {
    seg <- normalise_name(stringr::str_extract(rem$AddressLine, "^[^,]+"))
    ckey <- paste(normalise_name(rem$District), seg, sep = "|")
    ckey[is.na(rem$District) | is.na(seg)] <- NA_character_
    ckey[!is.na(extract_house_number(rem$AddressLine))] <- NA_character_
    cl <- street_centroid_lookup
    cl$UPRN <- NA_real_
    cl$match_source <- "usrn_street_centroid"
    st <- match_stage(rem, ckey, cl, "street")
    matched[[length(matched) + 1]] <- st$matched
    rem <- st$remaining
  }

  matched <- dplyr::bind_rows(matched)
  if (nrow(matched) > 0) {
    matched$source <- paste0(matched$match_source, "_match")
    matched$match_source <- NULL
  }
  unmatched <- rem[, orig_cols]

  message(
    nrow(matched), " of ", nrow(needs_geocode),
    " addresses matched for free (", nrow(unmatched), " left for the paid queue). ",
    "Quality: ", paste(names(table(matched$match_quality, useNA = "ifany")),
      table(matched$match_quality, useNA = "ifany"),
      sep = "=", collapse = ", "
    )
  )
  list(matched = matched, unmatched = unmatched)
}
