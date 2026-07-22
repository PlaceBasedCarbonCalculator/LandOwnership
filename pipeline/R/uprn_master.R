# The master UPRN address table: one row for EVERY known UPRN (everything
# that appeared in any 2020-2025 OS Open UPRN release) with every piece of
# address information this repo knows about it, side by side and clearly
# attributed:
#
#   - location + lifecycle from the OS releases (uprn_historical)
#   - domestic / non-domestic / unknown classification (uprn_historical_epc_lr)
#   - authoritative postcode + district from NSUL (uprn_places; E&W only)
#   - address lines from the domestic & non-domestic EPC registers
#   - address line from Display Energy Certificates (dec_addresses)
#   - address + latest sale details from the geocoded Land Registry Price
#     Paid data (house_price_lr_uprn)
#   - the "best" single address line and its parsed house number / street
#     (known_uprn_addresses, includes the 2022-geocode augmentation)
#   - inferred street / house number for addressless UPRNs, with provenance
#     flags (uprn_infill - OSM buildings, USRN street names, gap guesses)
#   - the UPRN's USRN and that street's inferred name (uprn_usrn +
#     usrn_street_names)
#   - the INSPIRE parcel the UPRN falls in (uprn_inspire_lookup)
#
# This is a deliverable of the repo in its own right (the `uprn_all_addresses`
# target, written to output/uprn_all_addresses.Rds), intended as the single
# place downstream projects (e.g. the build repo) look up "what do we know
# about this UPRN's address".

# The EPC register's own energy-efficiency band column (A-G), found
# defensively rather than hard-coded: `df` is the raw domestic/non-domestic
# --- EPC rating extraction --------------------------------------------------
#
# The domestic and non-domestic registers do NOT carry comparable ratings,
# so they are read by explicitly-named column and kept in SEPARATE output
# columns - never coalesced into one "epc_rating":
#
#   domestic     `cur_rate`  an A-G efficiency BAND (ordered factor, with an
#                            "INVALID!" level for unusable certificates).
#   non-domestic `rating`    a NUMERIC asset rating (e.g. 49, 56, 45) on a
#                            different scale entirely - not an A-G band, and
#                            not convertible to one without asserting band
#                            thresholds this repo has no authority to set.
#
# Both names come from the sibling EPC repo's publication-time rename in
# EPC/R/merge_epcs.R ("Clean up for publication: rename by name"), which is
# that repo's deliberate output contract - NOT the raw register's
# CURRENT_ENERGY_RATING/ASSET_RATING. This was previously read by a
# pick_epc_rating_col() helper that guessed from a list of candidate names;
# the list omitted `cur_rate`, so every domestic rating silently came back
# NA while the non-domestic numeric asset rating matched the bare "rating"
# candidate - i.e. the published column ended up holding only the values it
# was least meant to. Reading one named column per register instead means a
# future rename upstream fails loudly here rather than producing a
# plausible-looking but empty column. (The rest of this repo already reads
# the renamed schema - see build_known_uprn_addresses() in uprn_infill.R,
# which uses `addr`, the renamed ADDRESS1.)

# Domestic A-G band. "INVALID!" is a real level in the source factor for
# certificates whose rating couldn't be computed - mapped to NA so it never
# reaches a legend or a colour ramp as if it were a band.
epc_domestic_rating <- function(df, col = "cur_rate") {
  if (!col %in% names(df)) {
    stop(
      "epc_domestic_rating(): column '", col, "' not found in the domestic EPC ",
      "data. The sibling EPC repo (EPC/R/merge_epcs.R) may have renamed it - ",
      "check the published GB_domestic_epc.Rds schema and update this function."
    )
  }
  out <- toupper(trimws(as.character(df[[col]])))
  out[out %in% c("", "INVALID!", "INVALID", "NA")] <- NA_character_
  out
}

# Non-domestic numeric asset rating. Kept numeric (not banded, not coerced
# to a letter) - see the block comment above.
epc_nondomestic_asset_rating <- function(df, col = "rating") {
  if (!col %in% names(df)) {
    stop(
      "epc_nondomestic_asset_rating(): column '", col, "' not found in the ",
      "non-domestic EPC data. The sibling EPC repo (EPC/R/merge_epcs.R) may ",
      "have renamed it - check the published GB_nondomestic_epc.Rds schema ",
      "and update this function."
    )
  }
  suppressWarnings(as.numeric(df[[col]]))
}

build_uprn_all_addresses <- function(uprn_historical, uprn_epc_lr,
                                     dec_addresses, house_price_lr_uprn,
                                     known_uprn_addresses, uprn_infill,
                                     uprn_usrn, usrn_street_names,
                                     uprn_places, uprn_inspire_lookup,
                                     house_prices_nowcast_final = NULL) {
  dt <- data.table::as.data.table(
    uprn_historical[, c(
      "UPRN", "date_first", "date_last",
      "X_COORDINATE", "Y_COORDINATE", "LATITUDE", "LONGITUDE"
    )]
  )

  # --- classification + lifecycle flags (identical on the duplicated
  # "ambiguous" rows that appear in both the domestic and non-domestic
  # frames, so a plain dedupe is safe)
  cls <- data.table::rbindlist(list(
    data.table::as.data.table(uprn_epc_lr$domestic)[, .(UPRN, class = domestic, exists, newbuild)],
    data.table::as.data.table(uprn_epc_lr$nondomestic)[, .(UPRN, class = domestic, exists, newbuild)],
    data.table::as.data.table(uprn_epc_lr$unknown)[, .(UPRN, class = domestic, exists, newbuild)]
  ))
  cls <- unique(cls, by = "UPRN")
  dt <- merge(dt, cls, by = "UPRN", all.x = TRUE)

  # --- NSUL postcode / district (authoritative, E&W only)
  pl <- data.table::as.data.table(uprn_places)[, .(
    UPRN,
    postcode_nsul = postcode, district_nsul = district
  )]
  pl <- unique(pl, by = "UPRN")
  dt <- merge(dt, pl, by = "UPRN", all.x = TRUE)

  # --- EPC domestic address (rows in the domestic frame that actually have
  # an EPC record; class-only rows carry NA addr). epc_dom_rating: the
  # register's A-G energy-efficiency band, read from the published
  # `cur_rate` column - see epc_domestic_rating() above for why this is an
  # explicit named read and why it is NEVER merged with the non-domestic
  # asset rating.
  dom <- data.table::as.data.table(uprn_epc_lr$domestic)
  dom$epc_dom_rating <- epc_domestic_rating(dom)
  dom <- dom[!is.na(addr) & addr != "", .(
    UPRN,
    epc_dom_address1 = addr, epc_dom_address2 = ADDRESS2,
    epc_dom_address3 = ADDRESS3, epc_dom_postcode = POSTCODE,
    epc_dom_year = year, epc_dom_rating
  )]
  dom <- unique(dom, by = "UPRN")
  dt <- merge(dt, dom, by = "UPRN", all.x = TRUE)

  # --- EPC non-domestic address. epc_nondom_asset_rating is a NUMERIC asset
  # rating on its own scale, deliberately named differently from the
  # domestic A-G band so the two can never be confused or coalesced
  # downstream (see the block comment above epc_domestic_rating()).
  nondom <- data.table::as.data.table(uprn_epc_lr$nondomestic)
  nondom$epc_nondom_asset_rating <- epc_nondomestic_asset_rating(nondom)
  nondom <- nondom[!is.na(adr1) & adr1 != "", .(
    UPRN,
    epc_nondom_address1 = adr1, epc_nondom_address2 = adr2,
    epc_nondom_address3 = adr3, epc_nondom_postcode = postcode,
    epc_nondom_year = year, epc_nondom_asset_rating
  )]
  nondom <- unique(nondom, by = "UPRN")
  dt <- merge(dt, nondom, by = "UPRN", all.x = TRUE)

  # --- DEC address (postcode already attached from NSUL upstream)
  dec <- data.table::as.data.table(dec_addresses)[, .(
    UPRN,
    dec_address = addr, dec_year = year
  )]
  dec <- unique(dec, by = "UPRN")
  dt <- merge(dt, dec, by = "UPRN", all.x = TRUE)

  # --- Land Registry Price Paid: latest transaction per UPRN
  hp <- data.table::as.data.table(
    house_price_lr_uprn[!is.na(house_price_lr_uprn$uprn), c(
      "uprn", "date", "address1", "address2", "address3", "address4",
      "town", "la", "county", "postcode"
    )]
  )
  data.table::setorder(hp, -date, na.last = TRUE)
  hp <- unique(hp, by = "uprn")
  data.table::setnames(hp, c(
    "UPRN", "pp_date", "pp_address1", "pp_address2", "pp_address3",
    "pp_address4", "pp_town", "pp_district", "pp_county", "pp_postcode"
  ))
  dt <- merge(dt, hp, by = "UPRN", all.x = TRUE)

  # --- last sale price + nowcasted 2025 value (optional - house_prices_nowcast_final
  # is house_price_lr_final's own per-UPRN latest transaction, PLUS
  # house_price_extrapolate()'s price_2025 column - see the _targets.R
  # header on Stage 6d. Kept as a separate merge from the Price Paid address
  # block above rather than pulling `price`/`price_2025` into that same
  # data.table::as.data.table(house_price_lr_uprn[...]) select() because
  # house_price_lr_uprn there is deliberately Stage 6d's *_final address
  # data only - added July 2026 for the pmtiles export (pipeline/R/pmtiles.R)
  # after finding this table had NO price/value column at all despite
  # publishing every other price-Paid-derived address field.
  if (!is.null(house_prices_nowcast_final)) {
    val <- data.table::as.data.table(
      house_prices_nowcast_final[!is.na(house_prices_nowcast_final$uprn), c("uprn", "price", "price_2025")]
    )
    data.table::setnames(val, c("UPRN", "pp_price", "current_value_2025"))
    val <- unique(val, by = "UPRN")
    dt <- merge(dt, val, by = "UPRN", all.x = TRUE)
  } else {
    dt$pp_price <- NA_real_
    dt$current_value_2025 <- NA_real_
  }

  # --- best single address line + parsed house number / street
  kn <- data.table::as.data.table(known_uprn_addresses)[, .(
    UPRN,
    best_address = addr, best_postcode = postcode,
    best_house_number = house_number, best_street = street,
    best_address_source = address_source
  )]
  kn <- unique(kn, by = "UPRN")
  dt <- merge(dt, kn, by = "UPRN", all.x = TRUE)

  # --- inferred addresses for UPRNs with no real one (flags preserved)
  inf <- data.table::as.data.table(uprn_infill)[, .(
    UPRN,
    infill_house_number = house_number, infill_street = street,
    infill_postcode = postcode, infill_building_name = building_name,
    infill_district = district, infill_address_source = address_source,
    infill_number_guessed = number_guessed,
    infill_street_confidence = street_confidence
  )]
  inf <- unique(inf, by = "UPRN")
  dt <- merge(dt, inf, by = "UPRN", all.x = TRUE)

  # --- USRN + that street's inferred name. A UPRN can sit on several
  # USRNs; prefer one whose street name is known.
  uu <- merge(
    data.table::as.data.table(uprn_usrn),
    data.table::as.data.table(usrn_street_names)[, .(
      USRN,
      usrn_street = street, usrn_street_confidence = street_confidence
    )],
    by = "USRN", all.x = TRUE
  )
  uu <- uu[order(UPRN, is.na(usrn_street))]
  uu <- unique(uu, by = "UPRN")
  data.table::setcolorder(uu, "UPRN")
  dt <- merge(dt, uu, by = "UPRN", all.x = TRUE)

  # --- INSPIRE parcel containing the UPRN point
  il <- data.table::as.data.table(uprn_inspire_lookup)[, .(
    UPRN,
    inspire_id = INSPIREID, inspire_n_uprn = n_uprn,
    inspire_single_uprn_parcel = single_uprn_parcel
  )]
  il <- unique(il, by = "UPRN")
  dt <- merge(dt, il, by = "UPRN", all.x = TRUE)

  message(
    nrow(dt), " UPRNs in the master address table; ",
    sum(!is.na(dt$best_address)), " with a real address line, ",
    sum(is.na(dt$best_address) & !is.na(dt$infill_street)), " with only an inferred street", ", ",
    sum(!is.na(dt$current_value_2025)), " with a nowcasted 2025 value."
  )
  as.data.frame(dt)
}

# Persist the master table as the repo's published output. Returned path is
# tracked with format = "file" so downstream targets (and the build repo,
# eventually) can depend on the .Rds itself.
save_uprn_all_addresses <- function(uprn_all_addresses,
                                    path = "output/uprn_all_addresses.Rds") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(uprn_all_addresses, path)
  path
}
