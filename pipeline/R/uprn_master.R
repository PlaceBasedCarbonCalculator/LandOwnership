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

build_uprn_all_addresses <- function(uprn_historical, uprn_epc_lr,
                                     dec_addresses, house_price_lr_uprn,
                                     known_uprn_addresses, uprn_infill,
                                     uprn_usrn, usrn_street_names,
                                     uprn_places, uprn_inspire_lookup) {
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
  # an EPC record; class-only rows carry NA addr)
  dom <- data.table::as.data.table(uprn_epc_lr$domestic)
  dom <- dom[!is.na(addr) & addr != "", .(
    UPRN,
    epc_dom_address1 = addr, epc_dom_address2 = ADDRESS2,
    epc_dom_address3 = ADDRESS3, epc_dom_postcode = POSTCODE,
    epc_dom_year = year
  )]
  dom <- unique(dom, by = "UPRN")
  dt <- merge(dt, dom, by = "UPRN", all.x = TRUE)

  # --- EPC non-domestic address
  nondom <- data.table::as.data.table(uprn_epc_lr$nondomestic)
  nondom <- nondom[!is.na(adr1) & adr1 != "", .(
    UPRN,
    epc_nondom_address1 = adr1, epc_nondom_address2 = adr2,
    epc_nondom_address3 = adr3, epc_nondom_postcode = postcode,
    epc_nondom_year = year
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
    sum(is.na(dt$best_address) & !is.na(dt$infill_street)), " with only an inferred street."
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
