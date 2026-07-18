# Display Energy Certificates (DECs) as an extra source of UPRN addresses.
#
# The sibling EPC repo (F:/GitHub/PlaceBasedCarbonCalculator/EPC) publishes
# dec_clean.Rds alongside GB_domestic_epc.Rds / GB_nondomestic_epc.Rds: one
# row per UPRN with the certificate's first address line and the UPRN's
# point location, for public/commercial buildings that hold a DEC but often
# no EPC. dec_clean carries NO postcode column, so postcodes are attached
# from NSUL (authoritative UPRN -> postcode) before the addresses join the
# matching lookups and the known-address table.

# Read dec_clean.Rds down to the address columns this repo needs:
# UPRN, addr (first address line), year, and lon/lat pulled out of the sf
# geometry. One row per UPRN (most recent inspection wins).
load_dec_addresses <- function(path) {
  dec <- readRDS(path)
  xy <- sf::st_coordinates(dec)
  dec <- sf::st_drop_geometry(dec)
  out <- data.frame(
    UPRN = as.numeric(dec$UPRN),
    addr = as.character(dec$ADDRESS1),
    year = lubridate::year(as.Date(dec$INSPECTION_DATE)),
    LONGITUDE = xy[, 1],
    LATITUDE = xy[, 2],
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$UPRN) & !is.na(out$addr) & out$addr != "", ]
  out <- out[order(out$year, decreasing = TRUE), ]
  out[!duplicated(out$UPRN), ]
}

# DECs have no postcode of their own - take it from NSUL via uprn_places
# (UPRN -> postcode, district). Rows whose UPRN is not in NSUL (e.g. it was
# retired before the NSUL epoch) keep an NA postcode and simply won't join
# the postcode-keyed lookups.
attach_nsul_postcode <- function(addresses, uprn_places) {
  pl <- uprn_places[!is.na(uprn_places$postcode), c("UPRN", "postcode")]
  pl <- pl[!duplicated(pl$UPRN), ]
  dplyr::left_join(addresses, pl, by = "UPRN")
}
