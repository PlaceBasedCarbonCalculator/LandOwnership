# Two small official open-data extras added July 2026, both under
# F:/GitHub/PlaceBasedCarbonCalculator/inputdata/os_uprn/other_uprn_sources:
#   - cultural_venues_in_GIS_format.gpkg: London's Cultural Infrastructure
#     Map (GLA, 2023) - one gpkg layer per venue category (museums, theatres,
#     archives, ...), all sharing the same 14-column schema.
#   - education_establishments.zip: DfE "Get information about schools"
#     (GIAS) full establishment extract, one CSV.
# Both carry their own UPRN column, so - like osm_uprn.R's ref:gb:uprn tags -
# they're real address sources fed into build_known_uprn_addresses()
# (uprn_infill.R) as "cultural_venue"/"education", not infill guesses.

# GIA's Cultural Infrastructure Map ships one layer per venue category
# (identical schema) rather than one combined layer - read every layer and
# stack them. `os_addressbase_uprn` is meant to hold an OS AddressBase UPRN
# but a sizeable minority of rows carry a placeholder instead of a real
# match: free text ("No UPRN", "See notes", ...), which as.numeric() already
# turns into NA, or a suspiciously round sentinel number (100000000000,
# 200000000000, 100023000000, ... - one per London borough). Genuine UPRNs
# are never round to the nearest million, so that catches the numeric
# placeholders the text check misses.
load_cultural_venue_addresses <- function(gpkg_path) {
  layers <- sf::st_layers(gpkg_path)$name
  rows <- lapply(layers, function(l) {
    sf::st_drop_geometry(sf::st_read(gpkg_path, layer = l, quiet = TRUE))
  })
  x <- dplyr::bind_rows(rows)

  uprn <- suppressWarnings(as.numeric(x$os_addressbase_uprn))
  uprn[!is.na(uprn) & uprn %% 1e6 == 0] <- NA_real_

  out <- data.frame(
    UPRN = uprn,
    addr = trimws(x$address1),
    postcode = normalise_postcode(x$address3),
    LATITUDE = as.numeric(x$latitude),
    LONGITUDE = as.numeric(x$longitude),
    X_COORDINATE = as.numeric(x$easting),
    Y_COORDINATE = as.numeric(x$northing),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$UPRN) & !is.na(out$addr) & out$addr != "", ]
  out <- out[!duplicated(out$UPRN), ]
  message(nrow(out), " cultural venues have a usable UPRN + address.")
  out
}

# About two thirds of cultural venue rows record no postcode anywhere in
# address1/2/3 (unlike the schools extract, which always has one) - but
# every row does carry a UPRN, so the gap is filled from NSUL the same way
# attach_nsul_postcode() does for DEC in epc_addresses.R. Unlike DEC, these
# rows sometimes already have a postcode, so this coalesces rather than
# blindly left-joining (which would collide into postcode.x/postcode.y).
fill_missing_postcode_from_nsul <- function(addresses, uprn_places) {
  pl <- uprn_places[!is.na(uprn_places$postcode), c("UPRN", "postcode")]
  pl <- pl[!duplicated(pl$UPRN), ]
  pi <- match(addresses$UPRN, pl$UPRN)
  addresses$postcode <- ifelse(is.na(addresses$postcode), pl$postcode[pi], addresses$postcode)
  addresses
}

# DfE GIAS "edubasealldata" full establishment extract. Street already
# combines house number + street name in the same style as an EPC/Price-Paid
# first address line. Closed/not-yet-open establishments are dropped: a
# closed school's address would only add a stale entry to
# known_uprn_addresses, not fill a gap.
load_education_establishment_addresses <- function(zip_path) {
  tmp <- tempfile("edubase")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  fls <- utils::unzip(zip_path, list = TRUE)$Name
  csvf <- fls[grepl("\\.csv$", fls)][1]
  utils::unzip(zip_path, files = csvf, exdir = tmp)

  cols <- c("EstablishmentStatus (name)", "Street", "Postcode", "Easting", "Northing", "UPRN")
  dt <- data.table::fread(
    file.path(tmp, csvf),
    select = cols, integer64 = "numeric", showProgress = FALSE
  )
  data.table::setnames(dt, cols, c("status", "street", "postcode", "easting", "northing", "UPRN"))

  dt <- dt[status %in% c("Open", "Open, but proposed to close")]
  dt <- dt[!is.na(UPRN) & UPRN > 0 & !is.na(street) & street != "" &
    !is.na(easting) & !is.na(northing)]
  dt <- as.data.frame(dt)

  xy <- sf::st_coordinates(sf::st_transform(
    sf::st_as_sf(dt, coords = c("easting", "northing"), crs = 27700),
    4326
  ))
  out <- data.frame(
    UPRN = as.numeric(dt$UPRN),
    addr = trimws(dt$street),
    postcode = normalise_postcode(dt$postcode),
    LATITUDE = xy[, 2],
    LONGITUDE = xy[, 1],
    X_COORDINATE = dt$easting,
    Y_COORDINATE = dt$northing,
    stringsAsFactors = FALSE
  )
  out <- out[!duplicated(out$UPRN), ]
  message(nrow(out), " open education establishments have a usable UPRN + address.")
  out
}
