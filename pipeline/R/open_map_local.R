# OS Open Map Local (opmplc_gpkg_gb_*.zip), added July 2026: the `road`
# layer carries a `distinctive_name` (near-complete coverage) and a
# `classification` (Motorway / Primary Road / A Road / B Road / Minor Road /
# Local Road / Restricted Local Access Road / Local Access Road / Guided
# Busway Carriageway / Shared Use Carriageway - each optionally suffixed
# ", Collapsed Dual Carriageway" where OS has generalised a dual
# carriageway down to a single centreline) that OS Open USRN doesn't have.
# Geometrically the two products describe the same real-world roads, but
# NOT 1:1: Open Map Local splits a road into a new segment at every
# junction (so one USRN street can span several `road` rows), and for major
# roads it further simplifies dual carriageways into one line where USRN
# may still carry two. link_usrn_oml() below handles both by sampling
# several points along each USRN line rather than relying on a single
# probe point or an exact geometry match.
#
# Used two ways downstream:
#   1. Street naming (build_usrn_street_names(), uprn_infill.R): tried
#      AFTER the EPC/Price-Paid majority vote but BEFORE the OSM road-name
#      fallback - OML is authoritative OS data with near-complete coverage,
#      so it should beat OSM naming; OSM is only asked for whatever OML
#      still leaves unnamed (e.g. back alleys/private ways OML doesn't map
#      at all - see the file header note below).
#   2. Queue routing (geocode_queue.R): `classification` flags road types
#      that can never front a real property (motorways, guided busways) so
#      an address that only resolves to one of those streets is never worth
#      a paid Azure call. A/B roads are NOT blanket-excluded - per Malcolm,
#      "A roads come in a real mix, some are like motorways while others
#      are small local roads" - only the classes below are unambiguous.
#
# Known limitation carried over from Malcolm's brief: Open Map Local omits
# back alleys/service ways that DO appear in Open USRN - those USRNs simply
# get no OML match here and fall through to the OSM fallback (or stay
# unnamed) exactly as before this file existed.

# Road classifications that, by construction, never have a building
# fronting them - motorways and guided/shared-use busways carry no postal
# addresses at all. Ordinary A/B/local roads (including "Primary Road",
# which just means "part of the primary route network", not "motorway-like")
# are deliberately NOT included here.
oml_no_property_classes <- c(
  "Motorway", "Motorway, Collapsed Dual Carriageway",
  "Guided Busway Carriageway", "Shared Use Carriageway"
)

oml_excludes_properties <- function(classification) {
  !is.na(classification) & classification %in% oml_no_property_classes
}

# Read the `road` line layer straight out of the (large, ~8GB) source gpkg
# inside its zip via GDAL's /vsizip/ virtual filesystem - same convention as
# load_usrn_geometry() (uprn_infill.R), which reads OS Open USRN's gpkg the
# same way. Not a `format = "file"` target for the same hashing-cost reason
# as the other big constants in _targets.R - the path is a plain constant,
# bumped on future re-downloads.
load_open_map_local_roads <- function(zip_path) {
  fls <- utils::unzip(zip_path, list = TRUE)$Name
  gpkg <- fls[grepl("\\.gpkg$", fls)][1]
  vsi <- paste0("/vsizip/", gsub("\\\\", "/", zip_path), "/", gpkg)

  q <- "SELECT id, classification, distinctive_name, road_number, geometry FROM road"
  x <- sf::st_read(vsi, layer = "road", query = q, quiet = TRUE)
  x <- sf::st_zm(x)
  if (is.na(sf::st_crs(x))) {
    sf::st_crs(x) <- 27700
  } else if (sf::st_crs(x) != sf::st_crs(27700)) {
    x <- sf::st_transform(x, 27700)
  }
  message(
    nrow(x), " Open Map Local road segments loaded (",
    sum(!is.na(x$distinctive_name)), " with a distinctive_name)."
  )
  x
}

# Sample points at roughly `spacing` metres along each line in `geom`; very
# short lines that produce zero samples at that density fall back to a
# single point-on-surface probe (the same single-probe-point simplification
# name_usrns_from_osm() already uses, just as a floor rather than the
# general case here). Returns a data.frame of plain (id, x, y) - not sf -
# since the only thing every caller needs is coordinates to feed to a
# nearest-neighbour search.
#
# MULTILINESTRING handling is NOT optional: sf::st_line_sample() asserts
# inherits(x, "sfc_LINESTRING") and errors outright otherwise, and OS Open
# USRN publishes EVERY one of its 1.77M streets as MULTILINESTRING (checked
# against osopenusrn_202607 - 1766832 of 1766832, not a mix). Without the
# cast below this function - and therefore link_usrn_oml() and the whole
# usrn_oml_link target - fails on the first real call while passing happily
# against LINESTRING test fixtures. Casting to component parts and carrying
# `row_id` through maps every sampled point back to its parent feature; a
# street digitised in several disconnected pieces simply contributes probes
# from each piece, which is if anything better evidence for the vote.
sample_line_points <- function(ids, geom, spacing = 25) {
  if (length(geom) == 0) {
    return(data.frame(id = ids[0], x = numeric(0), y = numeric(0)))
  }
  parts <- sf::st_sf(row_id = seq_along(geom), geometry = geom)
  if (!inherits(geom, "sfc_LINESTRING")) {
    parts <- suppressWarnings(sf::st_cast(parts, "LINESTRING", warn = FALSE))
  }
  pgeom <- sf::st_geometry(parts)

  samp <- suppressWarnings(sf::st_line_sample(pgeom, density = 1 / spacing))
  # NB: lengths() on a MULTIPOINT sfc returns the underlying MATRIX length
  # (2 x the point count), not the point count - count rows explicitly so
  # this stays correct if it's ever used for more than an is-empty test.
  n_pts <- vapply(samp, function(g) {
    m <- unclass(g)
    if (is.null(m) || length(m) == 0) 0L else as.integer(nrow(m))
  }, integer(1))

  empty <- n_pts == 0
  if (any(empty)) {
    fallback <- suppressWarnings(sf::st_point_on_surface(pgeom[empty]))
    samp[empty] <- sf::st_cast(sf::st_sfc(fallback, crs = sf::st_crs(pgeom)), "MULTIPOINT")
  }
  pts_sf <- sf::st_sf(row_id = parts$row_id, geometry = samp)
  pts_sf <- sf::st_cast(pts_sf, "POINT", warn = FALSE)
  xy <- sf::st_coordinates(pts_sf)
  data.frame(id = ids[pts_sf$row_id], x = xy[, 1], y = xy[, 2])
}

# Link every USRN to its best-matching Open Map Local road(s) by sampling
# points along the USRN line (see sample_line_points() - this is what makes
# junction-split OML segments and simplified dual-carriageway geometry a
# non-issue: several probes per USRN, majority vote of whichever OML
# segment(s) they land nearest) and taking the nearest OML segment within
# `max_dist` metres of each probe (15m default - same tolerance
# name_usrns_from_osm() uses for the equivalent OSM join, since a USRN and
# its OML twin should practically overlap).
#
# Returns one row per USRN that got at least one hit, with:
#   street / street_n / street_agreement - majority-vote distinctive_name
#     among probes that DID land near a named OML segment (unnamed OML
#     segments, e.g. short unnamed slip roads, still count as classification
#     evidence but never win the name vote).
#   oml_classification / oml_class_agreement - majority-vote classification
#     among ALL probes (named or not) - kept even for USRNs that end up
#     named by EPC/Price-Paid evidence instead, so geocode_queue.R can still
#     use it for road-type routing regardless of how the street got its name.
#   oml_road_number - the classification winner's most common road_number
#     (e.g. "A1"), NA for unclassified-number roads.
# chunk_size mirrors name_usrns_from_osm()'s chunking (uprn_infill.R) - the
# same "hundreds of thousands of probe points against millions of OML
# segments" scale.
link_usrn_oml <- function(usrn_geom, oml_roads, max_dist = 15, spacing = 25,
                          chunk_size = 200000L) {
  empty <- data.frame(
    USRN = numeric(0), street = character(0), street_n = integer(0),
    street_agreement = numeric(0), oml_classification = character(0),
    oml_class_agreement = numeric(0), oml_road_number = character(0)
  )
  if (nrow(usrn_geom) == 0 || is.null(oml_roads) || nrow(oml_roads) == 0) {
    return(empty)
  }

  probes <- sample_line_points(usrn_geom$usrn, sf::st_geometry(usrn_geom), spacing = spacing)
  chunks <- split(seq_len(nrow(probes)), ceiling(seq_len(nrow(probes)) / chunk_size))

  hits <- lapply(seq_along(chunks), function(i) {
    p <- probes[chunks[[i]], ]
    p_sf <- sf::st_as_sf(p, coords = c("x", "y"), crs = 27700)
    nearest <- sf::st_nearest_feature(p_sf, oml_roads)
    d <- as.numeric(sf::st_distance(p_sf, oml_roads[nearest, ], by_element = TRUE))
    keep <- d <= max_dist
    message("  OML link chunk ", i, "/", length(chunks), ": ", sum(keep), "/", nrow(p), " probes matched")
    data.frame(
      USRN = p$id[keep],
      street = normalise_name(oml_roads$distinctive_name[nearest][keep]),
      classification = oml_roads$classification[nearest][keep],
      road_number = oml_roads$road_number[nearest][keep],
      stringsAsFactors = FALSE
    )
  })
  hits <- dplyr::bind_rows(hits)
  if (nrow(hits) == 0) {
    return(empty)
  }

  dt <- data.table::as.data.table(hits)
  per_usrn <- dt[, {
    named <- street[!is.na(street)]
    if (length(named) > 0) {
      tab <- sort(table(named), decreasing = TRUE)
      street_win <- names(tab)[1]
      street_n_win <- as.integer(tab[1])
      street_agreement_win <- street_n_win / length(named)
    } else {
      street_win <- NA_character_
      street_n_win <- 0L
      street_agreement_win <- NA_real_
    }

    ctab <- sort(table(classification), decreasing = TRUE)
    class_win <- names(ctab)[1]
    class_agreement_win <- as.numeric(ctab[1]) / .N
    rn <- road_number[classification == class_win & !is.na(road_number)]
    road_number_win <- if (length(rn) > 0) names(sort(table(rn), decreasing = TRUE))[1] else NA_character_

    list(
      street = street_win, street_n = street_n_win, street_agreement = street_agreement_win,
      oml_classification = class_win, oml_class_agreement = class_agreement_win,
      oml_road_number = road_number_win
    )
  }, by = USRN]

  as.data.frame(per_usrn)
}
