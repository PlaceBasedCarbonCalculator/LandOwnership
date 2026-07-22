# Direct UPRN tagging in OSM, added July 2026. Separate from the
# proximity-based OSM address infill in uprn_infill.R
# (build_infill_osm_addresses(): a UPRN point spatially joined into an
# addr-tagged building it happens to fall inside). A large bulk import from
# OS AddressBase/OpenUPRN onto OSM building outlines (and some standalone
# address nodes) instead tags the object directly with its UPRN via
# `ref:gb:uprn` - per taginfo, ~6M objects nationally, mostly ways (so
# almost always building=* polygons, captured by the `multipolygons` layer
# alongside the addr:* tags load_osm_building_addresses() already reads;
# the smaller remainder are nodes with no building outline, in `points`).
# Where present, this is a DIRECT UPRN -> geometry/address crosswalk with no
# proximity guess involved, so it's treated as a real address source (see
# build_osm_uprn_addresses(), fed into build_known_uprn_addresses() in
# uprn_infill.R as source "osm_uprn_tag") rather than an infill fallback.

# All OSM objects carrying a ref:gb:uprn tag, from both the `multipolygons`
# (building outlines - polygon centroid kept as the location) and `points`
# (address nodes) layers. Filter is pushed down to SQLite so only tagged
# objects are read. Reads via oe_read() against the same pbf/layer
# convention as every other OSM loader in this pipeline (uprn_infill.R,
# substations.R) - the first caller to touch a given layer for this pbf pays
# the one-time translation cost into its cached gpkg, every later query
# (including this one) reuses it.
load_osm_uprn_tags <- function(pbf_path) {
  q_poly <- paste0(
    "SELECT osm_id, other_tags, geometry FROM multipolygons ",
    "WHERE other_tags LIKE '%\"ref:gb:uprn\"=>%'"
  )
  q_pts <- paste0(
    "SELECT osm_id, other_tags, geometry FROM points ",
    "WHERE other_tags LIKE '%\"ref:gb:uprn\"=>%'"
  )
  polys <- osmextract::oe_read(pbf_path, layer = "multipolygons", query = q_poly, quiet = TRUE)
  pts <- osmextract::oe_read(pbf_path, layer = "points", query = q_pts, quiet = TRUE)

  # polygon centroid (building footprint) vs the node's own location, both
  # reduced to plain lon/lat - X/Y (27700) is left for
  # build_known_uprn_addresses() to derive, same as every other address
  # source there.
  to_rows <- function(x, geom_type) {
    empty <- data.frame(
      osm_id = character(0), UPRN = numeric(0), osm_housenumber = character(0),
      osm_street = character(0), osm_postcode = character(0), osm_city = character(0),
      osm_geom_type = character(0), LONGITUDE = numeric(0), LATITUDE = numeric(0),
      stringsAsFactors = FALSE
    )
    if (nrow(x) == 0) {
      return(empty)
    }
    if (geom_type == "polygon") {
      x <- sf::st_make_valid(x)
      suppressWarnings(sf::st_geometry(x) <- sf::st_point_on_surface(sf::st_geometry(x)))
    }
    x <- sf::st_transform(x, 4326)
    ll <- sf::st_coordinates(x)

    data.frame(
      osm_id = as.character(x$osm_id),
      UPRN = suppressWarnings(as.numeric(extract_osm_tag(x$other_tags, "ref:gb:uprn"))),
      osm_housenumber = extract_osm_tag(x$other_tags, "addr:housenumber"),
      osm_street = extract_osm_tag(x$other_tags, "addr:street"),
      osm_postcode = normalise_postcode(extract_osm_tag(x$other_tags, "addr:postcode")),
      osm_city = extract_osm_tag(x$other_tags, "addr:city"),
      osm_geom_type = geom_type,
      LONGITUDE = ll[, 1],
      LATITUDE = ll[, 2],
      stringsAsFactors = FALSE
    )
  }

  x <- dplyr::bind_rows(to_rows(polys, "polygon"), to_rows(pts, "point"))
  x <- x[!is.na(x$UPRN), ]

  # a UPRN tagged on more than one OSM object (rare - duplicate/erroneous
  # tagging) is kept once: polygon (building footprint) over point, then
  # first occurrence - same "ambiguous, drop the extras" convention as the
  # spatial joins elsewhere in this pipeline, except here duplicates are a
  # tagging error rather than a genuine spatial ambiguity, so keeping one
  # is safe rather than dropping both.
  x <- x[order(x$osm_geom_type != "polygon"), ]
  n_dup <- sum(duplicated(x$UPRN))
  x <- x[!duplicated(x$UPRN), ]

  message(
    nrow(x), " OSM objects carry a ref:gb:uprn tag (",
    sum(x$osm_geom_type == "polygon"), " polygon, ",
    sum(x$osm_geom_type == "point"), " point, ",
    n_dup, " duplicate-UPRN objects dropped)."
  )
  x
}

# The subset of osm_uprn_tags usable as a real address: needs both a house
# number and a street tag (a UPRN-only tag with no addr:* still counts
# towards osm_uprn_coverage but can't build an address line). Shaped for
# grab() in build_known_uprn_addresses() (uprn_infill.R): UPRN, addr,
# postcode, LATITUDE, LONGITUDE.
build_osm_uprn_addresses <- function(osm_uprn_tags) {
  x <- osm_uprn_tags[
    !is.na(osm_uprn_tags$osm_housenumber) & !is.na(osm_uprn_tags$osm_street),
  ]
  data.frame(
    UPRN = x$UPRN,
    addr = trimws(paste(x$osm_housenumber, x$osm_street)),
    postcode = x$osm_postcode,
    LATITUDE = x$LATITUDE,
    LONGITUDE = x$LONGITUDE,
    stringsAsFactors = FALSE
  )
}
