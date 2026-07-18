# Electricity substation matching, added July 2026. Electricity substation
# titles are a recurring, distinctive category in CCOD/OCOD: they're almost
# never geocodable from their address text alone (freehold titles with no
# postcode, describing the substation and little else - see
# split_nopc_complex() in split_addresses.R, which used to drop them
# entirely as `nopc_complex`), but unlike ordinary "land" titles a
# substation is a real, discrete feature that OSM tags explicitly
# (power=substation) and that usually carries its own UPRN. This file:
#   1. detects substation-describing text (is_substation_address()), used
#      both to route nopc titles through the ordinary boilerplate pipeline
#      instead of dropping them (split_addresses.R) and to gate the new
#      matching stage in match_free_sources.R;
#   2. loads OSM's power=substation features (points + polygons);
#   3. cross-references them against the UPRN dataset to build a lookup
#      match_free_sources.R can join against, in the same key/UPRN/
#      LATITUDE/LONGITUDE/match_source/match_quality shape every other
#      lookup there uses.

# Detect substation-describing text. Deliberately permissive - not anchored
# to "electricity" or a leading article - since every phrasing this project
# has seen ("an electricity sub station", "Sub-Station", "SUBSTATION",
# "sub stations") still contains this core token. Used as a gate: only rows
# that pass this ever try the substation-specific lookup, so an ordinary
# title sharing a street with a substation can never accidentally inherit
# the substation's UPRN.
is_substation_address <- function(x) {
  x <- ifelse(is.na(x), "", x)
  grepl("\\bsub[- ]?stations?\\b", x, ignore.case = TRUE)
}

# ---------------------------------------------------------------------------
# OSM loaders
# ---------------------------------------------------------------------------

# Node-tagged substations (small distribution substations, usually mapped as
# a single point rather than a compound polygon). `power` isn't a promoted
# GDAL column for the `points` layer (same as `building`/`addr:*` aren't for
# `multipolygons` - see extract_osm_tag() in uprn_infill.R), so it's matched
# inside the other_tags hstore. Same oe_read()/query convention as
# load_osm_road_names() - translates the `points` layer into the pbf's own
# cached gpkg once, reused on every later call.
load_osm_substation_points <- function(pbf_path) {
  q <- paste0(
    "SELECT osm_id, name, other_tags, geometry FROM points ",
    "WHERE other_tags LIKE '%\"power\"=>\"substation\"%'"
  )
  x <- osmextract::oe_read(pbf_path, layer = "points", query = q, quiet = TRUE)
  x$osm_ref <- extract_osm_tag(x$other_tags, "ref")
  x$osm_operator <- extract_osm_tag(x$other_tags, "operator")
  x$other_tags <- NULL
  x$osm_geom_type <- "point"
  sf::st_transform(x, 27700)
}

# Polygon-tagged substations (larger compounds - a fenced yard with its own
# footprint). Read via oe_read() against the `multipolygons` layer, same as
# load_osm_building_addresses() - the first caller to touch this layer for a
# given pbf pays the translation cost, every later query (including this
# one) reuses the cached gpkg.
load_osm_substation_polygons <- function(pbf_path) {
  q <- paste0(
    "SELECT osm_id, osm_way_id, name, other_tags, geometry FROM multipolygons ",
    "WHERE other_tags LIKE '%\"power\"=>\"substation\"%'"
  )
  x <- osmextract::oe_read(pbf_path, layer = "multipolygons", query = q, quiet = TRUE)
  x$osm_ref <- extract_osm_tag(x$other_tags, "ref")
  x$osm_operator <- extract_osm_tag(x$other_tags, "operator")
  x$other_tags <- NULL
  x$osm_geom_type <- "polygon"
  sf::st_make_valid(sf::st_transform(x, 27700))
}

# ---------------------------------------------------------------------------
# UPRN crosswalk
# ---------------------------------------------------------------------------

# Match UPRNs to the substation they sit on/at. Polygon substations: the
# UPRN must fall strictly within the compound (st_within) - a UPRN inside
# two overlapping compounds is ambiguous and dropped, same convention as
# build_infill_osm_addresses(). Point substations (no footprint to test
# containment against): the single nearest UPRN within `point_tolerance_m`,
# only when unambiguous (exactly one candidate that close) - small
# distribution substations are usually a single kiosk, so a tight tolerance
# avoids grabbing a neighbouring building's UPRN instead.
build_substation_uprn_lookup <- function(osm_substation_points, osm_substation_polygons,
                                         uprn_historical, point_tolerance_m = 20) {
  uprn_cols <- c("UPRN", "LATITUDE", "LONGITUDE", "X_COORDINATE", "Y_COORDINATE")
  uprn_pts <- sf::st_as_sf(
    uprn_historical[, uprn_cols],
    coords = c("X_COORDINATE", "Y_COORDINATE"), crs = 27700, remove = FALSE
  )

  poly_hits <- data.frame(
    UPRN = numeric(0), osm_id = character(0), name = character(0),
    stringsAsFactors = FALSE
  )
  if (nrow(osm_substation_polygons) > 0) {
    j <- sf::st_join(osm_substation_polygons[, c("osm_id", "name")], uprn_pts, join = sf::st_contains)
    j <- sf::st_drop_geometry(j)
    j <- j[!is.na(j$UPRN), ]
    j <- j[!j$UPRN %in% j$UPRN[duplicated(j$UPRN)], ] # UPRN in >1 compound - ambiguous
    poly_hits <- j[, c("UPRN", "osm_id", "name")]
    message(nrow(poly_hits), " UPRNs matched inside an OSM substation compound.")
  }

  point_hits <- data.frame(
    UPRN = numeric(0), osm_id = character(0), name = character(0),
    stringsAsFactors = FALSE
  )
  pts <- osm_substation_points[!osm_substation_points$osm_id %in% poly_hits$osm_id, ]
  if (nrow(pts) > 0) {
    near <- sf::st_is_within_distance(pts, uprn_pts, dist = point_tolerance_m)
    n_hit <- lengths(near)
    unambiguous <- n_hit == 1
    if (any(unambiguous)) {
      point_hits <- data.frame(
        UPRN = uprn_pts$UPRN[unlist(near[unambiguous])],
        osm_id = pts$osm_id[unambiguous],
        name = pts$name[unambiguous],
        stringsAsFactors = FALSE
      )
      # the same UPRN nearest to two different substation nodes is ambiguous
      point_hits <- point_hits[!point_hits$UPRN %in% point_hits$UPRN[duplicated(point_hits$UPRN)], ]
    }
    message(
      nrow(point_hits), " UPRNs matched to a single nearby OSM substation node ",
      "(within ", point_tolerance_m, "m)."
    )
  }

  hits <- dplyr::bind_rows(poly_hits, point_hits)
  ui <- match(hits$UPRN, uprn_historical$UPRN)
  hits$LATITUDE <- uprn_historical$LATITUDE[ui]
  hits$LONGITUDE <- uprn_historical$LONGITUDE[ui]
  hits
}

# ---------------------------------------------------------------------------
# match_free_sources.R lookup
# ---------------------------------------------------------------------------

# Turn the UPRN crosswalk into match keys, same key/UPRN/LATITUDE/LONGITUDE/
# match_source/match_quality shape every other lookup in match_free_sources.R
# uses. Substation titles never carry a house number, so the usual
# street_number_key() (which requires one) doesn't apply - the
# (district, street) key here is built with exactly the same
# paste(normalise_name(district), normalise_name(street), sep = "|") style
# the street-centroid fallback in match_free_sources() already uses for
# other numberless rows.
build_substation_lookup <- function(substation_uprn_lookup, uprn_places, uprn_usrn, usrn_street_names) {
  empty <- data.frame(
    key = character(0), UPRN = numeric(0), LATITUDE = numeric(0), LONGITUDE = numeric(0),
    match_source = character(0), match_quality = character(0), stringsAsFactors = FALSE
  )
  if (nrow(substation_uprn_lookup) == 0) {
    return(empty)
  }

  sub <- substation_uprn_lookup
  pi <- match(sub$UPRN, uprn_places$UPRN)
  sub$district <- uprn_places$district[pi]

  # street key: only where the substation's UPRN sits on a USRN
  # usrn_street_names could name
  us <- dplyr::inner_join(sub[, c("UPRN", "district")], uprn_usrn, by = "UPRN")
  us <- dplyr::inner_join(us, usrn_street_names[, c("USRN", "street")], by = "USRN")
  us <- us[!is.na(us$district) & !is.na(us$street), ]
  street_keys <- data.frame(
    key = paste(normalise_name(us$district), normalise_name(us$street), sep = "|"),
    UPRN = us$UPRN,
    match_source = "substation_street",
    match_quality = "medium",
    stringsAsFactors = FALSE
  )

  # district-singleton fallback: only where a district has exactly one known
  # substation UPRN. Speculative - one *known* OSM substation in a district
  # isn't proof there's only one there (OSM substation tagging is
  # incomplete) - so this is quality "low", same "indicative, not resolved"
  # tier as the infill gap-guesses and street-centroid fallback (see
  # docs/uprn_infill_design_2026-07.md).
  dist_n <- table(sub$district[!is.na(sub$district)])
  single_districts <- names(dist_n)[dist_n == 1]
  singles <- sub[!is.na(sub$district) & sub$district %in% single_districts, ]
  district_keys <- data.frame(
    key = normalise_name(singles$district),
    UPRN = singles$UPRN,
    match_source = "substation_district_singleton",
    match_quality = "low",
    stringsAsFactors = FALSE
  )

  lookup <- dplyr::bind_rows(street_keys, district_keys)
  if (nrow(lookup) == 0) {
    return(empty)
  }
  ui <- match(lookup$UPRN, substation_uprn_lookup$UPRN)
  lookup$LATITUDE <- substation_uprn_lookup$LATITUDE[ui]
  lookup$LONGITUDE <- substation_uprn_lookup$LONGITUDE[ui]

  # ambiguous keys (two substation UPRNs sharing the same district+street,
  # or two singleton districts colliding after name normalisation) can't be
  # trusted - drop, same convention as lookup_dedupe() in match_free_sources.R
  lookup <- lookup[!is.na(lookup$key), ]
  lookup[!lookup$key %in% lookup$key[duplicated(lookup$key)], ]
}

# Coverage summary: how many OSM substations were found, how many resolved
# to a UPRN, and how many LR titles flagged as substations actually matched.
# Same spirit as the checks in audit_uprn_coverage.R.
audit_substation_matches <- function(osm_substation_points, osm_substation_polygons,
                                     substation_uprn_lookup, needs_geocode, free_match) {
  n_substation_titles <- sum(is_substation_address(needs_geocode$AddressLine))
  matched_sources <- c("substation_street_match", "substation_district_singleton_match")
  n_substation_matched <- sum(free_match$matched$source %in% matched_sources)

  list(
    n_osm_substation_points = nrow(osm_substation_points),
    n_osm_substation_polygons = nrow(osm_substation_polygons),
    n_substation_uprns = nrow(substation_uprn_lookup),
    n_substation_titles_seen = n_substation_titles,
    n_substation_titles_matched = n_substation_matched,
    match_rate = round(n_substation_matched / max(n_substation_titles, 1), 4)
  )
}
