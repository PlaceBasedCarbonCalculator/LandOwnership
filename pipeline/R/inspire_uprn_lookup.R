# Stage 7 (task 7): clean the INSPIRE cadastral parcels and build a
# UPRN -> INSPIRE ID lookup, so a geolocated address can be upgraded from a
# point to its actual land parcel wherever there's exactly one UPRN inside
# that parcel.
#
# Polygon cleaning logic ported from R/prep_land_registry_alt.R (LA-by-LA
# GML import, merging parcels that were artificially split along the 500m
# British National Grid). Changes from the original:
#   - Kept in native EPSG:27700 (British National Grid) instead of
#     reprojecting to 4326 - that reprojection was only needed for the
#     tile-publishing geojson output; the UPRN spatial join is more
#     accurate done in the native CRS both datasets already share.
#   - Each LA's processing is wrapped in tryCatch(). The original script's
#     own comment notes "fails at 68 city of london - Splitting a Line by a
#     GeometryCollection is unsupported" but doesn't actually handle it -
#     a single bad LA would crash the entire ~300+ LA run. Failures are now
#     logged and skipped instead.
#   - Runs LAs in parallel (furrr/future multisession), same fix as the
#     build repo's R/inspire_v2.R for the identical sequential bottleneck
#     (v1 of load_inspire_clean() took days single-threaded over ~318 LAs;
#     each LA is independent so they parallelise directly). clean_inspire_la()
#     already extracts to a tempfile()-unique dir per call, so it's safe to
#     run concurrently without change.

# Reads every INSPIRE zip in `path` (one per Local Authority) and returns
# cleaned polygons with `local_authority`, `INSPIREID`, `area` (m2).
# When options(pipeline.sample_n = <n>) is set, only the first `n` zips are
# processed (INSPIRE cleaning is slow - this is for smoke-testing).
load_inspire_clean <- function(path, workers = NULL) {
  zips <- list.files(path, pattern = "\\.zip$", full.names = TRUE)
  sample_n <- pipeline_sample_zips()
  if (!is.na(sample_n)) {
    zips <- utils::head(zips, sample_n)
  }

  if (is.null(workers)) {
    workers <- min(8, max(1, future::availableCores() - 1))
  }
  future::plan("multisession", workers = workers)
  on.exit(future::plan("sequential"), add = TRUE)

  la_results <- furrr::future_map(
    zips,
    function(zip) {
      tryCatch(
        {
          result <- clean_inspire_la(zip)
          message(Sys.time(), " ", basename(zip), " ", nrow(result), " polygons")
          result
        },
        error = function(e) {
          message("INSPIRE cleaning failed for ", basename(zip), ": ", conditionMessage(e))
          e
        }
      )
    },
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE)
  )

  failed <- vapply(la_results, inherits, logical(1), "error")
  polys <- la_results[!failed]
  failures <- zips[failed]

  nms <- gsub("\\.zip$", "", basename(zips[!failed]))
  nms <- gsub("_Borough_Council|_District_Council|_Metropolitan_Borough_Council|_Council", "", nms)
  nms <- gsub("_", " ", nms)
  names(polys) <- nms

  polys_all <- data.table::rbindlist(polys, idcol = "local_authority", fill = TRUE)
  polys_all <- sf::st_as_sf(polys_all)
  polys_all <- polys_all[!duplicated(polys_all$GEOMETRY), ]

  if (length(failures) > 0) {
    attr(polys_all, "failures") <- failures
  }
  polys_all
}

# Clean a single LA's INSPIRE GML: rebuild the 500m grid over its bounding
# box, find parcels whose edges lie on it, and merge the two parcels either
# side of each shared grid-line segment back together (this repairs the
# INSPIRE publication artefact where a parcel crossing a paper-map sheet
# boundary is split into two separate polygons). A final pass merges any
# leftover exact 500m x 500m squares into their neighbours.
clean_inspire_la <- function(zip_path) {
  tmp <- tempfile("inspire")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  utils::unzip(zip_path, exdir = tmp)
  gml <- list.files(tmp, pattern = "\\.gml$", full.names = TRUE)[1]
  poly <- sf::read_sf(gml)
  poly <- poly[, c("INSPIREID", "VALIDFROM", "BEGINLIFESPANVERSION")]

  grd <- sf::st_bbox(poly)
  grd[1] <- plyr::round_any(grd[1], 500, f = floor)
  grd[2] <- plyr::round_any(grd[2], 500, f = floor)
  grd[3] <- plyr::round_any(grd[3], 500, f = ceiling)
  grd[4] <- plyr::round_any(grd[4], 500, f = ceiling)
  grd <- sf::st_as_sfc(grd)
  grd <- sf::st_make_grid(grd, c(500, 500))

  sub <- poly[grd, , op = sf::st_touches]
  grd_line <- sf::st_cast(grd, "LINESTRING")
  suppressWarnings(sub_line <- sf::st_cast(sub, "LINESTRING"))

  grd_line <- sf::st_as_sf(grd_line)
  grd_line$id <- 1
  grd_line <- stplanr::overline2(grd_line, "x", simplify = FALSE, quiet = TRUE)
  sub2 <- sub_line[grd_line, , op = sf::st_overlaps]

  sub3 <- sub_line[grd_line, , op = sf::st_covers]
  sub3 <- sub3[!sub3$INSPIREID %in% sub2$INSPIREID, ]
  if (nrow(sub3) > 0) sub2 <- rbind(sub2, sub3)

  if (nrow(sub2) > 0) {
    suppressWarnings(sub2_pt <- sf::st_cast(sub2, "POINT"))
    sub2_pt <- sub2_pt[grd_line, ]
    sub2_pt <- sub2_pt[!duplicated(sub2_pt$GEOMETRY), ]
    sub2_pt <- sf::st_combine(sub2_pt)

    grd_line <- lwgeom::st_split(grd_line, sub2_pt)
    grd_line <- sf::st_collection_extract(grd_line, "LINESTRING")
    grd_line <- sf::st_as_sf(grd_line)
    grd_line$id <- as.character(sample(seq_len(nrow(grd_line)), nrow(grd_line)))
    grd_line <- grd_line[sub2, , op = sf::st_covered_by]

    sub2 <- poly[poly$INSPIREID %in% sub2$INSPIREID, ]
    sub2_new <- sub2

    for (j in seq_len(nrow(grd_line))) {
      lin <- grd_line[j, ]
      sub2_sel <- sub2_new[lin, , op = sf::st_covers]
      if (nrow(sub2_sel) > 2) sub2_sel <- sub2_new[lin, , op = sf::st_intersects]
      if (nrow(sub2_sel) != 2) next

      sub2_new <- sub2_new[!sub2_new$INSPIREID %in% sub2_sel$INSPIREID, ]
      sub2_sel_geom <- sf::st_union(sub2_sel)
      sub2_sel <- sub2_sel[1, ]
      sub2_sel$GEOMETRY <- sub2_sel_geom
      sub2_new <- rbind(sub2_new, sub2_sel)
    }

    poly_new <- poly[!poly$INSPIREID %in% sub2$INSPIREID, ]
    poly_new <- rbind(poly_new, sub2_new)
  } else {
    poly_new <- poly
  }

  poly_new$area <- as.numeric(sf::st_area(poly_new))
  poly_new$perimiter <- as.numeric(sf::st_perimeter(poly_new))

  poly_squares <- poly_new[poly_new$area == 250000 & poly_new$perimiter == 2000, ]
  if (nrow(poly_squares) > 0) {
    poly_new <- poly_new[!poly_new$INSPIREID %in% poly_squares$INSPIREID, ]
    for (j in seq_len(nrow(poly_squares))) {
      sqr <- poly_squares[j, ]
      poly_sel <- poly_new[sqr, , op = sf::st_intersects]
      poly_new <- poly_new[!poly_new$INSPIREID %in% poly_sel$INSPIREID, ]
      poly_sel <- rbind(poly_sel, sqr)
      poly_sel_geom <- sf::st_union(poly_sel)
      poly_sel <- poly_sel[1, ]
      poly_sel$GEOMETRY <- poly_sel_geom
      poly_new <- rbind(poly_new, poly_sel)
    }
  }

  if (!"sfc_POLYGON" %in% class(poly_new$GEOMETRY)) {
    poly_mp <- poly_new[sf::st_geometry_type(poly_new) == "MULTIPOLYGON", ]
    poly_new <- poly_new[sf::st_geometry_type(poly_new) == "POLYGON", ]
    poly_mp <- sf::st_cast(poly_mp, "POLYGON")
    poly_new <- rbind(poly_new, poly_mp)
  }

  poly_new$area <- round(as.numeric(sf::st_area(poly_new)))
  poly_new <- sf::st_make_valid(poly_new)
  poly_new[!sf::st_is_empty(poly_new), ]
}

# Spatial-join OS Open UPRN points (native BNG, from uprn_historical) into
# the cleaned INSPIRE parcels, then flag whether each parcel contains
# exactly one UPRN (the simple "one property on one parcel" case, safe to
# treat the polygon as that address's land holding) or several.
build_uprn_inspire_lookup <- function(inspire_polys, uprn_historical) {
  uprn_pts <- sf::st_as_sf(
    uprn_historical[, c("UPRN", "X_COORDINATE", "Y_COORDINATE")],
    coords = c("X_COORDINATE", "Y_COORDINATE"), crs = 27700
  )
  inspire_bng <- sf::st_transform(inspire_polys[, c("INSPIREID", "local_authority", "area")], 27700)

  joined <- sf::st_join(uprn_pts, inspire_bng, join = sf::st_within)
  joined <- sf::st_drop_geometry(joined)
  joined <- joined[!is.na(joined$INSPIREID), ]

  uprn_counts <- joined |>
    dplyr::count(INSPIREID, name = "n_uprn")

  lookup <- dplyr::left_join(joined, uprn_counts, by = "INSPIREID")
  lookup$single_uprn_parcel <- lookup$n_uprn == 1
  lookup
}
