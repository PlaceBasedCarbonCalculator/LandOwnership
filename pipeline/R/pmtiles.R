# UPRN PMTiles export: a single vector tileset of every known UPRN carrying
# the best address text this repo can construct, its provenance, EPC rating
# and nowcasted 2025 value - see the "uprn_pmtiles.html" viewer
# (docs/uprn_pmtiles.html) for a click-to-inspect map.
#
# GeoJSON/PMTiles machinery (make_geojson(), make_pmtiles()) is PORTED,
# near-verbatim, from the sibling build repo's R/make_geojson.R and
# R/pmtiles.R rather than re-implemented - same tippecanoe-via-WSL
# convention (tippecanoe must be installed; on Windows this runs it inside
# WSL, so WSL + tippecanoe both need to be present). The point-tileset
# options below (single-pass, `drop = TRUE`, `extend_zoom = TRUE`) mirror
# the build repo's own `pmtiles_uprn_unknown` target - the closest existing
# precedent for a nationwide UPRN point tileset at this scale.
#
# `uprn_pmtiles_data` (the wide-to-narrow attribute table) IS a normal,
# cheap target in this repo's DAG - it's just a select()/coalesce() pass
# over the already-built uprn_all_addresses table. Actually WRITING the
# GeoJSON and running tippecanoe over ~40M points is a genuinely heavy,
# one-off geodata job (same cost class as this repo's other big one-off
# builds - Azure geocoding, OSM pbf translation; see _targets.R's header
# and geocode_batch_runner.R) - build_uprn_pmtiles() is deliberately NOT
# called from any tar_target(), even though tar_source("pipeline/R") does
# source this file (defines the function, never runs it). Build it
# yourself when ready:
#
#   source("pipeline/R/pmtiles.R")
#   pmtiles_data <- targets::tar_read(uprn_pmtiles_data)
#   build_uprn_pmtiles(pmtiles_data, output_path = "output/uprn_pmtiles")

# ---------------------------------------------------------------------------
# Ported from the build repo (R/make_geojson.R)
# ---------------------------------------------------------------------------

# Write an sf object to GeoJSON for tippecanoe: transforms to WGS84 if
# needed, sets 6dp coordinate precision, overwrites any existing file.
# Warns above 15 columns (wide attribute tables bloat tiles).
make_geojson <- function(z, path = "outputs/zones.geojson") {
  if (ncol(z) > 15) {
    warning("Thats a lot of columns for the GeoJSON, are they all needed?")
  }
  if (file.exists(path)) {
    unlink(path)
  }
  if (!sf::st_is_longlat(z)) {
    z <- sf::st_transform(z, 4326)
  }
  sf::st_precision(z) <- 1000000
  sf::st_write(obj = z, dsn = path, delete_dsn = FALSE)
  path
}

# ---------------------------------------------------------------------------
# Ported from the build repo (R/pmtiles.R)
# ---------------------------------------------------------------------------

# Shells out to `tippecanoe` (via WSL on Windows) to build a PMTiles vector
# tileset from a GeoJSON file inside `output_path`. See the build repo's
# make_pmtiles() roxygen for the full parameter reference - unchanged here.
make_pmtiles <- function(input = NULL,
                         geojson = "school_locations.geojson",
                         pmtiles = "schools.pmtiles",
                         name = "schools", layer = name,
                         output_path = "outputdata",
                         attribution = "UniverstyofLeeds",
                         min_zoom = 6,
                         max_zoom = NA,
                         extend_zoom = FALSE,
                         coalesce = FALSE,
                         drop = FALSE,
                         shared_borders = FALSE,
                         max_tile_bytes = 5000000,
                         simplification = 10,
                         buffer = 5,
                         drop_rate = NA,
                         force = TRUE,
                         new_line_delim = TRUE) {
  if (!dir.exists(output_path)) {
    stop("'", output_path, "' does not exist as a writeable folder in ", getwd())
  }
  if (!file.exists(file.path(output_path, geojson))) {
    stop("'", geojson, "' does not exist")
  }
  if (file.exists(file.path(output_path, pmtiles))) {
    unlink(file.path(output_path, pmtiles))
  }

  command_tippecanoe <- paste(
    "tippecanoe -o", pmtiles,
    paste0("--name=", name),
    paste0("--layer=", layer),
    paste0("--attribution=", attribution),
    paste0("--minimum-zoom=", min_zoom),
    ifelse(is.na(max_zoom), "-zg", paste0("--maximum-zoom=", max_zoom)),
    paste0("--maximum-tile-bytes=", format(max_tile_bytes, scientific = FALSE)),
    ifelse(coalesce, "--coalesce-smallest-as-needed", ""),
    ifelse(drop, "--drop-densest-as-needed", ""),
    ifelse(shared_borders, "--detect-shared-borders", ""),
    ifelse(extend_zoom, "--extend-zooms-if-still-dropping", ""),
    paste0("--simplification=", simplification),
    paste0("--buffer=", buffer),
    ifelse(is.na(drop_rate), "", paste0("--drop-rate=", drop_rate)),
    ifelse(force, "--force", ""),
    ifelse(new_line_delim, "-P", ""),
    geojson,
    collapse = " "
  )

  if (.Platform$OS.type == "unix") {
    command_cd <- paste0("cd ", output_path)
    command_all <- paste(c(command_cd, command_tippecanoe), collapse = "; ")
  } else {
    dir <- getwd()
    command_start <- "bash -c "
    command_cd <- paste0("cd /mnt/", tolower(substr(dir, 1, 1)), substr(dir, 3, nchar(dir)), "/", output_path)
    command_all <- paste(c(command_cd, command_tippecanoe), collapse = "; ")
    command_all <- paste0(command_start, '"', command_all, '"')
  }
  responce <- system(command_all, intern = TRUE)

  if (file.exists(file.path(output_path, pmtiles))) {
    return(file.path(output_path, pmtiles))
  } else {
    stop(responce)
  }
}

# ---------------------------------------------------------------------------
# UPRN-specific attribute table + orchestration
# ---------------------------------------------------------------------------

# Best single address line for DISPLAY, independent of the match-quality
# semantics used elsewhere in this pipeline (match_quality/street_confidence
# etc. grade how much a MATCH should be trusted; this just picks the most
# complete text available so the pmtiles popup always shows something).
# Preference order, most to least complete:
#   1. best_address - a real EPC/Price-Paid/DEC/OSM-tag/2022-geocode address.
#   2. inferred house number + street (uprn_infill).
#   3. inferred street/building name alone (no house number).
#   4. the USRN's own inferred street name (no number, no building name).
# `source` records which of the four supplied it (for best_address, its own
# address_source flag - epc_domestic/price_paid/osm_uprn_tag/...; for the
# infill tiers, "infill_<address_source>" or "usrn_street_name_only") so the
# viewer can show provenance rather than presenting an inferred guess as a
# verified address.
best_display_address <- function(uprn_all_addresses) {
  d <- uprn_all_addresses
  infill_full <- trimws(paste(
    ifelse(is.na(d$infill_house_number), "", d$infill_house_number),
    ifelse(is.na(d$infill_street), "", d$infill_street)
  ))
  infill_full[infill_full == "" | is.na(d$infill_street)] <- NA_character_

  address <- dplyr::coalesce(d$best_address, infill_full, d$infill_building_name, d$usrn_street)
  source <- dplyr::case_when(
    !is.na(d$best_address) ~ d$best_address_source,
    !is.na(infill_full) ~ paste0("infill_", d$infill_address_source),
    !is.na(d$infill_building_name) ~ paste0("infill_", d$infill_address_source),
    !is.na(d$usrn_street) ~ "usrn_street_name_only",
    TRUE ~ NA_character_
  )
  list(address = address, source = source)
}

# The wide-to-narrow attribute table ready for make_geojson()/make_pmtiles():
# one row per UPRN with LONGITUDE/LATITUDE plus the fields Malcolm asked
# for - best address + its source, EPC rating, current (2025 nowcast)
# value - and a handful of extras (class, district, postcode, last sale
# details) useful for colouring/filtering in the viewer. Kept at 14 non-
# geometry columns, under make_geojson()'s 15-column warning threshold.
#
# The two EPC ratings are carried as SEPARATE fields and never coalesced:
# `epc_rating` is the domestic A-G band, `epc_asset_rating` the non-domestic
# NUMERIC asset rating (49, 56, ...). They are different scales measuring
# different things, so a single column would either mix letters with numbers
# or silently assert band thresholds this repo has no authority to set - see
# the block comment above epc_domestic_rating() in uprn_master.R. The viewer
# (docs/uprn_pmtiles.html) gives each its own colour mode accordingly.
build_uprn_pmtiles_data <- function(uprn_all_addresses) {
  d <- uprn_all_addresses
  disp <- best_display_address(d)

  out <- data.frame(
    UPRN = d$UPRN,
    LONGITUDE = d$LONGITUDE, LATITUDE = d$LATITUDE,
    address = disp$address, address_source = disp$source,
    class = d$class,
    epc_rating = d$epc_dom_rating,
    epc_asset_rating = d$epc_nondom_asset_rating,
    current_value = d$current_value_2025,
    last_sale_price = d$pp_price, last_sale_date = as.character(d$pp_date),
    district = dplyr::coalesce(d$district_nsul, d$infill_district),
    postcode = dplyr::coalesce(d$postcode_nsul, d$best_postcode, d$infill_postcode),
    exists = d$exists,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$LONGITUDE) & !is.na(out$LATITUDE), ]
  message(
    nrow(out), " UPRNs ready for the pmtiles export (",
    sum(!is.na(out$address)), " with a display address, ",
    sum(!is.na(out$epc_rating)), " with a domestic A-G EPC band, ",
    sum(!is.na(out$epc_asset_rating)), " with a non-domestic asset rating, ",
    sum(!is.na(out$current_value)), " with a current value estimate)."
  )
  # A zero domestic-band count almost certainly means the sibling EPC repo
  # renamed `cur_rate` again rather than that no UPRN has a certificate -
  # say so here rather than leaving an all-grey layer in the viewer as the
  # only symptom (which is exactly how the original bug hid).
  if (nrow(out) > 0 && sum(!is.na(out$epc_rating)) == 0) {
    warning(
      "No domestic EPC bands at all in the pmtiles data - check ",
      "epc_domestic_rating() in pipeline/R/uprn_master.R against the ",
      "current GB_domestic_epc.Rds schema."
    )
  }
  out
}

# Build the GeoJSON, then the PMTiles, for the full UPRN attribute table.
# See the file header for why this is a manually-invoked helper, not a
# tar_target(). `output_path` must already exist (make_pmtiles()'s own
# convention). Single-pass tippecanoe with `drop = TRUE`
# (--drop-densest-as-needed) + `extend_zoom = TRUE`, matching the build
# repo's own `pmtiles_uprn_unknown` target - the established precedent for
# a nationwide UPRN point tileset at this scale (~40M points).
build_uprn_pmtiles <- function(pmtiles_data,
                               output_path = "output/uprn_pmtiles",
                               geojson_name = "uprn_points.geojson",
                               pmtiles_name = "uprn_points.pmtiles",
                               min_zoom = 6, max_zoom = 15) {
  dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  pts <- sf::st_as_sf(pmtiles_data, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = TRUE)
  make_geojson(pts, file.path(output_path, geojson_name))
  make_pmtiles(
    geojson = geojson_name, pmtiles = pmtiles_name,
    name = "uprn", layer = "uprn",
    output_path = output_path,
    attribution = "Contains OS, EPC, HM Land Registry and Ordnance Survey data",
    min_zoom = min_zoom, max_zoom = max_zoom,
    extend_zoom = TRUE, drop = TRUE
  )
}
