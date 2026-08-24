# PMTiles exports for the Land Ownership Explorer.
#
# This file builds THREE tilesets, all of them proper tar_targets:
#
#   uprn_points.pmtiles  every known UPRN + best address, EPC, 2025 value
#   landowners.pmtiles   CCOD/OCOD land titles as located points
#   inspire.pmtiles      cleaned INSPIRE freehold parcels
#
# The last two replace the tilesets on carbon.place that were built by hand
# from the pre-2026 scripts (R/prep_land_registry_alt.R and
# R/map_geocoded_data.R) and have been frozen at a 2022 snapshot ever since.
# That freeze is exactly why they are targets now rather than hand-run
# scripts: nothing about them was reproducible, so nothing about them got
# refreshed when the pipeline behind them was rebuilt.
#
# GeoJSON/PMTiles machinery (make_geojson(), make_pmtiles(), join_pmtiles())
# is PORTED, near-verbatim, from the sibling build repo's R/make_geojson.R
# and R/pmtiles.R rather than re-implemented - same tippecanoe-via-WSL
# convention (tippecanoe must be installed; on Windows this runs it inside
# WSL, so WSL + tippecanoe both need to be present). The point-tileset
# options below (single-pass, `drop = TRUE`, `extend_zoom = TRUE`) mirror
# the build repo's own `pmtiles_uprn_unknown` target - the closest existing
# precedent for a nationwide UPRN point tileset at this scale.
#
# These are heavy targets: each writes a multi-GB GeoJSON and then runs
# tippecanoe over tens of millions of features, so a full rebuild is hours,
# not minutes. They sit at the end of the DAG and nothing depends on them,
# so `tar_make(names = ...)` them individually when you want them.

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
                         new_line_delim = TRUE,
                         date = format(Sys.Date(), "%Y%m%d")) {
  # Date-stamp the output, as the build repo does, so a redeploy never
  # overwrites the tileset the site is currently serving: the new file goes up
  # under a new name, the reference is bumped, and the old one stays as a
  # rollback. Pass date = NA for an unstamped name.
  if (!is.na(date)) {
    pmtiles <- sub("\\.pmtiles$", paste0("_", date, ".pmtiles"), pmtiles)
  }
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
    # Single-quoted: the whole command is later wrapped in double quotes for
    # `bash -c "..."`, and an unquoted attribution containing shell
    # metacharacters breaks the parse before tippecanoe ever runs. The INSPIRE
    # attribution ends "(Crown copyright)", and bash treats a bare "(" as a
    # syntax error - the command failed with no useful message after the
    # GeoJSON had already been written.
    paste0("--attribution='", attribution, "'"),
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

# Merge several PMTiles files into one with tile-join (part of tippecanoe,
# via WSL on Windows). Used to combine the per-zoom-band INSPIRE tilesets
# into a single archive. Ported from the build repo's join_pmtiles().
join_pmtiles <- function(output = "inspire.pmtiles",
                         inputs = c("inspire_large.pmtiles", "inspire_medium.pmtiles", "inspire_all.pmtiles"),
                         output_path = "outputdata",
                         date = format(Sys.Date(), "%Y%m%d")) {
  if (!is.na(date)) {
    output <- sub("\\.pmtiles$", paste0("_", date, ".pmtiles"), output)
  }
  if (!dir.exists(output_path)) {
    stop("'", output_path, "' does not exist as a writeable folder in ", getwd())
  }
  for (i in seq_along(inputs)) {
    if (!file.exists(file.path(output_path, inputs[i]))) {
      stop("'", inputs[i], "' does not exist")
    }
  }
  if (file.exists(file.path(output_path, output))) {
    unlink(file.path(output_path, output))
  }

  command_tippecanoe <- paste("tile-join -o", output, "-pk --force",
                              paste(inputs, collapse = " "), collapse = " ")

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

  if (file.exists(file.path(output_path, output))) {
    return(file.path(output_path, output))
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
# `output_path` is created if absent (make_pmtiles() itself insists it
# already exists). Single-pass tippecanoe with `drop = TRUE`
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


# ---------------------------------------------------------------------------
# INSPIRE parcels: UPRN counts, price per square metre, and the tileset
# ---------------------------------------------------------------------------

# Attribute table for the INSPIRE parcel tileset.
#
# `uprn_inspire_lookup` already holds the expensive part - the point-in-polygon
# join of every historical UPRN into its parcel - so this stage never repeats
# it. It re-counts that same lookup restricted to UPRNs that are still ACTIVE,
# then attaches value data to the parcels where that count is exactly one.
#
# Active, not merely known. `uprn_inspire_lookup$n_uprn` counts every UPRN ever
# seen in the OS archive since 2020, including ones since retired, so a plot
# whose two flats were knocked through into one house still counts as 2 and
# would be excluded from the price-per-m2 calculation for a reason that stopped
# being true years ago. `exists` (carried through uprn_pmtiles_data from
# uprn_historical, TRUE only for UPRNs present in the most recent release) is
# the flag that answers "is this address a current property".
#
# The point of the 0/1/2+ split: a parcel containing exactly one active UPRN is
# one property on one piece of land, so the parcel's geometry IS that
# property's plot and its area is that property's plot size. That is the only
# case where dividing a property value by a parcel area produces a meaningful
# number, which is why `price_per_m2` is populated for those parcels alone
# rather than for every parcel with a value somewhere inside it.
#
# Parcels with zero active UPRNs are the interesting residual: bare land,
# garden and access strips, car parks, demolished sites, and parcels whose
# UPRNs sit just outside the mapped boundary.
build_inspire_map_data <- function(inspire_clean, uprn_inspire_lookup, uprn_pmtiles_data) {
  lookup <- data.table::as.data.table(uprn_inspire_lookup[, c("UPRN", "INSPIREID")])
  uprn <- data.table::as.data.table(
    uprn_pmtiles_data[, c("UPRN", "exists", "current_value")]
  )

  # exists is NA for UPRNs the master table never classified; treat those as
  # not-active rather than silently counting them, so "active" means one thing.
  uprn <- uprn[!is.na(exists) & exists]

  active <- merge(lookup, uprn, by = "UPRN", all = FALSE)

  counts <- active[, .(n_uprn_active = .N), by = INSPIREID]

  # The single active UPRN's value, for the parcels that have exactly one.
  single_ids <- counts$INSPIREID[counts$n_uprn_active == 1]
  singles <- active[INSPIREID %in% single_ids,
                    .(INSPIREID, single_uprn = UPRN, single_uprn_value = current_value)]

  out <- inspire_clean[, c("INSPIREID", "local_authority", "area")]
  out <- merge(out, as.data.frame(counts), by = "INSPIREID", all.x = TRUE, sort = FALSE)
  out <- merge(out, as.data.frame(singles), by = "INSPIREID", all.x = TRUE, sort = FALSE)

  out$n_uprn_active[is.na(out$n_uprn_active)] <- 0L

  # A three-level category rather than a raw count, because that is what the
  # map colours by and what the count actually means (see above). Kept as text
  # so the tool can use a `match` expression and the popup can show it as-is.
  out$uprn_class <- ifelse(out$n_uprn_active == 0, "0",
                    ifelse(out$n_uprn_active == 1, "1", "2+"))

  # Price per square metre of plot, single-active-UPRN parcels only. Guarded
  # against zero/degenerate areas, which cleaning can leave behind.
  out$price_per_m2 <- NA_real_
  one <- out$n_uprn_active == 1 & !is.na(out$single_uprn_value) &
    !is.na(out$area) & out$area > 0
  out$price_per_m2[one] <- round(out$single_uprn_value[one] / out$area[one])

  out$area <- round(out$area)
  out$single_uprn <- as.character(out$single_uprn)
  out$single_uprn_value <- round(out$single_uprn_value)

  message(
    nrow(out), " INSPIRE parcels: ",
    sum(out$n_uprn_active == 0), " with no active UPRN, ",
    sum(out$n_uprn_active == 1), " with exactly one, ",
    sum(out$n_uprn_active > 1), " with several; ",
    sum(!is.na(out$price_per_m2)), " with a price per m2."
  )
  out
}

# Build the INSPIRE parcel tileset.
#
# ~24M polygons is far too many to serve at every zoom, so - as the pre-2026
# hand-built tileset did - the parcels are tiled in three passes by size and
# the results tile-joined: only the very large holdings survive when zoomed
# out, everything appears once you are close enough for it to mean something.
# The thresholds are the original script's, in acres, because that is the unit
# land is discussed in.
build_inspire_pmtiles <- function(inspire_map_data,
                                  output_path = "output/inspire_pmtiles",
                                  pmtiles_name = "inspire.pmtiles",
                                  large_m2 = 404686,   # 100 acres
                                  medium_m2 = 40468) { # 10 acres
  dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

  levels <- list(
    list(nm = "large",  keep = inspire_map_data$area > large_m2,  min = 6,  max = 9),
    list(nm = "medium", keep = inspire_map_data$area > medium_m2, min = 10, max = 12),
    list(nm = "all",    keep = rep(TRUE, nrow(inspire_map_data)), min = 13, max = 15)
  )

  built <- character(0)
  for (lv in levels) {
    gj <- paste0("inspire_", lv$nm, ".geojson")
    pm <- paste0("inspire_", lv$nm, ".pmtiles")
    message(Sys.time(), " INSPIRE ", lv$nm, ": ", sum(lv$keep), " polygons")
    make_geojson(inspire_map_data[lv$keep, ], file.path(output_path, gj))
    built <- c(built, basename(make_pmtiles(
      geojson = gj, pmtiles = pm,
      name = "inspire", layer = "inspire",
      output_path = output_path,
      attribution = "Contains HM Land Registry INSPIRE and OS data (Crown copyright)",
      min_zoom = lv$min, max_zoom = lv$max,
      shared_borders = TRUE, coalesce = TRUE, drop = TRUE
    )))
  }

  join_pmtiles(pmtiles_name, built, output_path = output_path)
}


# ---------------------------------------------------------------------------
# Landowner points
# ---------------------------------------------------------------------------

# Attribute table for the landowner point tileset, from final_combined.
#
# Field names follow the pre-2026 tileset where the meaning is unchanged
# (Category, Tenure, Country) so the existing tool keeps working. The old
# Bing-specific `geocode_type` is gone, because the 2026 pipeline locates most
# titles by matching them to a UPRN rather than by geocoding an address string,
# and "how precise was the geocoder" is no longer the question worth asking.
# `match_quality` answers the question that does matter - how much to trust
# this dot - across all the location sources. Which source located it is
# deliberately not published: it is a pipeline-internal detail that means
# nothing to a map reader. `final_combined$source` still records it for
# auditing.
build_landowner_pmtiles_data <- function(final_combined) {
  d <- data.table::as.data.table(final_combined)
  d <- d[!is.na(latitude) & !is.na(longitude)]

  # Tenure is the Land Registry's own Freehold/Leasehold flag, carried through
  # from the raw CCOD/OCOD rows by combine_results.R. It used to be derived
  # from `dataset` here, which labelled every OCOD title "Overseas owned" - not
  # a tenure at all, and it hid whether those titles were freehold or
  # leasehold. Overseas ownership is still readable from Country.
  tenure <- as.character(d$tenure)
  tenure[is.na(tenure) | tenure == ""] <- "Unknown"

  out <- data.frame(
    title_number = d$title_number,
    LONGITUDE = as.numeric(d$longitude),
    LATITUDE = as.numeric(d$latitude),
    Proprietor = d$proprietor_name,
    Category = d$proprietorship_category,
    Tenure = tenure,
    Country = d$country_incorporated,
    address = d$property_address,
    district = d$district,
    postcode = d$postcode,
    # Source data mixes "High"/"high", "Medium"/"medium" and so on depending on
    # which matcher produced the row; a single case means one legend entry
    # instead of two identical ones.
    match_quality = tools::toTitleCase(tolower(as.character(d$match_quality))),
    uprn = as.character(d$uprn),
    stringsAsFactors = FALSE
  )

  message(
    nrow(out), " located land titles ready for the pmtiles export (",
    sum(!is.na(out$uprn)), " matched to a UPRN)."
  )
  out
}

# Build the landowner point tileset. Same single-pass point-tiling options as
# the UPRN layer.
build_landowner_pmtiles <- function(landowner_pmtiles_data,
                                    output_path = "output/landowner_pmtiles",
                                    geojson_name = "landowners.geojson",
                                    pmtiles_name = "landowners.pmtiles",
                                    min_zoom = 6, max_zoom = 15) {
  dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  pts <- sf::st_as_sf(landowner_pmtiles_data, coords = c("LONGITUDE", "LATITUDE"),
                      crs = 4326, remove = TRUE)
  make_geojson(pts, file.path(output_path, geojson_name))
  make_pmtiles(
    geojson = geojson_name, pmtiles = pmtiles_name,
    name = "landowners", layer = "landowners",
    output_path = output_path,
    attribution = "Contains HM Land Registry CCOD/OCOD and OS data (Crown copyright)",
    min_zoom = min_zoom, max_zoom = max_zoom,
    extend_zoom = TRUE, drop = TRUE
  )
}
