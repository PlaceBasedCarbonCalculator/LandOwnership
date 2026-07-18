# 2026 Land Ownership pipeline. See the "2026 Land Ownership Pipeline
# Rebuild" plan (C:/Users/earmmor/.claude/plans/radiant-nibbling-dream.md)
# for the design rationale.
#
# Run with targets::tar_make(). This DAG stops at the geocode queue - it
# never calls the Azure Maps API. To actually geocode, source
# pipeline/R/geocode_batch_runner.R yourself and call
# run_geocode_batch(n, confirm = TRUE); rerunning tar_make() afterwards
# will pick up the new results (queue_file / azure_results_file are
# format = "file" targets that detect the change) and rebuild
# final_combined.

library(targets)

source("R/find_onedrive.R")
tar_source("pipeline/R") # pure function definitions only - never R/ (old scripts have top-level side effects)

onedrive <- find_onedrive()
inspire_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/INSPIRE/2026"

# Static open-data downloads used by the UPRN address-infill stage
# (pipeline/R/uprn_infill.R). Plain path constants like inspire_path -
# deliberately NOT format = "file" targets, because hashing a 30GB gpkg on
# every tar_make() costs minutes for files that only change when manually
# re-downloaded. Bump the filename/version here when a new release lands.
lids_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/os_uprn/lids-2026-06_csv_BLPU-UPRN-Street-USRN-11.zip"
usrn_geom_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/os_usrn/osopenusrn_202607_gpkg.zip"
# united-kingdom-latest.osm.pbf was misleadingly named: despite "latest" it
# was an OSM snapshot from 2024-04-12. Repointed 2026-07-18 at a freshly
# downloaded extract; every OSM consumer (building-address infill, road
# naming, substation matching - all via osmextract::oe_read(), see
# load_osm_road_names() in uprn_infill.R and the loaders in substations.R)
# reads straight from this pbf and lets oe_read() manage its own translated-
# gpkg cache (created next to the pbf on first use, one entry per layer -
# multipolygons/lines/points - reused by every later query against that
# layer). There's no separate gpkg path constant any more for the same
# reason. Bump this filename on future re-downloads.
osm_pbf_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/osm/united-kingdom-260717.osm.pbf"
# National Statistics UPRN Lookup: authoritative UPRN -> postcode + local
# authority for every E&W UPRN. Bump on new epoch downloads.
nsul_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/os_uprn/NSUL_E126_MAY_2026.zip"

# --- UPRN / address source data now owned by THIS repo (July 2026) ---
# The cleaned EPC registers are the outputs of the sibling EPC repo
# (F:/GitHub/PlaceBasedCarbonCalculator/EPC), written to inputdata/epc; the
# historical UPRN archive, Price Paid CSVs and the UBDC transaction->UPRN
# lookup were previously processed by the build repo, whose functions are
# ported into pipeline/R/price_paid.R and pipeline/R/uprn_historical.R.
# Plain path constants (not format = "file") for the same hashing-cost
# reason as above - bump the names when new releases land.
epc_dom_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/epc/GB_domestic_epc.Rds"
epc_nondom_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/epc/GB_nondomestic_epc.Rds"
dec_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/epc/dec_clean.Rds"
uprn_hist_zip_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/os_uprn/osopenuprn_2020_2025_all.zip"
price_paid_dir <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/house prices/land registry"
ubdc_zip_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/house prices/ppdid_uprn_usrn.zip"

tar_option_set(
  packages = c(
    "readr", "dplyr", "purrr", "stringr", "stringi", "sf", "readxl",
    "data.table", "plyr", "lwgeom", "stplanr", "future", "furrr",
    "progressr", "lubridate"
  )
)

list(
  # --- Stage 1: raw import (stable Title Number key, by-name columns) ---
  tar_target(ccod_2026_raw, import_ccod_raw(file.path(onedrive, "Land Registry/UK Ownership/CCOD_FULL_2026_07.zip"))),
  tar_target(ccod_2022_raw, import_ccod_raw(file.path(onedrive, "Land Registry/UK Ownership/CCOD_FULL_2022_07.zip"))),
  tar_target(ocod_2026_raw, import_ocod_raw(file.path(onedrive, "Land Registry/Overseas Ownership/OCOD_FULL_2026_07.zip"))),
  tar_target(ocod_2022_raw, import_ocod_raw(file.path(onedrive, "Land Registry/Overseas Ownership/OCOD_FULL_2022_07.zip"))),

  # --- Stage 2: diff vs 2022 (task 2) ---
  tar_target(ccod_diffed, diff_titles(ccod_2026_raw, ccod_2022_raw)),
  tar_target(ocod_diffed, diff_titles(ocod_2026_raw, ocod_2022_raw)),

  # --- Categorise by geocoding difficulty ---
  tar_target(ccod_freehold_categorised, categorise_ccod_freehold(ccod_diffed)),
  tar_target(ccod_leasehold_categorised, categorise_ccod_leasehold(ccod_diffed)),
  tar_target(ocod_categorised, categorise_ocod(ocod_diffed)),
  tar_target(
    all_categorised,
    {
      x <- dplyr::bind_rows(ccod_freehold_categorised, ccod_leasehold_categorised, ocod_categorised)
      x$orig_row_id <- seq_len(nrow(x)) # stable id used by audit_split_addresses() to trace parse failures
      x
    }
  ),

  # --- Stage 3: clean + split (task 3) ---
  tar_target(clean_strings_xlsx, "data/clean_strings.xlsx", format = "file"),
  tar_target(long_strings_xlsx, "data/long_strings.xlsx", format = "file"),
  tar_target(text_rem, readxl::read_excel(clean_strings_xlsx)),
  tar_target(long_text_rem, readxl::read_excel(long_strings_xlsx)),
  tar_target(split_result, build_split_addresses(all_categorised, text_rem, long_text_rem)),

  # --- Stage 4: audit (task 3) ---
  tar_target(cleaning_audit, audit_split_addresses(all_categorised, split_result)),

  # --- Carry forward addresses whose title didn't change (task 2) ---
  tar_target(results_2022, load_2022_final_results()),
  tar_target(
    carry_forward,
    carry_forward_unchanged(split_result[split_result$parse_ok, ], results_2022)
  ),

  # --- Stage 5: UPRN / address source datasets (owned by this repo) ---
  # Previously read ready-made from the build repo's store; now built here
  # from the raw inputs (see price_paid.R / uprn_historical.R - ported from
  # the build repo, same target names so it can later consume ours). Only
  # the generic statistical geographies still come from the sibling store.
  tar_target(ext_lookup_postcode_lsoa, load_lookup_postcode_lsoa()),
  tar_target(ext_bounds_lsoa_gb, load_bounds_lsoa_gb_full()),
  tar_target(ext_lsoa_admin, load_lsoa_admin()),

  tar_target(uprn_historical, load_uprn_historical(uprn_hist_zip_path)),
  tar_target(house_price_lr, load_lr_price_paid(price_paid_dir)),
  tar_target(house_prices_ubdc, load_ubdc_house_prices(ubdc_zip_path)),
  tar_target(
    house_price_lr_uprn,
    land_registry_add_uprn(
      house_price_lr, house_prices_ubdc, uprn_historical,
      ext_lookup_postcode_lsoa, ext_bounds_lsoa_gb,
      epc_dom_path, epc_nondom_path
    )
  ),
  tar_target(house_prices_nowcast, house_price_extrapolate(house_price_lr_uprn, ext_lsoa_admin)),
  tar_target(
    uprn_historical_epc_lr,
    combine_uprn_epc_lr(uprn_historical, house_prices_nowcast, epc_dom_path, epc_nondom_path)
  ),

  # Display Energy Certificates: extra UPRN address lines (public buildings
  # that hold a DEC but often no EPC). dec_clean.Rds has no postcode column,
  # so NSUL postcodes are attached before use (see epc_addresses.R).
  tar_target(dec_addresses_raw, load_dec_addresses(dec_path)),
  tar_target(dec_addresses, attach_nsul_postcode(dec_addresses_raw, uprn_places)),

  # --- Stage 6: free-source matching (tasks 5/6) ---
  tar_target(epc_lookup, build_epc_lookup(uprn_historical_epc_lr, dec_addresses)),
  tar_target(price_paid_lookup, build_price_paid_lookup(house_price_lr_uprn)),
  tar_target(building_lookup, build_building_lookup(uprn_historical_epc_lr, house_price_lr_uprn, dec_addresses)),

  # --- Stage 6b: UPRN address infill (OS Linked Identifiers + Open USRN + OSM) ---
  # See pipeline/R/uprn_infill.R. Everything inferred is flagged with
  # address_source / number_source / number_guessed - gap-guessed house
  # numbers are never treated as better than "guess" quality.
  tar_target(uprn_usrn, load_uprn_usrn_lookup(lids_path)),
  tar_target(usrn_geom, load_usrn_geometry(usrn_geom_path)),
  tar_target(osm_addresses, load_osm_building_addresses(osm_pbf_path)),
  tar_target(osm_road_names, load_osm_road_names(osm_pbf_path)),
  tar_target(la_bounds_file, "data/la_bounds.geojson", format = "file"),
  tar_target(
    known_uprn_addresses_base,
    build_known_uprn_addresses(uprn_historical_epc_lr, house_price_lr_uprn, dec_addresses)
  ),
  # 2022 precise-address geocodes (entityType "Address") whose point lands
  # on/next to an addressless UPRN donate their LR address line to it, so
  # the infill starts from a more complete known-address table. Flagged
  # address_source = "geocode_2022".
  tar_target(
    known_uprn_addresses,
    augment_known_uprn_addresses_2022(known_uprn_addresses_base, results_2022, uprn_historical_epc_lr)
  ),
  # --- coverage checks (see pipeline/R/audit_uprn_coverage.R) ---
  # Cheap read-only summaries at each key stage: house-number/street/
  # postcode coverage, and postcode well-formedness (is_valid_postcode(),
  # utils.R). Expectation is that coverage climbs stage over stage and
  # almost every UPRN is addressed by uprn_all_addresses_coverage at the end.
  tar_target(known_uprn_addresses_coverage, audit_known_uprn_addresses(known_uprn_addresses)),
  tar_target(postcode_district, build_postcode_district_lookup(ccod_2026_raw, ocod_2026_raw)),

  # NSUL: authoritative per-UPRN postcode + district (LR-spelling via
  # build_lad_district_lookup). Feeds street naming, infill enrichment,
  # gap-guess validation and the postcode-singleton stage.
  tar_target(nsul, load_nsul(nsul_path)),
  tar_target(nsul_lad_names, load_nsul_lad_names(nsul_path)),
  tar_target(lad_district, build_lad_district_lookup(nsul, postcode_district, nsul_lad_names)),
  tar_target(uprn_places, build_uprn_places(nsul, lad_district)),

  tar_target(
    usrn_street_names,
    build_usrn_street_names(
      uprn_usrn, known_uprn_addresses, postcode_district,
      usrn_geom, osm_road_names, la_bounds_file, uprn_places
    )
  ),
  tar_target(usrn_street_names_coverage, audit_usrn_street_names(uprn_usrn, usrn_street_names)),
  # Infill is broken into one target per stage (OSM building match, USRN
  # street name, gap-guessed house number) so tar_make() only reruns the
  # stage(s) whose own inputs changed, instead of redoing all three (the OSM
  # join and the gap-guess loop are the expensive ones) every time.
  tar_target(infill_candidates, build_infill_candidates(uprn_historical_epc_lr, known_uprn_addresses)),
  tar_target(infill_un_usrn, build_infill_un_usrn(infill_candidates, uprn_usrn)),
  tar_target(infill_osm_addresses, build_infill_osm_addresses(infill_candidates, osm_addresses)),
  tar_target(infill_usrn_street, build_infill_usrn_street(infill_un_usrn, usrn_street_names)),
  tar_target(
    infill_gap_guess,
    build_infill_gap_guess(infill_un_usrn, known_uprn_addresses, uprn_usrn, usrn_geom)
  ),
  tar_target(
    uprn_infill,
    build_uprn_infill(
      infill_candidates, infill_osm_addresses, infill_usrn_street, infill_gap_guess,
      infill_un_usrn, usrn_street_names, postcode_district, uprn_places
    )
  ),
  tar_target(uprn_infill_coverage, audit_uprn_infill(infill_candidates, uprn_infill)),
  tar_target(street_lookup, build_street_lookup(known_uprn_addresses, postcode_district)),
  tar_target(infill_lookup, build_infill_lookup(uprn_infill)),
  tar_target(postcode_singleton_lookup, build_postcode_singleton_lookup(nsul, uprn_historical)),
  tar_target(street_centroid_lookup, build_street_centroid_lookup(usrn_street_names, usrn_geom)),
  tar_target(
    street_centroid_postcode_lookup,
    build_street_centroid_postcode_lookup(usrn_street_names, usrn_geom)
  ),

  # --- Stage 6e: electricity substation matching (see pipeline/R/substations.R) ---
  # OSM's own power=substation tagging + the UPRN dataset, used to resolve
  # substation titles to a precise point instead of a street/district
  # fallback - see match_free_sources() below. osm_substation_polygons
  # queries the pbf's "multipolygons" layer, already translated by
  # osm_addresses above (oe_read() caches per layer, so this doesn't pay a
  # second translation cost); osm_substation_points translates the "points"
  # layer, not otherwise used elsewhere in this pipeline.
  tar_target(osm_substation_points, load_osm_substation_points(osm_pbf_path)),
  tar_target(osm_substation_polygons, load_osm_substation_polygons(osm_pbf_path)),
  tar_target(
    substation_uprn_lookup,
    build_substation_uprn_lookup(osm_substation_points, osm_substation_polygons, uprn_historical)
  ),
  tar_target(
    substation_lookup,
    build_substation_lookup(substation_uprn_lookup, uprn_places, uprn_usrn, usrn_street_names)
  ),

  # --- Stage 6c: recover UPRNs for Price Paid transactions the early
  # UBDC/EPC matching missed ---
  # land_registry_add_uprn() (house_price_lr_uprn) only has the UBDC
  # linkage - which stops at 2022 - and an exact-string EPC match to work
  # with. Now that the fuller address infrastructure above exists (known
  # addresses, street/infill/postcode-singleton lookups), re-run the same
  # free-matching approach against Price Paid's own structured PAON/Street/
  # District columns. See pipeline/R/price_paid.R.
  tar_target(
    house_price_lr_rematch,
    rematch_price_paid_unmatched(
      house_price_lr_uprn, postcode_district,
      epc_lookup, price_paid_lookup, building_lookup,
      street_lookup, infill_lookup, postcode_singleton_lookup
    )
  ),
  # How much Price Paid data has (still) got no UPRN, by year - expect a
  # visible drop from 2023 onwards (UBDC coverage ends 2022) that the
  # rematch pass only partly closes. See pipeline/R/audit_price_paid.R.
  tar_target(
    house_price_lr_match_report,
    audit_price_paid_uprn_match(house_price_lr_uprn, house_price_lr_rematch)
  ),

  # --- Stage 6d: fold the recovered UPRNs into a second, fuller pass ---
  # house_price_lr_rematch only carries the *change* (what it recovered +
  # what's still unmatched); house_price_lr_final adds that back onto the
  # originally-matched rows to get the full Price Paid picture, which then
  # reruns the price-derived UPRN attributes - nowcast, domestic/
  # non-domestic classification, known addresses - a second time so the
  # published uprn_all_addresses table reflects them. This has to be a
  # second pass with its own *_final targets rather than updating
  # house_prices_nowcast/uprn_historical_epc_lr/known_uprn_addresses in
  # place: those three feed street_lookup/infill_lookup/epc_lookup, which
  # house_price_lr_rematch itself depends on - looping the rematch output
  # back into them would be a circular dependency. See
  # combine_price_paid_rematch() in pipeline/R/price_paid.R.
  tar_target(
    house_price_lr_final,
    combine_price_paid_rematch(
      house_price_lr_uprn, house_price_lr_rematch,
      uprn_historical, ext_bounds_lsoa_gb
    )
  ),
  tar_target(house_prices_nowcast_final, house_price_extrapolate(house_price_lr_final, ext_lsoa_admin)),
  tar_target(
    uprn_historical_epc_lr_final,
    combine_uprn_epc_lr(uprn_historical, house_prices_nowcast_final, epc_dom_path, epc_nondom_path)
  ),
  tar_target(
    known_uprn_addresses_base_final,
    build_known_uprn_addresses(uprn_historical_epc_lr_final, house_price_lr_final, dec_addresses)
  ),
  tar_target(
    known_uprn_addresses_final,
    augment_known_uprn_addresses_2022(known_uprn_addresses_base_final, results_2022, uprn_historical_epc_lr_final)
  ),
  # Same coverage check as known_uprn_addresses_coverage, run again on the
  # _final version so the gain from the rematch pass is visible as a number.
  tar_target(known_uprn_addresses_final_coverage, audit_known_uprn_addresses(known_uprn_addresses_final)),

  tar_target(
    free_match,
    match_free_sources(
      carry_forward$needs_geocode, epc_lookup, price_paid_lookup,
      building_lookup = building_lookup, street_lookup = street_lookup,
      infill_lookup = infill_lookup, substation_lookup = substation_lookup,
      postcode_singleton_lookup = postcode_singleton_lookup,
      street_centroid_lookup = street_centroid_lookup,
      street_centroid_postcode_lookup = street_centroid_postcode_lookup
    )
  ),
  tar_target(
    substation_match_coverage,
    audit_substation_matches(
      osm_substation_points, osm_substation_polygons,
      substation_uprn_lookup, carry_forward$needs_geocode, free_match
    )
  ),

  # --- Stage 7: INSPIRE <-> UPRN lookup (task 7) ---
  tar_target(inspire_clean, load_inspire_clean(inspire_path)),
  tar_target(uprn_inspire_lookup, build_uprn_inspire_lookup(inspire_clean, uprn_historical)),

  # --- Stage 7b: master UPRN address table (published output) ---
  # Every known UPRN with all available address data side by side - EPC,
  # DEC, Price Paid, NSUL, best/parsed line, inferred street, USRN and
  # INSPIRE parcel. See pipeline/R/uprn_master.R. Uses the Stage 6d *_final
  # versions (uprn_historical_epc_lr_final / house_price_lr_final /
  # known_uprn_addresses_final) so the published table includes the Price
  # Paid transactions the rematch pass recovered - uprn_infill is left as
  # the original (Stage 6b) version since it's only a fallback for UPRNs
  # that still have no real address; build_uprn_all_addresses() prefers
  # best_address (now sourced from known_uprn_addresses_final) over
  # infill_street wherever both are present, so a UPRN that gained a real
  # address here simply makes its stale infill guess redundant, not wrong.
  tar_target(
    uprn_all_addresses,
    build_uprn_all_addresses(
      uprn_historical, uprn_historical_epc_lr_final, dec_addresses,
      house_price_lr_final, known_uprn_addresses_final, uprn_infill,
      uprn_usrn, usrn_street_names, uprn_places, uprn_inspire_lookup
    )
  ),
  tar_target(
    uprn_all_addresses_file,
    save_uprn_all_addresses(uprn_all_addresses),
    format = "file"
  ),
  # Headline coverage check: by this point almost every UPRN should have a
  # real or inferred address (see audit_uprn_all_addresses()).
  tar_target(uprn_all_addresses_coverage, audit_uprn_all_addresses(uprn_all_addresses)),

  # --- Stage 8: geocode queue (tasks 2/4) ---
  tar_target(queue_built, build_geocode_queue(free_match$unmatched)),
  # format = "file": re-reads whenever run_geocode_batch() (run manually,
  # outside this DAG) updates row statuses on disk.
  tar_target(queue_file, { force(queue_built); "data/geocoding/queue.rds" }, format = "file"),
  tar_target(queue_current, readRDS(queue_file)),

  # --- Stages 10-11: UPRN snap + final combine ---
  # ensure_azure_results_file() only creates an empty placeholder if
  # nothing has been geocoded yet - it never calls Azure.
  tar_target(azure_results_file, ensure_azure_results_file(), format = "file"),
  tar_target(azure_results, readRDS(azure_results_file)),
  tar_target(
    final_combined,
    combine_final(
      carry_forward$carried_forward, free_match$matched, azure_results,
      queue_current, uprn_historical, uprn_inspire_lookup
    )
  )
)
