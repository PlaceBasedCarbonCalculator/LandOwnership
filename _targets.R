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
#
# Three logically separate streams run through this DAG:
#   Stream 1 - build the best possible UPRN/address database
#     (known_uprn_addresses, uprn_infill, and the lookups/fuzzy_lookup
#     derived from them). Closes after ONE pass - nothing downstream ever
#     feeds address data back into it, so there's never any ambiguity about
#     which version of "the database" a matcher is running against.
#   Stream 2 - match CCOD/OCOD land-title address strings against Stream 1
#     (match_free_sources.R's exact cascade, then fuzzy_match.R's fuzzy
#     fallback - both against Stream 1's single set of lookups/fuzzy_lookup).
#   Stream 3 - enrich the database with Price Paid data, which itself
#     requires matching Price Paid's own unmatched transactions against
#     Stream 1 (house_price_lr_rematch, reusing fuzzy_lookup - see Stage 6c/
#     6d below). Only Stream 3's price/classification columns are folded
#     back into the published uprn_all_addresses table (Stage 7b); its
#     matching never rebuilds Stream 1's address data.

library(targets)

source("R/find_onedrive.R")
tar_source("pipeline/R") # pure function definitions only - never R/ (old scripts have top-level side effects)

onedrive <- find_onedrive()
inspire_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/INSPIRE/2026"
# The sibling build repo's own _targets store, already holding
# bounds_postcodes_2015/2020/2024 (OS Postcode Polygons) for its own
# purposes - read cross-store via targets::tar_read() in
# build_postcode_history_lookup() (pipeline/R/postcode_history.R) rather
# than re-downloading/re-parsing them here. Never rebuilt by THIS repo's
# pipeline - if it's missing those targets, run tar_make() there first.
build_repo_targets_store <- "F:/GitHub/PlaceBasedCarbonCalculator/build/_targets"

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
# OS Open Map Local: `road` layer distinctive_name/classification, linked to
# USRN geometry (pipeline/R/open_map_local.R) BEFORE OSM road naming is
# tried - see build_usrn_street_names() in uprn_infill.R. Single ~8GB gpkg
# inside the zip, read via /vsizip/ like usrn_geom_path above - same
# not-format="file" reasoning as the other big constants here.
oml_zip_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/os_openmaplocal/opmplc_gpkg_gb_20260401.zip"
# National Statistics UPRN Lookup: authoritative UPRN -> postcode + local
# authority for every E&W UPRN. Bump on new epoch downloads.
nsul_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/os_uprn/NSUL_E126_MAY_2026.zip"
# Two small official open-data extras (see pipeline/R/other_uprn_sources.R):
# London's Cultural Infrastructure Map (GLA, 2023) and DfE's GIAS schools
# extract, both UPRN-keyed. Small enough (<10MB) that format = "file" is
# cheap, unlike the constants above - so these are declared as targets below
# rather than plain path strings.
cultural_venues_gpkg_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/os_uprn/other_uprn_sources/cultural_venues_in_GIS_format.gpkg"
education_establishments_zip_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/os_uprn/other_uprn_sources/education_establishments.zip"

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
    "progressr", "lubridate", "stringdist", "RANN"
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

  # Open Map Local road layer, linked to USRN geometry (see
  # pipeline/R/open_map_local.R) - tried for street naming/classification
  # ahead of the OSM fallback above (usrn_street_names target below).
  tar_target(oml_roads, load_open_map_local_roads(oml_zip_path)),
  tar_target(usrn_oml_link, link_usrn_oml(usrn_geom, oml_roads)),

  # Direct UPRN tagging in OSM (ref:gb:uprn, ~6M objects nationally per
  # taginfo - see pipeline/R/osm_uprn.R). Separate from osm_addresses above:
  # that's a proximity join (a UPRN point falling inside an addr-tagged
  # building); this is an exact UPRN->object crosswalk via the object's own
  # tag, so it's fed into known_uprn_addresses as a real address source
  # ("osm_uprn_tag") rather than the infill fallback stage.
  tar_target(osm_uprn_tags, load_osm_uprn_tags(osm_pbf_path)),
  tar_target(osm_uprn_addresses, build_osm_uprn_addresses(osm_uprn_tags)),
  tar_target(osm_uprn_coverage, audit_osm_uprn_tags(osm_uprn_tags, uprn_historical)),

  # Two further official, UPRN-keyed address sources (see
  # pipeline/R/other_uprn_sources.R) - same "real address, not an infill
  # guess" treatment as osm_uprn_addresses above.
  tar_target(cultural_venues_gpkg, cultural_venues_gpkg_path, format = "file"),
  tar_target(education_establishments_zip, education_establishments_zip_path, format = "file"),
  tar_target(cultural_venue_addresses_raw, load_cultural_venue_addresses(cultural_venues_gpkg)),
  # ~68% of cultural_venue_addresses_raw rows have no postcode in the source
  # data at all - filled from NSUL via uprn_places (built further below;
  # tar_make() resolves the dependency regardless of list order).
  tar_target(cultural_venue_addresses, fill_missing_postcode_from_nsul(cultural_venue_addresses_raw, uprn_places)),
  tar_target(education_establishment_addresses, load_education_establishment_addresses(education_establishments_zip)),

  tar_target(
    known_uprn_addresses_base,
    build_known_uprn_addresses(
      uprn_historical_epc_lr, house_price_lr_uprn, dec_addresses,
      osm_uprn_addresses = osm_uprn_addresses,
      education_addresses = education_establishment_addresses,
      cultural_venue_addresses = cultural_venue_addresses
    )
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
      usrn_geom, osm_road_names, la_bounds_file, uprn_places,
      oml_link = usrn_oml_link
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
  # The single, closed Stream-1 address lookup (see header note above) used
  # for fuzzy free-text matching by BOTH Stream 2 (CCOD/OCOD - fuzzy_match,
  # Stage 6f below) and Stream 3 (Price Paid rematch - house_price_lr_rematch,
  # Stage 6c below). Built directly from known_uprn_addresses/uprn_infill/
  # uprn_places rather than uprn_all_addresses so it's available before
  # Stream 3 runs - uprn_all_addresses is only published after Stream 3
  # completes (Stage 7b), and Stream 3 needs a lookup to match against.
  tar_target(fuzzy_lookup, build_fuzzy_lookup(known_uprn_addresses, uprn_infill, uprn_places)),
  # Historical postcode-polygon centroids (see pipeline/R/postcode_history.R)
  # - a last-resort geographic fallback for fuzzy_match.R when a title's own
  # postcode text doesn't match any UPRN at that house number. Only depends
  # on the (constant) sibling-repo store path, so it caches like the other
  # static-data targets above.
  tar_target(postcode_history_lookup, build_postcode_history_lookup(build_repo_targets_store)),
  tar_target(street_centroid_lookup, build_street_centroid_lookup(usrn_street_names, usrn_geom)),
  tar_target(
    street_centroid_postcode_lookup,
    build_street_centroid_postcode_lookup(usrn_street_names, usrn_geom)
  ),
  # Real postcode-POLYGON intersections (current 2024 + historical 2020/2015
  # boundaries), precise enough for a road that spans several postcodes and
  # able to resolve a stale postcode/street combination from an older title
  # - see pipeline/R/postcode_history.R. Tried ahead of the majority-string
  # approximation above in match_free_sources().
  tar_target(
    street_postcode_boundary_lookup,
    build_street_postcode_boundary_lookup(usrn_geom, usrn_street_names, build_repo_targets_store)
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

  # --- Stage 6c: Stream 3 - recover UPRNs for Price Paid transactions the
  # early UBDC/EPC matching missed ---
  # land_registry_add_uprn() (house_price_lr_uprn) only has the UBDC
  # linkage - which stops at 2022 - and an exact-string EPC match to work
  # with. This is Stream 3's matching step: it matches Price Paid's own
  # structured PAON/Street/postcode/Local-Authority columns against the same
  # closed Stream-1 fuzzy_lookup that Stream 2 (CCOD/OCOD) matches against
  # below, plus the postcode-singleton fallback for rows with no usable
  # street text. See pipeline/R/price_paid.R.
  tar_target(
    house_price_lr_rematch,
    rematch_price_paid_unmatched(
      house_price_lr_uprn, fuzzy_lookup, postcode_singleton_lookup
    )
  ),
  # How much Price Paid data has (still) got no UPRN, by year - expect a
  # visible drop from 2023 onwards (UBDC coverage ends 2022) that the
  # rematch pass only partly closes. See pipeline/R/audit_price_paid.R.
  tar_target(
    house_price_lr_match_report,
    audit_price_paid_uprn_match(house_price_lr_uprn, house_price_lr_rematch)
  ),

  # --- Stage 6d: Stream 3 continued - fold the recovered UPRNs into the
  # price/classification enrichment (NOT the address database) ---
  # house_price_lr_rematch only carries the *change* (what it recovered +
  # what's still unmatched); house_price_lr_final adds that back onto the
  # originally-matched rows to get the full Price Paid picture. This in turn
  # reruns the nowcast and domestic/non-domestic classification so the
  # published uprn_all_addresses table's price/class columns reflect the
  # fuller Price Paid coverage. Deliberately does NOT rebuild
  # known_uprn_addresses/uprn_infill a second time: every rematch-recovered
  # row was, by construction, matched via a fuzzy_lookup key that already
  # existed in known_uprn_addresses/uprn_infill (Stream 1, closed after one
  # pass - see the header note above), so its own address text adds nothing
  # a matching consumer doesn't already have. house_prices_nowcast_final/
  # uprn_historical_epc_lr_final are cheap to rerun (no dependency on
  # known_uprn_addresses at all) and are the only *_final targets kept for
  # this reason. See combine_price_paid_rematch() in pipeline/R/price_paid.R.
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
    free_match,
    match_free_sources(
      carry_forward$needs_geocode, epc_lookup, price_paid_lookup,
      building_lookup = building_lookup, street_lookup = street_lookup,
      infill_lookup = infill_lookup, substation_lookup = substation_lookup,
      postcode_singleton_lookup = postcode_singleton_lookup,
      street_centroid_lookup = street_centroid_lookup,
      street_centroid_postcode_lookup = street_centroid_postcode_lookup,
      street_postcode_boundary_lookup = street_postcode_boundary_lookup
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
  # INSPIRE parcel. See pipeline/R/uprn_master.R. Price/classification
  # columns come from the Stage 6d *_final versions (uprn_historical_epc_lr_final /
  # house_price_lr_final) so the published table's price and domestic/
  # non-domestic columns reflect the Price Paid transactions the rematch
  # pass recovered. known_uprn_addresses/uprn_infill are the closed,
  # single-pass Stream-1 versions (not rebuilt post-rematch - see the Stage
  # 6d comment above); build_uprn_all_addresses() prefers best_address over
  # infill_street wherever both are present, so a UPRN that later gained a
  # real address via rematch (matched precisely because it already had one)
  # simply makes its infill guess redundant, not wrong.
  tar_target(
    uprn_all_addresses,
    build_uprn_all_addresses(
      uprn_historical, uprn_historical_epc_lr_final, dec_addresses,
      house_price_lr_final, known_uprn_addresses, uprn_infill,
      uprn_usrn, usrn_street_names, uprn_places, uprn_inspire_lookup,
      house_prices_nowcast_final = house_prices_nowcast_final
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

  # Wide-to-narrow attribute table for the UPRN pmtiles export - cheap (a
  # select()/coalesce() pass), so it's a normal DAG target; actually writing
  # the GeoJSON/PMTiles (build_uprn_pmtiles(), pipeline/R/pmtiles.R) is a
  # heavy one-off left for Malcolm to run manually, same convention as Azure
  # geocoding - see that file's header.
  tar_target(uprn_pmtiles_data, build_uprn_pmtiles_data(uprn_all_addresses)),

  # --- Stage 6f: Stream 2 continued - fuzzy free-text matching (last
  # resort, see pipeline/R/fuzzy_match.R) ---
  # Runs only on rows every exact stage in match_free_sources() already
  # failed on, fuzzily comparing AddressLine's street text against
  # fuzzy_lookup (built above, alongside street_lookup/infill_lookup - see
  # the header note) (block-then-compare, never all-pairs) instead of
  # requiring an exact key. Tagged match_quality = "fuzzy" - the
  # least-trusted tier, below "guess" - with a similarity score kept for
  # auditing. Kept as its own stage/target rather than folded into
  # match_free_sources() so the well-tested exact cascade is untouched and
  # this fuzzier logic stays easy to disable on its own.
  tar_target(fuzzy_match, match_fuzzy_sources(free_match$unmatched, fuzzy_lookup, postcode_history_lookup)),
  tar_target(
    fuzzy_match_coverage,
    audit_fuzzy_matches(free_match$unmatched, fuzzy_match)
  ),

  # --- Stage 8: geocode queue (tasks 2/4) ---
  # Nationwide (street name -> how many districts use it) lookup that flags
  # postcode-less, ambiguous-street queue rows for deprioritisation - see
  # build_street_ambiguity_lookup()/flag_ambiguous_street() in
  # pipeline/R/geocode_queue.R.
  tar_target(street_ambiguity_lookup, build_street_ambiguity_lookup(usrn_street_names)),
  tar_target(
    queue_built,
    build_geocode_queue(fuzzy_match$unmatched, street_ambiguity = street_ambiguity_lookup)
  ),
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
      carry_forward$carried_forward,
      dplyr::bind_rows(free_match$matched, fuzzy_match$matched),
      azure_results,
      queue_current, uprn_historical, uprn_inspire_lookup
    )
  )
)
