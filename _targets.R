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
# For a fast end-to-end smoke test instead of the full multi-million-row
# run, set options(pipeline.sample_n = 20000) before tar_make() - the raw
# CCOD/OCOD importers and the INSPIRE loader will both subsample.

library(targets)

source("R/find_onedrive.R")
tar_source("pipeline/R") # pure function definitions only - never R/ (old scripts have top-level side effects)

onedrive <- find_onedrive()
inspire_path <- "F:/GitHub/PlaceBasedCarbonCalculator/inputdata/INSPIRE"

tar_option_set(
  packages = c(
    "readr", "dplyr", "purrr", "stringr", "stringi", "sf", "readxl",
    "data.table", "plyr", "lwgeom", "stplanr"
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

  # --- Stage 5: external free resources (tasks 5/6) ---
  tar_target(ext_uprn_historical, load_uprn_historical()),
  tar_target(ext_uprn_epc_lr, load_uprn_historical_epc_lr()),
  tar_target(ext_house_price_lr_uprn, load_house_price_lr_uprn()),

  # --- Stage 6: free-source matching (tasks 5/6) ---
  tar_target(epc_lookup, build_epc_lookup(ext_uprn_epc_lr)),
  tar_target(price_paid_lookup, build_price_paid_lookup(ext_house_price_lr_uprn)),
  tar_target(free_match, match_free_sources(carry_forward$needs_geocode, epc_lookup, price_paid_lookup)),

  # --- Stage 7: INSPIRE <-> UPRN lookup (task 7) ---
  tar_target(inspire_clean, load_inspire_clean(inspire_path)),
  tar_target(uprn_inspire_lookup, build_uprn_inspire_lookup(inspire_clean, ext_uprn_historical)),

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
      queue_current, ext_uprn_historical, uprn_inspire_lookup
    )
  )
)
