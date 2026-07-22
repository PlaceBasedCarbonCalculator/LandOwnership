# Regression tests for the UPRN pmtiles attribute table
# (pipeline/R/pmtiles.R::best_display_address()/build_uprn_pmtiles_data()).
# Pure functions + synthetic data only - no tippecanoe/WSL, no network.
# Run from the repo root:
#   Rscript pipeline/tests/test_pmtiles.R

suppressMessages(library(dplyr))

setwd_root <- function() {
  if (file.exists("R/text_cleaning.R")) return(invisible())
  if (file.exists("../../R/text_cleaning.R")) return(setwd("../../"))
  stop("Run from the repository root.")
}
setwd_root()

source("pipeline/R/pmtiles.R")
source("pipeline/R/uprn_master.R") # epc_domestic_rating/epc_nondomestic_asset_rating

check <- function(label, cond) {
  if (!isTRUE(cond)) stop("FAILED: ", label, call. = FALSE)
  invisible(TRUE)
}

# --- EPC rating extraction --------------------------------------------------
# The published EPC schema (sibling EPC repo, EPC/R/merge_epcs.R) names the
# domestic band `cur_rate` (an ordered factor A<...<G<INVALID!) and the
# non-domestic asset rating `rating` (numeric). A previous
# pick_epc_rating_col() helper guessed from a candidate list that omitted
# `cur_rate` entirely, so every domestic band came back NA while the bare
# "rating" candidate matched the non-domestic NUMBER - the published column
# ended up holding only the values it was least meant to, and the viewer's
# A-G ramp rendered a uniformly grey map. These tests pin the real names.
dom_df <- data.frame(
  cur_rate = factor(c("C", "INVALID!", "A"), levels = c("A", "C", "INVALID!")),
  stringsAsFactors = FALSE
)
check("domestic band read from cur_rate", identical(epc_domestic_rating(dom_df)[c(1, 3)], c("C", "A")))
check("domestic INVALID! mapped to NA", is.na(epc_domestic_rating(dom_df)[2]))
check(
  "domestic rating fails loudly if cur_rate is renamed upstream",
  inherits(try(epc_domestic_rating(data.frame(CURRENT_ENERGY_RATING = "C")), silent = TRUE), "try-error")
)
check(
  "non-domestic asset rating read from `rating`, kept numeric",
  identical(epc_nondomestic_asset_rating(data.frame(rating = c(49, 56))), c(49, 56))
)
check(
  "non-domestic rating fails loudly if `rating` is renamed upstream",
  inherits(try(epc_nondomestic_asset_rating(data.frame(ASSET_RATING = 49)), silent = TRUE), "try-error")
)
cat("epc rating extraction: OK\n")

# Four UPRNs, one per address-source tier:
#   1: has a real best_address - wins outright.
#   2: no best_address, but an inferred house number + street.
#   3: no best_address/number, only an inferred building name.
#   4: nothing but the USRN's own street name.
#   5: nothing at all - address/source both NA.
d <- data.frame(
  UPRN = 1:5,
  LONGITUDE = c(-1.1, -1.2, -1.3, -1.4, NA),
  LATITUDE = c(53.1, 53.2, 53.3, 53.4, 53.5),
  best_address = c("12 Real Street", NA, NA, NA, NA),
  best_address_source = c("epc_domestic", NA, NA, NA, NA),
  best_postcode = c("LS1 1AA", NA, NA, NA, NA),
  class = c("domestic", "domestic", "non-domestic", "unknown", "unknown"),
  epc_dom_rating = c("C", NA, NA, NA, NA),
  epc_nondom_asset_rating = c(NA, NA, 56, NA, NA),
  current_value_2025 = c(250000, NA, NA, NA, NA),
  pp_price = c(200000, NA, NA, NA, NA),
  pp_date = as.Date(c("2020-01-01", NA, NA, NA, NA)),
  district_nsul = c("LEEDS", NA, NA, NA, NA),
  infill_district = c(NA, "LEEDS", "LEEDS", "WAKEFIELD", NA),
  postcode_nsul = c("LS1 1AA", NA, NA, NA, NA),
  infill_postcode = c(NA, "LS6 2AB", NA, NA, NA),
  infill_house_number = c(NA, "22", NA, NA, NA),
  infill_street = c(NA, "Infill Street", NA, NA, NA),
  infill_building_name = c(NA, NA, "Ivy Cottage", NA, NA),
  infill_address_source = c(NA, "osm_building", "usrn_street", NA, NA),
  usrn_street = c(NA, NA, NA, "Fallback Road", NA),
  exists = c(TRUE, TRUE, TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

disp <- best_display_address(d)
check("tier 1: real best_address wins", disp$address[1] == "12 Real Street" && disp$source[1] == "epc_domestic")
check("tier 2: inferred number+street", disp$address[2] == "22 Infill Street" && disp$source[2] == "infill_osm_building")
check("tier 3: inferred building name alone", disp$address[3] == "Ivy Cottage" && disp$source[3] == "infill_usrn_street")
check("tier 4: USRN street name only", disp$address[4] == "Fallback Road" && disp$source[4] == "usrn_street_name_only")
check("tier 5: nothing available -> NA/NA", is.na(disp$address[5]) && is.na(disp$source[5]))
cat("best_display_address: OK\n")

out <- suppressMessages(build_uprn_pmtiles_data(d))
check("rows with no coordinates are dropped (UPRN 5)", !5 %in% out$UPRN && nrow(out) == 4)
# The two EPC ratings must stay in SEPARATE columns - a domestic A-G band
# and a non-domestic numeric asset rating are different scales, and merging
# them previously meant the viewer's A-G colour ramp silently matched
# nothing (see build_uprn_pmtiles_data()).
check(
  "domestic A-G band lands in epc_rating only",
  out$epc_rating[out$UPRN == 1] == "C" && is.na(out$epc_rating[out$UPRN == 3])
)
check(
  "non-domestic asset rating lands in epc_asset_rating only, still numeric",
  out$epc_asset_rating[out$UPRN == 3] == 56 &&
    is.na(out$epc_asset_rating[out$UPRN == 1]) &&
    is.numeric(out$epc_asset_rating)
)
check("current_value carried through", out$current_value[out$UPRN == 1] == 250000)
check(
  "district coalesces NSUL then infill",
  out$district[out$UPRN == 1] == "LEEDS" && out$district[out$UPRN == 2] == "LEEDS" &&
    out$district[out$UPRN == 4] == "WAKEFIELD"
)
check(
  "postcode coalesces NSUL, best, infill",
  out$postcode[out$UPRN == 1] == "LS1 1AA" && out$postcode[out$UPRN == 2] == "LS6 2AB"
)
check("kept at or under the 15-column make_geojson() warning threshold", ncol(out) - 2 <= 15) # -2 for lon/lat, cast to geometry later
cat("build_uprn_pmtiles_data: OK\n")

cat("\nAll tests passed.\n")
