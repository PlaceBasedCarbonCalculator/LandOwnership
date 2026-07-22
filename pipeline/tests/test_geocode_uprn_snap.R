# Regression tests for snapping Azure geocode results to the nearest UPRN
# (pipeline/R/geocode_uprn_snap.R). Pure functions + synthetic data only -
# no big data, no network, no Azure.
# Run from the repo root:
#   Rscript pipeline/tests/test_geocode_uprn_snap.R

setwd_root <- function() {
  if (file.exists("R/text_cleaning.R")) return(invisible())
  if (file.exists("../../R/text_cleaning.R")) return(setwd("../../"))
  stop("Run from the repository root.")
}
setwd_root()

source("pipeline/R/geocode_uprn_snap.R")

check <- function(label, cond) {
  if (!isTRUE(cond)) stop("FAILED: ", label, call. = FALSE)
  invisible(TRUE)
}

# --- empty input short-circuits without touching uprn_historical -----------
empty_res <- data.frame(queue_key = character(0), longitude = numeric(0), latitude = numeric(0))
out0 <- snap_geocoded_to_uprn(empty_res, data.frame(UPRN = 1, X_COORDINATE = 1, Y_COORDINATE = 1))
check(
  "empty input returns 0 rows with expected columns",
  nrow(out0) == 0 && all(c("UPRN", "uprn_snap_distance_m", "source") %in% names(out0))
)
cat("empty-input short-circuit: OK\n")

# --- nearest-UPRN snap + tolerance cutoff -----------------------------------
# Three UPRNs on a line in BNG (27700), 100-300m apart.
uprn_historical <- data.frame(
  UPRN = c(101, 102, 103),
  X_COORDINATE = c(430000, 430100, 430300),
  Y_COORDINATE = c(433000, 433000, 433000)
)
pts_bng <- sf::st_as_sf(uprn_historical, coords = c("X_COORDINATE", "Y_COORDINATE"), crs = 27700)
pts_ll <- sf::st_coordinates(sf::st_transform(pts_bng, 4326))

azure_results <- data.frame(
  queue_key = c("A", "B", "C"),
  # A: a few metres from UPRN 102 (should snap to it); B: exactly UPRN 101;
  # C: several hundred km away (should exceed tolerance and stay unsnapped)
  longitude = c(pts_ll[2, 1] + 0.0002, pts_ll[1, 1], pts_ll[3, 1] + 5),
  latitude = c(pts_ll[2, 2], pts_ll[1, 2], pts_ll[3, 2])
)

out <- snap_geocoded_to_uprn(azure_results, uprn_historical, tolerance_m = 50)

check("nearest point snaps to the closer UPRN", out$UPRN[out$queue_key == "A"] == 102)
check("exact-coordinate point snaps with ~0 distance", out$UPRN[out$queue_key == "B"] == 101 && out$uprn_snap_distance_m[out$queue_key == "B"] < 1)
check("snapped rows tagged azure_geocoded_uprn_snap", all(out$source[out$queue_key %in% c("A", "B")] == "azure_geocoded_uprn_snap"))
check("point beyond tolerance_m is left unsnapped", is.na(out$UPRN[out$queue_key == "C"]))
check("unsnapped row tagged plain azure_geocoded", out$source[out$queue_key == "C"] == "azure_geocoded")
check("original azure_results columns are preserved", all(c("queue_key", "longitude", "latitude") %in% names(out)))
cat("nearest-UPRN snap + tolerance cutoff: OK\n")

# --- flat-group inference provenance carried through source ----------------
azure_results2 <- azure_results
azure_results2$inferred_from <- c(NA, "T1||1 Fake Street", NA)
out2 <- snap_geocoded_to_uprn(azure_results2, uprn_historical, tolerance_m = 50)
check(
  "a row with inferred_from is tagged azure_geocoded_flat_inferred, not azure_geocoded_uprn_snap",
  out2$source[out2$queue_key == "B"] == "azure_geocoded_flat_inferred"
)
check(
  "a row with no inferred_from keeps the ordinary uprn_snap tag",
  out2$source[out2$queue_key == "A"] == "azure_geocoded_uprn_snap"
)
cat("inferred_from provenance passthrough: OK\n")

cat("\nAll tests passed.\n")
