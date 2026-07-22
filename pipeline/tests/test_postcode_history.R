# Regression tests for postcode-BOUNDARY x USRN centroid linking
# (pipeline/R/postcode_history.R::link_usrn_postcode_boundaries()). Pure
# function + synthetic geometry only - no cross-store read, no big data.
# Run from the repo root:
#   Rscript pipeline/tests/test_postcode_history.R

suppressMessages({
  library(sf)
  library(dplyr)
  library(data.table)
})

setwd_root <- function() {
  if (file.exists("R/text_cleaning.R")) return(invisible())
  if (file.exists("../../R/text_cleaning.R")) return(setwd("../../"))
  stop("Run from the repository root.")
}
setwd_root()

source("pipeline/R/utils.R")
source("pipeline/R/postcode_history.R")

check <- function(label, cond) {
  if (!isTRUE(cond)) stop("FAILED: ", label, call. = FALSE)
  invisible(TRUE)
}

mk_poly <- function(x0, y0, x1, y1) {
  sf::st_polygon(list(matrix(c(x0, y0, x1, y0, x1, y1, x0, y1, x0, y0), ncol = 2, byrow = TRUE)))
}
mk_line <- function(x1, y1, x2, y2) sf::st_linestring(matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE))

# Two USRNs:
#   700 "Test Road" runs the full width (10,50)-(90,50), crossing BOTH
#     postcodes AA1 1AA (west half, x 0-50) and AA1 1AB (east half, x 50-100)
#     in every boundary vintage - tests the multi-postcode st_intersection()
#     path and that each half gets a centroid on its own side.
#   701 "Old Road" sits far away (x ~1000) inside a postcode (ZZ9 9ZZ) that
#     only exists in the 2020 boundary release (retired by 2015... no,
#     retired BEFORE 2024/absent from 2015 too here) - tests that a
#     postcode/street combo found ONLY in a historical vintage still
#     surfaces, tagged with that vintage's year.
usrn_geom_syn <- sf::st_sf(
  usrn = c(700, 701),
  geometry = sf::st_sfc(
    mk_line(10, 50, 90, 50),
    mk_line(1000, 10, 1000, 90),
    crs = 27700
  )
)
usrn_street_names_syn <- data.frame(
  USRN = c(700, 701), street = c("Test Road", "Old Road"), stringsAsFactors = FALSE
)

bounds_current <- sf::st_sf(
  POSTCODE = c("AA1 1AA", "AA1 1AB"),
  geometry = sf::st_sfc(mk_poly(0, 0, 50, 100), mk_poly(50, 0, 100, 100), crs = 27700)
)
bounds_2020 <- sf::st_sf(
  POSTCODE = c("AA1 1AA", "AA1 1AB", "ZZ9 9ZZ"),
  geometry = sf::st_sfc(
    mk_poly(0, 0, 50, 100), mk_poly(50, 0, 100, 100), mk_poly(990, 0, 1010, 100)
  ),
  crs = 27700
)
bounds_years_syn <- list("2024" = bounds_current, "2020" = bounds_2020, "2015" = bounds_current)

out <- link_usrn_postcode_boundaries(usrn_geom_syn, usrn_street_names_syn, bounds_years_syn)

check("west-half key present", "AA11AA|TEST ROAD" %in% out$key)
check("east-half key present", "AA11AB|TEST ROAD" %in% out$key)
check(
  "west-half key most recent year is 2024 (freshest wins)",
  out$last_seen_year[out$key == "AA11AA|TEST ROAD"] == 2024
)
check(
  "retired postcode/street combo surfaces from the 2020-only boundary",
  "ZZ99ZZ|OLD ROAD" %in% out$key && out$last_seen_year[out$key == "ZZ99ZZ|OLD ROAD"] == 2020
)

west_lon <- out$LONGITUDE[out$key == "AA11AA|TEST ROAD"]
east_lon <- out$LONGITUDE[out$key == "AA11AB|TEST ROAD"]
check("west centroid sits west of east centroid (split on the right side)", west_lon < east_lon)

# sanity: every returned centroid is a real lon/lat pair
check("all centroids have finite coordinates", all(is.finite(out$LATITUDE)) && all(is.finite(out$LONGITUDE)))
cat("link_usrn_postcode_boundaries: OK\n")

# --- the representative point must lie ON one of the roads -------------------
# Two SEPARATE USRNs sharing a street name inside one postcode, on parallel
# lines 60m apart. The centroid must be a real point on one of them
# (y == 433000 or y == 433060), never the average of the two (y == 433030),
# which would sit in the middle of the block - off the road entirely, and
# for a concave or disconnected street potentially outside the postcode too.
# This is the whole guarantee the lookup exists to provide, so it is
# asserted directly.
#
# Unlike the fixtures above (which only assert relative east/west ordering),
# this one checks an ABSOLUTE position after the function's internal
# 27700 -> 4326 conversion, so it has to use realistic British National Grid
# coordinates. Near the BNG false origin the OSGB36 datum shift is
# undefined - a (25, 20) round-trip comes back as (-36, 93), tens of metres
# out - which would make a metre-level assertion meaningless. These are
# central Leeds.
usrn_geom_par <- sf::st_sf(
  usrn = c(800, 801),
  geometry = sf::st_sfc(
    mk_line(430000, 433000, 430300, 433000),
    mk_line(430000, 433060, 430300, 433060),
    crs = 27700
  )
)
usrn_street_names_par <- data.frame(
  USRN = c(800, 801), street = c("Parallel Street", "Parallel Street"),
  stringsAsFactors = FALSE
)
bounds_par <- sf::st_sf(
  POSTCODE = "BB2 2BB",
  geometry = sf::st_sfc(mk_poly(429900, 432900, 430400, 433100), crs = 27700)
)
out_par <- link_usrn_postcode_boundaries(
  usrn_geom_par, usrn_street_names_par, list("2024" = bounds_par)
)
par_row <- out_par[out_par$key == "BB22BB|PARALLEL STREET", ]
check("two USRNs sharing a (postcode, street) collapse to one row", nrow(par_row) == 1)
check("both USRNs counted", par_row$n_usrn == 2)
par_bng <- sf::st_coordinates(sf::st_transform(
  sf::st_as_sf(par_row, coords = c("LONGITUDE", "LATITUDE"), crs = 4326), 27700
))
check(
  "centroid is ON one of the two roads",
  abs(par_bng[1, 2] - 433000) < 1 || abs(par_bng[1, 2] - 433060) < 1
)
check(
  "centroid is NOT the midpoint between the two roads",
  abs(par_bng[1, 2] - 433030) > 1
)

# A thunk (zero-arg function) is accepted as well as a ready-made layer, so
# build_street_postcode_boundary_lookup() can defer the multi-GB cross-store
# tar_read() until the loop actually needs that vintage.
out_thunk <- link_usrn_postcode_boundaries(
  usrn_geom_par, usrn_street_names_par, list("2024" = function() bounds_par)
)
check("lazily-supplied boundary layer gives the same result", identical(out_thunk, out_par))
cat("on-road centroid + lazy boundary loading: OK\n")

# --- empty inputs short-circuit cleanly --------------------------------------
empty_out <- link_usrn_postcode_boundaries(
  usrn_geom_syn[0, ], usrn_street_names_syn, bounds_years_syn
)
check("empty usrn_geom returns 0 rows", nrow(empty_out) == 0)
check(
  "empty result has the expected columns",
  all(c("key", "LATITUDE", "LONGITUDE", "n_usrn", "last_seen_year") %in% names(empty_out))
)
cat("empty-input short-circuit: OK\n")

cat("\nAll tests passed.\n")
