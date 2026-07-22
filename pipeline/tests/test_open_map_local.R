# Regression tests for OS Open Map Local <-> USRN linking
# (pipeline/R/open_map_local.R). Pure functions + synthetic geometry only -
# no big data, no network.
# Run from the repo root:
#   Rscript pipeline/tests/test_open_map_local.R

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
source("pipeline/R/open_map_local.R")

check <- function(label, cond) {
  if (!isTRUE(cond)) stop("FAILED: ", label, call. = FALSE)
  invisible(TRUE)
}

# --- oml_excludes_properties --------------------------------------------------
check("motorway excluded", oml_excludes_properties("Motorway"))
check("collapsed-dual motorway excluded", oml_excludes_properties("Motorway, Collapsed Dual Carriageway"))
check("guided busway excluded", oml_excludes_properties("Guided Busway Carriageway"))
check("A Road NOT excluded (mixed in practice)", !oml_excludes_properties("A Road"))
check("Primary Road NOT excluded", !oml_excludes_properties("Primary Road"))
check("Local Road NOT excluded", !oml_excludes_properties("Local Road"))
check("NA classification NOT excluded", !oml_excludes_properties(NA_character_))
cat("oml_excludes_properties: OK\n")

# --- link_usrn_oml (synthetic geometry, EPSG:27700) --------------------------
mk_line <- function(x1, y1, x2, y2) sf::st_linestring(matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE))
# OS Open USRN publishes every street as MULTILINESTRING (all 1766832 of
# them in osopenusrn_202607 - not a mix), and sf::st_line_sample() rejects
# anything that isn't sfc_LINESTRING. The USRN fixtures below are therefore
# MULTILINESTRING, matching the real product: an earlier LINESTRING-only
# fixture let a bug through that made link_usrn_oml() fail on the very first
# real call while every test passed.
mk_mline <- function(...) sf::st_multilinestring(lapply(list(...), unclass))

# USRN 900: a single 100m street that OML splits into two junction-to-
# junction segments, both named "Fake Street", classification "Local Road".
# USRN 901: a short (8m) street below the sample spacing (25m) - must still
# get exactly one probe via the point-on-surface fallback.
# USRN 902: far from any OML road - no match at all.
# USRN 903: digitised in TWO disconnected parts (a real MULTILINESTRING with
# more than one member), both alongside "Other Lane" - probes from both
# parts must be attributed back to the one USRN.
usrn_geom_syn <- sf::st_sf(
  usrn = c(900, 901, 902, 903),
  geometry = sf::st_sfc(
    mk_mline(mk_line(0, 0, 100, 0)),
    mk_mline(mk_line(500, 500, 508, 500)),
    mk_mline(mk_line(0, 5000, 100, 5000)),
    mk_mline(mk_line(500, 502, 540, 502), mk_line(560, 502, 600, 502)),
    crs = 27700
  )
)
check(
  "fixture matches the real product's geometry type",
  inherits(sf::st_geometry(usrn_geom_syn), "sfc_MULTILINESTRING")
)
oml_roads_syn <- sf::st_sf(
  id = c("r1", "r2", "r3", "m1"),
  classification = c("Local Road", "Local Road", "Local Road", "Motorway"),
  distinctive_name = c("Fake Street", "Fake Street", "Other Lane", NA_character_),
  road_number = c(NA_character_, NA_character_, NA_character_, "M1"),
  geometry = sf::st_sfc(
    mk_line(0, 1, 50, 1), # first half of USRN 900, 1m away
    mk_line(50, 1, 100, 1), # second half of USRN 900, 1m away
    mk_line(500, 501, 508, 501), # USRN 901's short street, 1m away
    mk_line(0, 6000, 100, 6000), # far from everything (nearest to USRN 902 but >15m... actually 1000m away)
    crs = 27700
  )
)

link <- link_usrn_oml(usrn_geom_syn, oml_roads_syn, max_dist = 15, spacing = 25)
check("USRN 900 named from majority OML segment", link$street[link$USRN == 900] == "FAKE STREET")
check("USRN 900 classification carried through", link$oml_classification[link$USRN == 900] == "Local Road")
check(
  "USRN 901 (shorter than spacing) still gets a fallback probe and a name",
  901 %in% link$USRN && link$street[link$USRN == 901] == "OTHER LANE"
)
check(
  "multi-part USRN 903 is named once, from probes across both parts",
  sum(link$USRN == 903) == 1 && link$street[link$USRN == 903] == "OTHER LANE"
)
check("USRN 902 (nothing within max_dist) has no match", !902 %in% link$USRN)
cat("link_usrn_oml: OK\n")

# --- build_usrn_street_names(): OML tried before OSM fallback ---------------
source("R/text_cleaning.R")
source("R/address_functions.R")
source("pipeline/R/uprn_infill.R")

oml_link_syn <- data.frame(
  USRN = c(950, 951), street = c("OML NAMED ROAD", "OML SHARED ROAD"),
  street_n = c(3, 3), street_agreement = c(1, 0.9),
  oml_classification = c("Local Road", "Motorway"),
  oml_class_agreement = c(1, 1), oml_road_number = c(NA_character_, "M60"),
  stringsAsFactors = FALSE
)
osm_roads_for_names <- sf::st_sf(
  name = "OSM NAMED ROAD",
  geometry = sf::st_sfc(mk_line(0, 1, 100, 1), crs = 27700)
)
usrn_geom_for_names <- sf::st_sf(
  usrn = c(950, 951, 952),
  geometry = sf::st_sfc(
    mk_line(0, 0, 100, 0), # 950: has an OML name - should NOT fall through to OSM
    mk_line(200, 200, 300, 200), # 951: has an OML name too (a motorway)
    mk_line(0, 0, 100, 0), # 952: no OML link at all - should fall through to OSM
    crs = 27700
  )
)
names_out <- suppressMessages(build_usrn_street_names(
  uprn_usrn = data.frame(UPRN = numeric(0), USRN = numeric(0)),
  known_uprn_addresses = data.frame(
    UPRN = numeric(0), street = character(0), postcode = character(0)
  ),
  postcode_district = data.frame(postcode = character(0), district = character(0)),
  usrn_geom = usrn_geom_for_names,
  osm_roads = osm_roads_for_names,
  la_bounds_path = NULL,
  uprn_places = NULL,
  oml_link = oml_link_syn
))
check(
  "USRN 950 named from OML, not OSM, confidence tagged oml_road",
  names_out$street[names_out$USRN == 950] == "OML NAMED ROAD" &&
    names_out$street_confidence[names_out$USRN == 950] == "oml_road"
)
check(
  "USRN 950 carries OML classification",
  names_out$oml_classification[names_out$USRN == 950] == "Local Road"
)
check(
  "USRN 951's motorway classification survives even though it's named",
  names_out$oml_classification[names_out$USRN == 951] == "Motorway"
)
check(
  "USRN 952 (no OML link) falls through to the OSM name",
  names_out$street[names_out$USRN == 952] == "OSM NAMED ROAD" &&
    names_out$street_confidence[names_out$USRN == 952] == "osm_road"
)
cat("build_usrn_street_names OML integration: OK\n")

cat("\nAll tests passed.\n")
