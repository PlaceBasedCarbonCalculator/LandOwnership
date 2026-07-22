# Regression tests for the text cleaning / address parsing / matching-key
# functions. Pure functions only - no big data, no network, no Azure.
# Run from the repo root:
#   Rscript pipeline/tests/test_text_cleaning.R
# Prints one line per test group and stops with an error on any failure.

suppressMessages({
  library(stringr)
  library(stringi)
  library(dplyr)
  library(data.table)
  library(sf)
})

setwd_root <- function() {
  # allow running from repo root or from pipeline/tests
  if (file.exists("R/text_cleaning.R")) return(invisible())
  if (file.exists("../../R/text_cleaning.R")) return(setwd("../../"))
  stop("Run from the repository root.")
}
setwd_root()

source("R/text_cleaning.R")
source("R/address_functions.R")
source("pipeline/R/utils.R")
source("pipeline/R/geocode_queue.R")
source("pipeline/R/uprn_infill.R")
source("pipeline/R/match_free_sources.R")
source("pipeline/R/substations.R")

check <- function(label, cond) {
  if (!isTRUE(cond)) stop("FAILED: ", label, call. = FALSE)
  invisible(TRUE)
}

# --- final_address_tidy ------------------------------------------------------
suppressMessages({
  check("tidy strips tags", final_address_tidy("@MNS Market Street") == "Market Street")
  check("tidy strips multiple tags", !grepl("@", final_address_tidy("@LND @POS 12 High Street")))
  check(
    "tidy trims 'the site of'",
    final_address_tidy("the site of Weeland Road, Knottingley") == "Weeland Road, Knottingley"
  )
  check(
    "tidy trims leading 'and' + empty brackets",
    final_address_tidy("and 11 Wood Street (); Bradford") == "11 Wood Street; Bradford"
  )
  check(
    "tidy trims leading 'being'",
    final_address_tidy("being a Garage on North Lane, Headingley") == "a Garage on North Lane, Headingley"
  )
  check(
    "tidy trims compass glue",
    final_address_tidy("east of Somerton Road, Street") == "Somerton Road, Street"
  )
  check(
    "tidy trims trailing orphan",
    final_address_tidy("12 High Street and") == "12 High Street"
  )
  check(
    "tidy leaves normal addresses alone",
    final_address_tidy("Oaktree House, 408 Oakwood Lane, Leeds") ==
      "Oaktree House, 408 Oakwood Lane, Leeds"
  )
  check(
    "tidy keeps leading 'The'",
    final_address_tidy("The Old Vicarage, Church Lane") == "The Old Vicarage, Church Lane"
  )
})
cat("final_address_tidy: OK\n")

# --- clean_mines / clean_airspace (lazy + bounded, audit F3) -----------------
suppressMessages({
  m1 <- clean_mines("all mines and minerals as excepted filed at the Registry 12 High Street, Leeds")
  check("mines clause replaced", grepl("@MNS", m1))
  check("mines: address after clause survives", grepl("12 High Street, Leeds", m1))

  # greedy regression: an address sandwiched before a second end-marker
  # must not be eaten (lazy stops at the first end marker)
  m2 <- clean_mines(paste0(
    "the mines and minerals excepted by a deed dated 17B Curzon Street, London ",
    "shown on the plan registered under Title 12345"
  ))
  check("mines: lazy stops at first end marker", grepl("Curzon Street", m2))

  a1 <- clean_airspace("The airspace above 12 High Street, London")
  check("airspace tagged", grepl("@ASP", a1))
  check("airspace: address survives", grepl("12 High Street, London", a1))

  a2 <- clean_airspace("airspace at roof level over 5 High Street being flat 3")
  check("airspace: lazy keeps text after first end marker", grepl("5 High Street", a2))
})
cat("clean_mines / clean_airspace: OK\n")

# --- clean_flats: eighth floor (audit F5) ------------------------------------
f1 <- clean_flats("the eighth floor being 55 Broadway, London")
check("clean_flats matches 'eighth'", grepl("@FTS", f1))
cat("clean_flats: OK\n")

# --- clean_roof_basement -----------------------------------------------------
suppressMessages({
  check(
    "roof: 'the roof of' tagged and address survives",
    grepl("@ASP", clean_roof_basement("the roof of 55 Prospect Terrace")) &&
      grepl("55 Prospect Terrace", clean_roof_basement("the roof of 55 Prospect Terrace"))
  )
  check(
    "roof: compass-facing roof tagged (comma end, no 'of')",
    grepl("@ASP", clean_roof_basement("the south-facing roof, 55 Prospect Terrace")) &&
      grepl("55 Prospect Terrace", clean_roof_basement("the south-facing roof, 55 Prospect Terrace"))
  )
  check(
    "roof: spaced compass variant ('south east facing roof') tagged",
    grepl("@ASP", clean_roof_basement("the south east facing roof of 12 Kirkgate"))
  )
  check(
    "basement: 'the basement,' tagged and address survives",
    grepl("@ASP", clean_roof_basement("the basement, 23 Call Lane")) &&
      grepl("23 Call Lane", clean_roof_basement("the basement, 23 Call Lane"))
  )
  check(
    "basement: 'the basement of' tagged",
    grepl("@ASP", clean_roof_basement("the basement of 23 Call Lane"))
  )
  check(
    "roof/basement: bare word without leading 'the' left untouched",
    !grepl("@ASP", clean_roof_basement("Roof Gardens, 4 High Street"))
  )
  check(
    "roof/basement: normal address unaffected",
    clean_roof_basement("Oaktree House, 408 Oakwood Lane, Leeds") ==
      "Oaktree House, 408 Oakwood Lane, Leeds"
  )
})
cat("clean_roof_basement: OK\n")

# --- split_numbers -----------------------------------------------------------
# (split output keeps incidental double spaces - final_address_tidy squishes
# them later - so compare squished)
s1 <- str_squish(split_numbers("10 to 14 (even) Example Street"))
check("even range expands", identical(s1, c("10 Example Street", "12 Example Street", "14 Example Street")))

s2 <- str_squish(split_numbers("31 to 35 (Odd) Foo Street"))
check(
  "uppercase Odd now normalised (audit F5)",
  identical(s2, c("31 Foo Street", "33 Foo Street", "35 Foo Street"))
)

s3 <- str_squish(split_numbers("103a and 103b Archel Road, London"))
check("letter suffixes kept", all(c("103a Archel Road, London", "103b Archel Road, London") %in% s3))

# "8 to 12 (EVN)": the second number carries the link-back and the modifier
s4 <- suppressWarnings(parse_number_table(data.frame(
  position = c(1, 3), number = c("8", "12"), link = c(FALSE, TRUE),
  mod = c(FALSE, TRUE), modval = c("", "(EVN)")
)))
check("parse_number_table numeric order", identical(s4, c("8", "10", "12")))

# Unit/room codes shaped like "L3-03629" (a level/block letter fused to
# digits, hyphen, more digits) must not be read as a house-number range -
# "L3-03629" was previously expanded into 3,627 fabricated rows for a single
# student-hall title (Level 3, Room 03629, not numbers 3 to 3629).
s5 <- split_numbers("L3-03629, Trinity A Halls Of Residence, Laisteridge Lane, Bradford")
check("unit code not treated as a range", identical(s5, "L3-03629, Trinity A Halls Of Residence, Laisteridge Lane, Bradford"))

# Even without the letter-prefix shape, any range this implausibly large
# (parse_number_table()'s max_range guard) must fall back to the address
# unchanged rather than fabricating thousands of rows.
s6 <- suppressMessages(split_numbers("1-5000 Example Street"))
check("implausible range falls back unchanged", identical(s6, "1-5000 Example Street"))
cat("split_numbers: OK\n")

# --- matching keys -----------------------------------------------------------
check(
  "number key",
  normalise_match_key("34 Autumn Terrace, Leeds", "LS6 1RN") == "LS61RN|34"
)
check(
  "number key NA without postcode",
  is.na(normalise_match_key("34 Autumn Terrace, Leeds", NA))
)
check(
  "building key",
  normalise_building_key("Ivy Cottage, Ackton Lane, Pontefract", "WF7 6HP") == "WF76HP|IVY COTTAGE"
)
check(
  "building key refuses generic 'Flat 3'",
  is.na(normalise_building_key("Flat 3, 10 High Street", "LS1 1AA"))
)
check(
  "building key refuses numbered address",
  is.na(normalise_building_key("10 High Street", "LS1 1AA"))
)
check(
  "street key",
  street_number_key("23", "Fir Tree Gardens", "Leeds") == "LEEDS|FIR TREE GARDENS|23"
)
check(
  "street key NA without district",
  is.na(street_number_key("23", "Fir Tree Gardens", NA))
)
check(
  "street name extraction",
  extract_street_name("23 Fir Tree Gardens, Leeds") == "Fir Tree Gardens"
)
check(
  "street name NA for named building",
  is.na(extract_street_name("Ivy Cottage, Ackton Lane"))
)
check(
  "street name collapses a repeated street mention (land/substation boilerplate)",
  extract_street_name("348 Lowedges Road Lowedges Road and substation Lowedges Road") == "Lowedges Road"
)
check(
  "street name unaffected when there is no repeat",
  extract_street_name("22 Acacia Avenue") == "Acacia Avenue"
)
cat("matching keys: OK\n")

# --- classify_geocodability (audit F2) ---------------------------------------
g <- classify_geocodability(
  c(
    "@MNS", "Properties at Aire Walk, Croft Avenue and Willow Road, Knottingley",
    "16", "16", "34 Autumn Terrace, Leeds",
    paste0(
      "and easement or right in perpetuity for all or any of the purposes ",
      "of the London Electric Railway Acts 1923 and the Acts incorporated ",
      "therewith shown edged red on the plan of the above title filed at the Registry"
    ),
    ""
  ),
  c(NA, NA, NA, "LS6 1RN", "LS6 1RN", NA, NA)
)
check("gate: tag-only", g[1] == "residual_tag")
check("gate: Properties at", g[2] == "multi_property_list")
check("gate: bare number without postcode", g[3] == "bare_number_no_postcode")
check("gate: bare number WITH postcode ok", g[4] == "ok")
check("gate: normal address ok", g[5] == "ok")
check("gate: legalese", g[6] == "legalese")
check("gate: empty", g[7] == "empty")
cat("classify_geocodability: OK\n")

# --- extract_buried_street ----------------------------------------------------
check(
  "buried street: found after leading non-street text",
  extract_buried_street("the paddock north of Foo Street") == "Foo Street"
)
check(
  "buried street: picks the LAST match, not the first",
  extract_buried_street("Land adjoining Old Mill Lane, rear of Foo Street") == "Foo Street"
)
check(
  "buried street: NA when nothing matches",
  is.na(extract_buried_street("just some random text"))
)
cat("extract_buried_street: OK\n")

# --- match_free_sources(): buried-street last-resort stage --------------------
street_centroid_lookup_syn <- data.frame(
  key = "LEEDS|FOO STREET", LATITUDE = 53.8, LONGITUDE = -1.5, n_usrn = 1,
  stringsAsFactors = FALSE
)
buried_rows <- data.frame(
  `Title Number` = "T1", AddressLine = "the paddock north of Foo Street",
  PostalCode = NA_character_, District = "Leeds",
  check.names = FALSE, stringsAsFactors = FALSE
)
res_buried <- suppressMessages(match_free_sources(
  buried_rows,
  epc_lookup = data.frame(key = character(0), UPRN = numeric(0), LATITUDE = numeric(0), LONGITUDE = numeric(0), match_source = character(0)),
  price_paid_lookup = data.frame(key = character(0), UPRN = numeric(0), LATITUDE = numeric(0), LONGITUDE = numeric(0), match_source = character(0)),
  street_centroid_lookup = street_centroid_lookup_syn
))
check(
  "buried street stage resolves a row the leading-segment stage would miss",
  nrow(res_buried$matched) == 1 && res_buried$matched$match_quality == "street" &&
    grepl("buried", res_buried$matched$source)
)
cat("match_free_sources buried-street stage: OK\n")

# --- street ambiguity (geocode_queue.R) ---------------------------------------
usrn_street_names_amb <- data.frame(
  USRN = 1:5,
  street = c("Prospect Terrace", "Prospect Terrace", "Prospect Terrace", "Prospect Terrace", "Unique Close"),
  district = c("LEEDS", "WAKEFIELD", "BRADFORD", NA, "LEEDS"),
  stringsAsFactors = FALSE
)
amb_lookup <- build_street_ambiguity_lookup(usrn_street_names_amb)
check(
  "ambiguity lookup counts distinct districts, ignoring NA",
  amb_lookup$n_district[amb_lookup$street_norm == "PROSPECT TERRACE"] == 3
)
check(
  "flag_ambiguous_street: no postcode + common name -> ambiguous",
  flag_ambiguous_street("55 Prospect Terrace", NA_character_, amb_lookup, min_district = 3)
)
check(
  "flag_ambiguous_street: WITH a postcode -> not flagged",
  !flag_ambiguous_street("55 Prospect Terrace", "LS6 1RN", amb_lookup, min_district = 3)
)
check(
  "flag_ambiguous_street: unique street name -> not flagged",
  !flag_ambiguous_street("2 Unique Close", NA_character_, amb_lookup, min_district = 3)
)
cat("street ambiguity flagging: OK\n")

# --- build_geocode_queue(): queue_priority wiring ------------------------------
queue_input <- data.frame(
  `Title Number` = c("T1", "T2"),
  AddressLine = c("55 Prospect Terrace", "12 Unique Close, Leeds"),
  PostalCode = NA_character_, District = c(NA_character_, "LEEDS"),
  check.names = FALSE, stringsAsFactors = FALSE
)
qtmp <- tempfile(fileext = ".rds")
q_out <- suppressMessages(build_geocode_queue(queue_input, queue_path = qtmp, street_ambiguity = amb_lookup))
check(
  "ambiguous row deprioritised but still pending",
  q_out$queue_priority[q_out$AddressLine == "55 Prospect Terrace"] == 1L &&
    q_out$status[q_out$AddressLine == "55 Prospect Terrace"] == "pending"
)
check(
  "unambiguous row keeps default priority",
  q_out$queue_priority[q_out$AddressLine == "12 Unique Close, Leeds"] == 0L
)
unlink(qtmp)
cat("build_geocode_queue queue_priority: OK\n")

# --- gap guessing (synthetic street) -----------------------------------------
known <- data.frame(
  UPRN = c(1, 2, 3),
  USRN = c(100, 100, 100),
  house_number = c("22", "26", "21"),
  postcode = c("LS61RN", "LS61RN", "LS61RN"),
  X_COORDINATE = c(0, 20, 10),
  Y_COORDINATE = c(5, 5, -20)
)
unknown <- data.frame(
  UPRN = c(9, 10),
  USRN = c(100, 100),
  X_COORDINATE = c(10, 300),
  Y_COORDINATE = c(6, 5)
)
gg <- guess_gap_numbers(unknown, known, usrn_geom = NULL)
check("gap guess: 24 guessed between 22 and 26", nrow(gg) == 1 && gg$house_number == "24")
check("gap guess: inherits postcode", gg$postcode == "LS61RN")
check("gap guess: far-away UPRN not guessed", !10 %in% gg$UPRN)

# unknown on the wrong side of the street must not be guessed
unknown_wrong_side <- data.frame(
  UPRN = 11, USRN = 100, X_COORDINATE = 10, Y_COORDINATE = -21
)
gg2 <- guess_gap_numbers(unknown_wrong_side, known, usrn_geom = NULL)
check("gap guess: wrong side rejected", nrow(gg2) == 0)

# two candidate unknowns in one gap is ambiguous - no guess
unknown_two <- data.frame(
  UPRN = c(12, 13), USRN = c(100, 100),
  X_COORDINATE = c(8, 12), Y_COORDINATE = c(6, 6)
)
gg3 <- guess_gap_numbers(unknown_two, known, usrn_geom = NULL)
check("gap guess: two candidates -> no guess", nrow(gg3) == 0)

# gap of 6 (two missing houses) - too uncertain, no guess
known_wide <- known
known_wide$house_number <- c("22", "28", "21")
gg4 <- guess_gap_numbers(unknown, known_wide, usrn_geom = NULL)
check("gap guess: wide gap -> no guess", nrow(gg4) == 0)
cat("guess_gap_numbers: OK\n")

# --- name_usrns_from_osm (synthetic geometry, EPSG:27700) --------------------
mk_line <- function(x1, y1, x2, y2) sf::st_linestring(matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE))
usrn_geom_syn <- sf::st_sf(
  usrn = c(500, 501),
  geometry = sf::st_sfc(
    mk_line(0, 0, 100, 0),     # runs parallel to Fake Street, 1m away
    mk_line(0, 500, 100, 500), # nowhere near any named road
    crs = 27700
  )
)
osm_roads_syn <- sf::st_sf(
  name = c("Fake Street", "Other Road"),
  geometry = sf::st_sfc(
    mk_line(0, 1, 100, 1),
    mk_line(0, 200, 100, 200),
    crs = 27700
  )
)
nm <- name_usrns_from_osm(usrn_geom_syn, osm_roads_syn)
check("osm road naming: adjacent road wins", nm$street[nm$USRN == 500] == "FAKE STREET")
check("osm road naming: distant USRN not named", !501 %in% nm$USRN)
nm2 <- name_usrns_from_osm(usrn_geom_syn, osm_roads_syn, exclude_usrns = 500)
check("osm road naming: excluded USRN skipped", nrow(nm2) == 0)
cat("name_usrns_from_osm: OK\n")

# --- NSUL: lad->district, uprn_places, apply_uprn_places, singletons ---------
nsul_syn <- data.frame(
  UPRN = c(1, 2, 3, 4),
  postcode = c("LS61RN", "LS61RN", "WF76HP", "XX00XX"),
  lad_code = c("E1", "E1", "E2", "E3")
)
pcd_syn <- data.frame(postcode = c("LS61RN", "WF76HP"), district = c("LEEDS", "WAKEFIELD"))
lad_names_syn <- data.frame(
  lad_code = c("E1", "E2", "E3"),
  lad_name = c("Leeds", "Wakefield", "Somewhere Else")
)
lad_d <- build_lad_district_lookup(nsul_syn, pcd_syn, lad_names_syn)
check("lad->district: LR spelling wins", lad_d$district[lad_d$lad_code == "E1"] == "LEEDS")
check(
  "lad->district: official name fallback",
  lad_d$district[lad_d$lad_code == "E3"] == "SOMEWHERE ELSE"
)
up <- build_uprn_places(nsul_syn, lad_d)
check(
  "uprn_places joins postcode + district",
  up$postcode[up$UPRN == 1] == "LS61RN" && up$district[up$UPRN == 1] == "LEEDS"
)

infill_syn <- data.frame(
  UPRN = c(10, 11, 12),
  house_number = c("24", "7", NA),
  street = c("FAKE STREET", "REAL ROAD", "OTHER WAY"),
  postcode = c("LS61RN", NA, NA), # 10: neighbour-inherited; 11/12: none
  postcode_source = c("gap_neighbours", NA, NA),
  district = c(NA, NA, "KEPT"),
  number_source = c("gap_guess", "osm", NA),
  number_guessed = c(TRUE, FALSE, FALSE),
  guess_between = c("22-26", NA, NA),
  stringsAsFactors = FALSE
)
places_syn <- data.frame(
  UPRN = c(10, 11),
  postcode = c("LS62AB", "WF76HP"), # 10 conflicts with the inherited postcode
  district = c("LEEDS", "WAKEFIELD")
)
ap <- suppressMessages(apply_uprn_places(infill_syn, places_syn))
check("nsul conflict withdraws gap guess", is.na(ap$house_number[ap$UPRN == 10]) && !ap$number_guessed[ap$UPRN == 10])
check("nsul postcode wins", ap$postcode[ap$UPRN == 10] == "LS62AB" && ap$postcode_source[ap$UPRN == 10] == "nsul")
check("nsul fills missing postcode", ap$postcode[ap$UPRN == 11] == "WF76HP")
check("osm number NOT withdrawn by postcode fill", ap$house_number[ap$UPRN == 11] == "7")
check("district kept when UPRN not in NSUL", ap$district[ap$UPRN == 12] == "KEPT")

single_syn <- build_postcode_singleton_lookup(
  nsul = data.frame(UPRN = c(20, 21, 22), postcode = c("AA11AA", "BB22BB", "BB22BB")),
  uprn_historical = data.frame(UPRN = c(20, 21, 22), LATITUDE = c(53, 54, 55), LONGITUDE = c(-1, -2, -3))
)
check("postcode singleton: true singleton kept", identical(single_syn$key, "AA11AA") && single_syn$UPRN == 20)
cat("NSUL integration: OK\n")

# --- OSM tag parsing ----------------------------------------------------------
tags <- '"addr:city"=>"Iver","addr:housenumber"=>"28","addr:postcode"=>"SL0 9BY","addr:street"=>"Ridge Way"'
check("osm tag: housenumber", extract_osm_tag(tags, "addr:housenumber") == "28")
check("osm tag: street", extract_osm_tag(tags, "addr:street") == "Ridge Way")
check("osm tag: missing key is NA", is.na(extract_osm_tag(tags, "addr:unit")))
cat("extract_osm_tag: OK\n")

# --- is_substation_address / broadened clean_spelling (substations.R) -------
check("substation detect: plain word", is_substation_address("Electricity Substation, Long Lane"))
check("substation detect: hyphenated no article", is_substation_address("The Sub-Station, Field Lane"))
check("substation detect: spaced plural", is_substation_address("Electricity Sub Stations, Field Lane"))
check("substation detect: negative", !is_substation_address("12 High Street, Leeds"))
check("substation detect: NA-safe", !is_substation_address(NA_character_))

check(
  "clean_spelling: bare 'Sub Station' now normalised (no article required)",
  clean_spelling("Sub Station, Field Lane") == "substation, Field Lane"
)
check(
  "clean_spelling: 'the electricity sub-station' normalised",
  clean_spelling("the electricity sub-station, Field Lane") == "substation, Field Lane"
)
check(
  "clean_spelling: plural normalised",
  clean_spelling("electricity sub-stations, Field Lane") == "substation, Field Lane"
)
cat("is_substation_address / clean_spelling substation variants: OK\n")

# --- build_substation_uprn_lookup (synthetic OSM + UPRN points, EPSG:27700) --
mk_poly <- function(x0, y0, x1, y1) {
  sf::st_polygon(list(matrix(c(x0, y0, x1, y0, x1, y1, x0, y1, x0, y0), ncol = 2, byrow = TRUE)))
}
osm_poly_syn <- sf::st_sf(
  osm_id = "p1", name = "Compound A",
  geometry = sf::st_sfc(mk_poly(0, 0, 10, 10), crs = 27700)
)
osm_pts_syn <- sf::st_sf(
  osm_id = c("n1", "n2"), name = c("Node A", "Node B"),
  geometry = sf::st_sfc(
    sf::st_point(c(100, 100)), sf::st_point(c(200, 200)),
    crs = 27700
  )
)
uprn_hist_syn <- data.frame(
  UPRN = c(10, 20, 21, 22),
  LATITUDE = c(53, 53, 53, 53), LONGITUDE = c(-1, -1, -1, -1),
  X_COORDINATE = c(5, 105, 205, 206), Y_COORDINATE = c(5, 100, 200, 200)
)
sub_uprn <- build_substation_uprn_lookup(osm_pts_syn, osm_poly_syn, uprn_hist_syn, point_tolerance_m = 20)
check("substation uprn: UPRN inside compound matched", 10 %in% sub_uprn$UPRN)
check("substation uprn: unambiguous nearby node matched", 20 %in% sub_uprn$UPRN)
check("substation uprn: two equally-near UPRNs -> ambiguous, dropped", !21 %in% sub_uprn$UPRN && !22 %in% sub_uprn$UPRN)
cat("build_substation_uprn_lookup: OK\n")

# --- build_substation_lookup (synthetic crosswalk) ---------------------------
sub_uprn_syn <- data.frame(
  UPRN = c(1, 2, 3, 4), osm_id = c("a", "b", "c", "d"),
  name = c("Sub A", "Sub B", "Sub C", "Sub D"),
  LATITUDE = c(53, 54, 55, 56), LONGITUDE = c(-1, -2, -3, -4)
)
uprn_places_syn <- data.frame(
  UPRN = c(1, 2, 3, 4),
  postcode = c("LS61RN", "WF76HP", "XX00XX", "LS62AB"),
  district = c("LEEDS", "WAKEFIELD", "SOMEWHERE", "LEEDS") # 1 & 4 share a district
)
uprn_usrn_syn <- data.frame(UPRN = c(1), USRN = c(100)) # only UPRN 1 has a USRN link
usrn_street_names_syn <- data.frame(USRN = c(100), street = c("Fake Street"))

sl <- build_substation_lookup(sub_uprn_syn, uprn_places_syn, uprn_usrn_syn, usrn_street_names_syn)
check(
  "substation lookup: street key for UPRN on a named USRN",
  any(sl$key == "LEEDS|FAKE STREET" & sl$UPRN == 1 & sl$match_quality == "medium")
)
check(
  "substation lookup: district-singleton fallback for a genuinely unique district",
  any(sl$key == "WAKEFIELD" & sl$UPRN == 2 & sl$match_quality == "low")
)
check(
  "substation lookup: district shared by two substation UPRNs is NOT a singleton",
  !"LEEDS" %in% sl$key
)
cat("build_substation_lookup: OK\n")

cat("\nAll tests passed.\n")
