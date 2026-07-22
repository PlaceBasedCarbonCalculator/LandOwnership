# Regression tests for the last-resort fuzzy free-text matching stage
# (pipeline/R/fuzzy_match.R). Pure functions + synthetic data only - no big
# data, no network, no Azure.
# Run from the repo root:
#   Rscript pipeline/tests/test_fuzzy_match.R
# Prints one line per test group and stops with an error on any failure.

suppressMessages({
  library(stringr)
  library(stringi)
  library(dplyr)
  library(data.table)
  library(stringdist)
})

setwd_root <- function() {
  if (file.exists("R/text_cleaning.R")) return(invisible())
  if (file.exists("../../R/text_cleaning.R")) return(setwd("../../"))
  stop("Run from the repository root.")
}
setwd_root()

source("R/address_functions.R")
source("pipeline/R/utils.R")
source("pipeline/R/fuzzy_match.R")
source("pipeline/R/match_free_sources.R")
source("pipeline/R/price_paid.R")

check <- function(label, cond) {
  if (!isTRUE(cond)) stop("FAILED: ", label, call. = FALSE)
  invisible(TRUE)
}

# --- build_fuzzy_lookup -------------------------------------------------------
# UPRN 1: known_uprn_addresses only, real numbered address.
# UPRN 2: uprn_infill only, inferred street.
# UPRN 3: uprn_infill only, no street - falls back to the inferred building name.
# UPRN 4: known_uprn_addresses only, a building-name-only address (no leading
#   house number, so its `street` column is NA, same as extract_street_name()
#   would give it) - `addr` (the raw single-line address, always present on
#   the real known_uprn_addresses table) is what building_norm is now parsed
#   from, closing what used to be an accepted gap (see the 2026-07 Kirklees
#   audit: ~31% of that district's queue had no leading house number and so
#   never got any fuzzy attempt at all).
known <- data.frame(
  UPRN = c(1, 4),
  LATITUDE = c(53.1, 53.4), LONGITUDE = c(-1.1, -1.4),
  addr = c("22 Acacia Avenue", "Ivy Cottage, Ackton Lane"),
  house_number = c("22", NA),
  street = c("Acacia Avenue", NA),
  postcode = c("LS61RN", "WF76HP"),
  stringsAsFactors = FALSE
)
infill <- data.frame(
  UPRN = c(2, 3),
  LATITUDE = c(53.2, 53.3), LONGITUDE = c(-1.2, -1.3),
  house_number = c("7", NA),
  street = c("Fake Street", NA),
  postcode = c("LS62AB", NA),
  building_name = c(NA, "Ivy Cottage"),
  district = c(NA, "SHEFFIELD"),
  stringsAsFactors = FALSE
)
places <- data.frame(
  UPRN = c(1, 4),
  postcode = c(NA, NA),
  district = c("LEEDS", "WAKEFIELD"),
  stringsAsFactors = FALSE
)
fl <- build_fuzzy_lookup(known, infill, places)
check("fuzzy lookup: known_uprn_addresses street kept", fl$street_norm[fl$UPRN == 1] == "ACACIA AVENUE")
check("fuzzy lookup: falls back to infill street", fl$street_norm[fl$UPRN == 2] == "FAKE STREET")
check("fuzzy lookup: falls back to infill building name", fl$street_norm[fl$UPRN == 3] == "IVY COTTAGE")
check("fuzzy lookup: building-name-only known address now included via building_norm", 4 %in% fl$UPRN)
check("fuzzy lookup: building_norm parsed from addr", fl$building_norm[fl$UPRN == 4] == "IVY COTTAGE")
check("fuzzy lookup: numbered address has no building_norm", is.na(fl$building_norm[fl$UPRN == 1]))
check("fuzzy lookup: postcode kept from known_uprn_addresses", fl$postcode[fl$UPRN == 1] == "LS61RN")
check("fuzzy lookup: district coalesced from uprn_places", fl$district[fl$UPRN == 1] == "LEEDS")
cat("build_fuzzy_lookup: OK\n")

# --- match_fuzzy_sources: postcode block, typo tolerated ---------------------
lookup1 <- data.frame(
  UPRN = c(101, 102), LATITUDE = c(53, 54), LONGITUDE = c(-1, -2),
  street_norm = c("SOMERSET ROAD", "OTHER ROAD"),
  house_number = c("101", "101"),
  postcode = c("HD11AA", "HD11AA"),
  district = c(NA, NA),
  stringsAsFactors = FALSE
)
unmatched1 <- data.frame(
  `Title Number` = "T1", AddressLine = "101 Somerset Rd, Huddersfield",
  PostalCode = "HD1 1AA", District = NA_character_,
  check.names = FALSE, stringsAsFactors = FALSE
)
res1 <- match_fuzzy_sources(unmatched1, lookup1, min_similarity = 0.85)
check("fuzzy match: abbreviation typo recovered via postcode block", nrow(res1$matched) == 1 && res1$matched$UPRN == 101)
check("fuzzy match: tagged least-trusted quality", res1$matched$match_quality == "fuzzy")
check("fuzzy match: similarity score present and below 1", res1$matched$fuzzy_score < 1 && res1$matched$fuzzy_score >= 0.85)

# --- match_fuzzy_sources: exact house number required, even if street matches perfectly ---
lookup2 <- data.frame(
  UPRN = 201, LATITUDE = 53, LONGITUDE = -1,
  street_norm = "SOMERSET ROAD", house_number = "103",
  postcode = "HD11AA", district = NA_character_,
  stringsAsFactors = FALSE
)
unmatched2 <- data.frame(
  `Title Number` = "T2", AddressLine = "101 Somerset Road, Huddersfield",
  PostalCode = "HD1 1AA", District = NA_character_,
  check.names = FALSE, stringsAsFactors = FALSE
)
res2 <- match_fuzzy_sources(unmatched2, lookup2, min_similarity = 0.85)
check("fuzzy match: different house number never matches", nrow(res2$matched) == 0)

# --- trust_unique_block: an unambiguous postcode+house-number pairing is
# trusted even when the street text is dissimilar (same trust level as
# match_free_sources.R's bare postcode+house-number stages) - but this
# trust does NOT extend to district blocks, which can span a whole city
# (see the district-block test below).
lookup2b <- data.frame(
  UPRN = 202, LATITUDE = 53, LONGITUDE = -1,
  street_norm = "COMPLETELY DIFFERENT LANE", house_number = "101",
  postcode = "HD11AA", district = NA_character_,
  stringsAsFactors = FALSE
)
unmatched2b <- data.frame(
  `Title Number` = "T2b", AddressLine = "101 Somerset Road, Huddersfield",
  PostalCode = "HD1 1AA", District = NA_character_,
  check.names = FALSE, stringsAsFactors = FALSE
)
res2b <- match_fuzzy_sources(unmatched2b, lookup2b, min_similarity = 0.85)
check(
  "fuzzy match: unique postcode+house-number block trusted despite dissimilar street",
  nrow(res2b$matched) == 1 && res2b$matched$UPRN == 202
)
cat("match_fuzzy_sources (postcode block): OK\n")

# --- match_fuzzy_sources: district block (no postcode), similarity too low --
lookup3 <- data.frame(
  UPRN = 301, LATITUDE = 53, LONGITUDE = -1,
  street_norm = "COMPLETELY DIFFERENT LANE", house_number = "348",
  postcode = NA_character_, district = "SHEFFIELD",
  stringsAsFactors = FALSE
)
unmatched3 <- data.frame(
  `Title Number` = "T3", AddressLine = "348 Lowedges Road, Sheffield",
  PostalCode = NA_character_, District = "SHEFFIELD",
  check.names = FALSE, stringsAsFactors = FALSE
)
res3 <- match_fuzzy_sources(unmatched3, lookup3, min_similarity = 0.85)
check("fuzzy match: dissimilar street rejected below threshold", nrow(res3$matched) == 0)

lookup4 <- data.frame(
  UPRN = 401, LATITUDE = 53, LONGITUDE = -1,
  street_norm = "LOWEDGES ROAD", house_number = "348",
  postcode = NA_character_, district = "SHEFFIELD",
  stringsAsFactors = FALSE
)
unmatched4 <- data.frame(
  `Title Number` = "T4",
  AddressLine = "348 Lowedges Road Lowedges Road and substation Lowedges Road, Sheffield",
  PostalCode = NA_character_, District = "SHEFFIELD",
  check.names = FALSE, stringsAsFactors = FALSE
)
res4 <- match_fuzzy_sources(unmatched4, lookup4, min_similarity = 0.85)
check("fuzzy match: district block recovers a clean street key", nrow(res4$matched) == 1 && res4$matched$UPRN == 401)
cat("match_fuzzy_sources (district block): OK\n")

# --- max_block: an oversized, ambiguous candidate group is skipped, not guessed ---
lookup5 <- data.frame(
  UPRN = 500 + seq_len(10), LATITUDE = rep(53, 10), LONGITUDE = rep(-1, 10),
  street_norm = paste("STREET", seq_len(10)),
  house_number = rep("5", 10),
  postcode = rep("AA11AA", 10), district = rep(NA_character_, 10),
  stringsAsFactors = FALSE
)
unmatched5 <- data.frame(
  `Title Number` = "T5", AddressLine = "5 Street 1, Town",
  PostalCode = "AA1 1AA", District = NA_character_,
  check.names = FALSE, stringsAsFactors = FALSE
)
res5 <- match_fuzzy_sources(unmatched5, lookup5, min_similarity = 0.5, max_block = 5)
check("fuzzy match: oversized candidate block skipped rather than guessed", nrow(res5$matched) == 0)

# --- max_block: an oversized block IS rescued when a street-prefix narrows
# it back under the cap (see the 2026-07 Kirklees audit: a common house
# number spread across a whole metropolitan borough routinely exceeds
# max_block at the district level, even when the true street name is an
# exact hit - this used to disable district-block matching entirely for any
# large city/borough) ---
lookup5b <- data.frame(
  UPRN = c(500 + seq_len(9), 600), LATITUDE = rep(53, 10), LONGITUDE = rep(-1, 10),
  street_norm = c(paste("OTHER STREET", seq_len(9)), "BENOMLEY CRESCENT"),
  house_number = rep("15", 10),
  postcode = rep("AA11AA", 10), district = rep(NA_character_, 10),
  stringsAsFactors = FALSE
)
unmatched5b <- data.frame(
  `Title Number` = "T5b", AddressLine = "15 Benomley Crescent, Town",
  PostalCode = "AA1 1AA", District = NA_character_,
  check.names = FALSE, stringsAsFactors = FALSE
)
res5b <- match_fuzzy_sources(unmatched5b, lookup5b, min_similarity = 0.9, max_block = 5)
check(
  "fuzzy match: oversized block rescued by street-prefix narrowing",
  nrow(res5b$matched) == 1 && res5b$matched$UPRN == 600
)
cat("max_block guard (+ prefix rescue): OK\n")

# --- rows with no house number and no usable building name are skipped -----
unmatched6 <- data.frame(
  `Title Number` = "T6", AddressLine = "Land at Fake Street, Town",
  PostalCode = NA_character_, District = "SHEFFIELD",
  check.names = FALSE, stringsAsFactors = FALSE
)
res6 <- match_fuzzy_sources(unmatched6, lookup4, min_similarity = 0.5)
check("fuzzy match: numberless row with no usable building name skipped", nrow(res6$matched) == 0 && nrow(res6$unmatched) == 1)
cat("numberless rows skipped: OK\n")

# --- building-name stage: a numberless address (hotel, named cottage, ...)
# now gets a fuzzy attempt via building_norm, blocked by postcode ------------
lookup7 <- data.frame(
  UPRN = 701, LATITUDE = 53, LONGITUDE = -1,
  street_norm = NA_character_, building_norm = "IVY COTTAGE",
  house_number = NA_character_,
  postcode = "WF76HP", district = NA_character_,
  stringsAsFactors = FALSE
)
unmatched7 <- data.frame(
  `Title Number` = "T7", AddressLine = "Ivy Cottage, Ackton Lane",
  PostalCode = "WF7 6HP", District = NA_character_,
  check.names = FALSE, stringsAsFactors = FALSE
)
res7 <- match_fuzzy_sources(unmatched7, lookup7, min_similarity = 0.9)
check(
  "fuzzy match: building-name row recovered via postcode block",
  nrow(res7$matched) == 1 && res7$matched$UPRN == 701 && res7$matched$fuzzy_block == "building_name"
)
cat("building-name stage: OK\n")

# --- postcode-mismatch -> district fallback: a postcode-bearing row whose
# postcode is WRONG for this house number (LR title text disagreeing with
# the true UPRN postcode - the exact Benomley Crescent pattern from the
# 2026-07 Kirklees audit) is no longer a dead end - it gets a second try at
# the district block -------------------------------------------------------
lookup8 <- data.frame(
  UPRN = 801, LATITUDE = 53, LONGITUDE = -1,
  street_norm = "BENOMLEY CRESCENT", house_number = "15",
  postcode = "HD58LT", district = "KIRKLEES", # true postcode differs from the query's
  stringsAsFactors = FALSE
)
unmatched8 <- data.frame(
  `Title Number` = "T8", AddressLine = "15 Benomley Crescent, Huddersfield",
  PostalCode = "HD5 8LU", District = "KIRKLEES", # Land Registry's (wrong) postcode
  check.names = FALSE, stringsAsFactors = FALSE
)
res8 <- match_fuzzy_sources(unmatched8, lookup8, min_similarity = 0.9)
check(
  "fuzzy match: wrong-postcode row recovered via district fallback",
  nrow(res8$matched) == 1 && res8$matched$UPRN == 801 && res8$matched$fuzzy_block == "postcode_mismatch_district"
)
cat("postcode-mismatch district fallback: OK\n")

# --- geographic fallback: same wrong-postcode scenario as above, but with
# NO district text at all - only postcode_history (the historical postcode
# centroid) can recover it, by proximity instead of by district text -------
lookup9 <- data.frame(
  UPRN = 901, LATITUDE = 53.6360, LONGITUDE = -1.7550,
  street_norm = "BENOMLEY CRESCENT", house_number = "15",
  postcode = "HD58LT", district = NA_character_,
  stringsAsFactors = FALSE
)
unmatched9 <- data.frame(
  `Title Number` = "T9", AddressLine = "15 Benomley Crescent, Huddersfield",
  PostalCode = "HD5 8LU", District = NA_character_,
  check.names = FALSE, stringsAsFactors = FALSE
)
postcode_history9 <- data.frame(
  postcode = "HD58LU", LATITUDE = 53.6362, LONGITUDE = -1.7552, last_seen_year = 2015,
  stringsAsFactors = FALSE
)
res9 <- match_fuzzy_sources(unmatched9, lookup9, postcode_history = postcode_history9, min_similarity = 0.9)
check(
  "fuzzy match: wrong/stale-postcode row recovered via geographic fallback",
  nrow(res9$matched) == 1 && res9$matched$UPRN == 901 && res9$matched$fuzzy_block == "postcode_geographic"
)
cat("geographic fallback: OK\n")

# --- rematch_price_paid_unmatched (Stream 3: Price Paid vs fuzzy_lookup) -----
fuzzy_lookup_pp <- data.frame(
  UPRN = c(701, 702), LATITUDE = c(53.5, 53.6), LONGITUDE = c(-1.5, -1.6),
  street_norm = c("SOMERSET ROAD", "OTHER ROAD"),
  house_number = c("11", "11"),
  postcode = c("HD11AA", "HD11AA"),
  district = c(NA_character_, NA_character_),
  stringsAsFactors = FALSE
)
pc_singleton_pp <- data.frame(
  key = "HD22BB", UPRN = 703, LATITUDE = 53.7, LONGITUDE = -1.7,
  match_source = "postcode_singleton_nsul", stringsAsFactors = FALSE
)
pp <- data.frame(
  transactionid = c("t1", "t2", "t3"),
  uprn = NA_character_,
  address1 = c("11", "Ivy Cottage", NA),
  address3 = c("Somerset Rd", "Ackton Lane", NA),
  postcode = c("HD1 1AA", "WF7 6HP", "HD2 2BB"),
  la = c(NA_character_, NA_character_, NA_character_),
  LATITUDE = NA_real_, LONGITUDE = NA_real_,
  stringsAsFactors = FALSE
)
res_pp <- rematch_price_paid_unmatched(pp, fuzzy_lookup_pp, pc_singleton_pp, min_similarity = 0.85)
check(
  "rematch: postcode-blocked fuzzy recovery (abbreviation tolerated)",
  res_pp$matched$uprn[res_pp$matched$transactionid == "t1"] == 701
)
check(
  "rematch: building-name PAON has no house number, skipped (accepted gap)",
  !"t2" %in% res_pp$matched$transactionid
)
check(
  "rematch: postcode-singleton fallback recovers a row with no usable street text",
  res_pp$matched$uprn[res_pp$matched$transactionid == "t3"] == 703 &&
    res_pp$matched$match_quality[res_pp$matched$transactionid == "t3"] == "medium"
)
cat("rematch_price_paid_unmatched: OK\n")

cat("\nAll tests passed.\n")
