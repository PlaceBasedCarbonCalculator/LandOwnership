# Shared constants and helpers for the 2026 pipeline.

# UK postcode regex, used throughout the original R/ scripts - kept identical
# so behaviour doesn't silently drift.
postcode_rx <- "\\b(?:[A-Za-z][A-HJ-Ya-hj-y]?[0-9][0-9A-Za-z]? ?[0-9][A-Za-z]{2}|[Gg][Ii][Rr] ?0[Aa]{2})\\b"

# Optional row cap applied by the raw importers when
# options(pipeline.sample_n = <n>) is set, so the pipeline can be smoke
# tested end-to-end in minutes instead of hours. Leave unset for full runs.
pipeline_sample_n <- function() {
  getOption("pipeline.sample_n", default = NA_integer_)
}

# Separate, smaller cap for load_inspire_clean() (options(pipeline.sample_zips
# = <n>)): INSPIRE's per-LA grid-merge cleaning is slow enough that even a
# "sample_n rows" -sized cap on the number of LA zips processed would still
# be too slow for a quick smoke test.
pipeline_sample_zips <- function() {
  getOption("pipeline.sample_zips", default = NA_integer_)
}

sample_rows <- function(df) {
  n <- pipeline_sample_n()
  if (is.na(n) || nrow(df) <= n) {
    return(df)
  }
  df[seq_len(n), ]
}

# Split a `Property Address` column at postcode boundaries so a title with
# several postcodes becomes several rows, one per postcode segment (each
# retaining every other column from the source row). This is the same
# "walk the postcode match positions, cut the string between them" logic
# that was previously duplicated near-verbatim in prep_address_mulitple_postcode.R,
# prep_oversees_owners.R, prep_land_postcode.R and prep_UK_leashold_owners.R.
split_multi_postcode <- function(df, address_col = "Property Address") {
  if (nrow(df) == 0) {
    df$AddressLine <- character(0)
    df$PostalCode <- character(0)
    return(df)
  }

  split_locs <- stringr::str_locate_all(df[[address_col]], postcode_rx)
  df_list <- split(df, seq_len(nrow(df)))

  res <- vector("list", length(df_list))
  for (i in seq_along(df_list)) {
    df_sub <- df_list[[i]]
    split_sub <- split_locs[[i]]
    breaks <- c(split_sub[seq(1, nrow(split_sub) - 1), 2], nchar(df_sub[[address_col]]))
    starts <- c(1, breaks[seq(1, length(breaks) - 1)] + 1)
    sections <- matrix(c(starts, breaks), ncol = 2)

    pa <- vector("list", nrow(sections))
    for (j in seq_len(nrow(sections))) {
      pa[[j]] <- substr(df_sub[[address_col]], sections[j, 1], sections[j, 2])
    }
    pa <- unlist(pa)
    df2 <- df_sub[rep(1, times = length(pa)), ]
    df2$AddressLine <- pa
    df2$PostalCode <- stringr::str_match(df2$AddressLine, postcode_rx)[, 1]
    res[[i]] <- df2
  }

  res <- dplyr::bind_rows(res)

  # Extract the postcode out of the segment text, then tidy up the leading
  # joining words/punctuation left over from the cut (e.g. ", and Long Lane...")
  res$AddressLine <- stringr::str_replace(res$AddressLine, postcode_rx, "")
  res$AddressLine <- sub("^and\\s", "", res$AddressLine)
  res$AddressLine <- sub("^\\s\\sand\\s", "", res$AddressLine)
  res$AddressLine <- sub("^\\sand\\s", "", res$AddressLine)
  res$AddressLine <- sub("^[[:punct:]] and\\s", "", res$AddressLine)
  res$AddressLine <- sub("^\\s[[:punct:]] and\\s", "", res$AddressLine)
  res$AddressLine <- sub("^\\)\\s[[:punct:]] and\\s", "", res$AddressLine)
  res$AddressLine <- sub("^\\)[[:punct:]] ", "", res$AddressLine)
  res$AddressLine <- sub("^[[:punct:]]\\s", "", res$AddressLine)
  res$AddressLine <- sub("^[[:punct:]]", "", res$AddressLine)
  res$AddressLine <- sub("^\\s", "", res$AddressLine)
  res$AddressLine <- sub("\\s\\(\\)$", "", res$AddressLine)
  res$AddressLine <- sub("\\s\\($", "", res$AddressLine)

  res
}

# Normalise a Land Registry / EPC / Price Paid style address into a
# (postcode, leading house token) key for deterministic free-source
# matching in match_free_sources.R. Not a full address parse - just enough
# to disambiguate "10 Example Street" from "12 Example Street" at the same
# postcode without needing a geocoder. Returns NA (never matches anything)
# when either half is missing - a NA postcode or missing house number is
# too ambiguous to key on safely (every no-postcode row would otherwise
# collide with every other no-postcode row).
normalise_match_key <- function(address, postcode) {
  pc <- normalise_postcode(postcode)
  house <- extract_house_number(address)
  key <- paste(pc, house, sep = "|")
  key[is.na(pc) | is.na(house)] <- NA_character_
  key
}

normalise_postcode <- function(postcode) {
  pc <- toupper(gsub("[^A-Za-z0-9]", "", postcode))
  pc[pc == ""] <- NA_character_
  pc
}

# Leading house-number token: "22", "84A". NA when the address doesn't
# start with a number.
extract_house_number <- function(address) {
  toupper(stringr::str_extract(address, "^[0-9]+[A-Za-z]?\\b"))
}

# Street name from a "22 Acacia Avenue" style first address segment. Only
# trusted when the segment starts with a house number (otherwise the
# segment is a building/estate name, not a street). Returns NA otherwise.
extract_street_name <- function(address) {
  seg <- stringr::str_extract(address, "^[^,]+")
  has_number <- grepl("^\\s*[0-9]+[A-Za-z]?\\b", seg)
  street <- sub("^\\s*[0-9]+[A-Za-z]?\\b[\\s,]*", "", seg, perl = TRUE)
  street <- stringr::str_squish(street)
  street[!has_number | is.na(street) | nchar(street) < 4] <- NA_character_
  street
}

# Uppercase, alphanumeric-only normalisation for street / district names so
# "Fir Tree Gardens" and "FIR-TREE GARDENS" key identically.
normalise_name <- function(x) {
  x <- toupper(x)
  x <- gsub("[^A-Z0-9 ]", " ", x)
  x <- stringr::str_squish(x)
  x[x == ""] <- NA_character_
  x
}

# (postcode, building-name) key for addresses that start with a name rather
# than a number ("Ivy Cottage, Ackton Lane"). Generic leading words (FLAT,
# UNIT, LAND, ...) are refused - "Flat 3" is not a building name and would
# collide across every block at the postcode.
generic_leading_words <- c(
  "FLAT", "FLATS", "UNIT", "UNITS", "APARTMENT", "APARTMENTS", "PLOT",
  "LAND", "GARAGE", "GARAGES", "STORE", "STORES", "SUITE", "ROOM", "ROOMS",
  "PART", "SITE", "REAR", "FIRST", "SECOND", "THIRD", "GROUND", "BASEMENT",
  "THE SITE", "CAR", "PARKING", "AIRSPACE"
)

normalise_building_key <- function(address, postcode) {
  pc <- normalise_postcode(postcode)
  seg <- stringr::str_extract(address, "^[^,]+")
  name <- normalise_name(seg)
  first_word <- stringr::str_extract(name, "^[A-Z0-9]+")
  bad <- is.na(name) | nchar(name) < 4 |
    grepl("^[0-9]", name) |
    first_word %in% generic_leading_words
  key <- paste(pc, name, sep = "|")
  key[is.na(pc) | bad] <- NA_character_
  key
}

# (district, street, house number) key for rows with no postcode at all.
# All three parts are required - without a district a nationwide
# street+number key would collide constantly ("23 High Street").
street_number_key <- function(number, street, district) {
  n <- toupper(number)
  s <- normalise_name(street)
  d <- normalise_name(district)
  key <- paste(d, s, n, sep = "|")
  key[is.na(n) | is.na(s) | is.na(d)] <- NA_character_
  key
}

# Final tidy applied to every AddressLine after all cleaning/splitting:
# guarantees no internal @TAGs, empty brackets, leading legal glue
# ("the site of", "being", "adjoining", "north of", ...) or dangling
# punctuation reach the free-matching stage or the paid geocoder. The
# pre-tidy string is kept by the caller (AddressLine_tagged) because the
# tags encode what kind of title the text described.
final_address_tidy <- function(x) {
  ci <- stringi::stri_opts_regex(case_insensitive = TRUE)
  x <- stringi::stri_replace_all_regex(x, "@[A-Za-z]+", " ")
  x <- stringi::stri_replace_all_regex(x, "\\(\\s*\\)", " ")
  # runs of commas/semicolons left behind by phrase removal
  x <- stringi::stri_replace_all_regex(x, "\\s*[,;]\\s*(?=[,;])", "")
  glue_rx <- paste0(
    "^[\\s\\p{P}]*",
    "((and|of|at|on|to|off|being|adjoining|adjacent to|",
    "the site of|site of|the rear of|rear of|land at|",
    "forming|formerly|",
    "(the |on the )?(north|south|east|west)([ -](east|west))?(ern)?",
    "( side| end| corner)? of)\\b[\\s\\p{P}]*)+"
  )
  for (i in 1:3) {
    x <- stringi::stri_replace_first_regex(x, glue_rx, "", opts_regex = ci)
  }
  # trailing orphan joining words
  x <- stringi::stri_replace_last_regex(
    x, "([\\s\\p{P}]+\\b(and|of|at|on|being|to|the)\\b)+[\\s\\p{P}]*$", "",
    opts_regex = ci
  )
  x <- stringi::stri_replace_all_regex(x, "\\s+([,;.])", "$1")
  x <- stringr::str_squish(x)
  x <- sub("^[[:punct:][:space:]]+", "", x)
  x <- sub("[,;[:space:]]+$", "", x)
  x
}
