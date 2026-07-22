# Shared constants and helpers for the 2026 pipeline.

# UK postcode regex, used throughout the original R/ scripts - kept identical
# so behaviour doesn't silently drift.
postcode_rx <- "\\b(?:[A-Za-z][A-HJ-Ya-hj-y]?[0-9][0-9A-Za-z]? ?[0-9][A-Za-z]{2}|[Gg][Ii][Rr] ?0[Aa]{2})\\b"

# Anchored version of postcode_rx: TRUE only when the ENTIRE (trimmed)
# string is a well-formed UK postcode, not merely contains one somewhere
# inside a longer string (that's what postcode_rx itself is for - see
# classify_geocodability(), split_multi_postcode()). Used by the
# coverage-audit checks in audit_uprn_coverage.R to flag missing or
# malformed postcode values (PostalCode, NSUL PCDS, ...) as a count instead
# of only surfacing as a silent match failure several stages downstream.
postcode_rx_full <- "^(?:[A-Za-z][A-HJ-Ya-hj-y]?[0-9][0-9A-Za-z]? ?[0-9][A-Za-z]{2}|[Gg][Ii][Rr] ?0[Aa]{2})$"
is_valid_postcode <- function(x) {
  x <- trimws(x)
  !is.na(x) & x != "" & grepl(postcode_rx_full, x)
}

# Split a `Property Address` column at postcode boundaries so a title with
# several postcodes becomes several rows, one per postcode segment (each
# retaining every other column from the source row). This is the same
# "walk the postcode match positions, cut the string between them" logic
# that was previously duplicated near-verbatim in prep_address_mulitple_postcode.R,
# prep_oversees_owners.R, prep_land_postcode.R and prep_UK_leashold_owners.R.
# The per-row cut-and-rebuild is independent row to row, so it runs under
# furrr::future_map() - relies on the caller (build_split_addresses()) having
# set a future::plan(); falls back to sequential (identical to the old
# for-loop) if none is set.
split_multi_postcode <- function(df, address_col = "Property Address") {
  if (nrow(df) == 0) {
    df$AddressLine <- character(0)
    df$PostalCode <- character(0)
    return(df)
  }

  split_locs <- stringr::str_locate_all(df[[address_col]], postcode_rx)
  df_list <- split(df, seq_len(nrow(df)))

  res <- furrr::future_map(seq_along(df_list), function(i) {
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
    df2
  })

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

# Street name from a "22 Acacia Avenue" or "22, Acacia Avenue" style leading
# address segment. Only trusted when the address starts with a house number
# (otherwise it's a building/estate name, not a street). Returns NA
# otherwise. EPC/Price-Paid addresses very often put a comma directly after
# the house number ("6, Leyfield Road") - if the first comma-delimited
# segment turns out to be nothing but that number, the street name is
# actually the NEXT segment, not an empty string.
extract_street_name <- function(address) {
  seg <- stringr::str_extract(address, "^[^,]+")
  has_number <- grepl("^\\s*[0-9]+[A-Za-z]?\\b", seg)
  street <- sub("^\\s*[0-9]+[A-Za-z]?\\b[\\s,]*", "", seg, perl = TRUE)

  # Registry boilerplate describing the numbering, not the street, placed
  # directly after the house number - "2 (Consecutive numbers) Moorcrest
  # Road", "211 (exclusive), Old Street" (see the 2026-07-21 Kirklees audit:
  # this was gluing "(Consecutive numbers)" onto the front of the parsed
  # street name, so "KIRKLEES|MOORCREST ROAD|7" - sitting right there in
  # street_lookup from EPC data - could never be keyed to). A real street
  # name never starts with an opening bracket immediately after the house
  # number, so this is safe to strip unconditionally.
  street <- sub("^\\([^()]*\\)\\s*", "", street, perl = TRUE)
  street <- stringr::str_squish(street)

  # Titles that bundle a numbered frontage with adjoining land or a
  # substation restate the street name two or three times ("Lowedges Road
  # Lowedges Road and substation Lowedges Road") once the connecting
  # boilerplate ("land on the north side of", "an electricity") is stripped
  # elsewhere. Collapse an immediately self-repeating leading phrase back to
  # one copy so the street key still matches known addresses; an ordinary,
  # non-repeating street name never matches this pattern and passes through
  # unchanged.
  street <- sub("^(.*?)\\s+\\1\\b.*$", "\\1", street, perl = TRUE)

  bare_number_seg <- has_number & !is.na(street) & street == ""
  if (any(bare_number_seg)) {
    next_seg <- stringr::str_match(address, "^[^,]+,\\s*([^,]+)")[, 2]
    street[bare_number_seg] <- stringr::str_squish(next_seg[bare_number_seg])
  }

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

# Building/institution name from a "Ivy Cottage, Ackton Lane" or "Lodge
# Hotel, 48 Birkby Lodge Road" style leading address segment - the same
# segment normalise_building_key() keys on, factored out so build_fuzzy_lookup()
# (fuzzy_match.R) can also index UPRNs by building name, not just street.
# Returns the ORIGINAL-case text (mirroring extract_street_name()'s
# contract - callers normalise_name() it themselves), or NA when the
# leading segment starts with a digit (that's a house number, not a name),
# is too short to be distinctive, or starts with a generic word
# (FLAT/UNIT/LAND/... - "Flat 3" is not a building name and would collide
# across every block at the postcode).
extract_building_name <- function(address) {
  seg <- stringr::str_extract(address, "^[^,]+")
  name_norm <- normalise_name(seg)
  first_word <- stringr::str_extract(name_norm, "^[A-Z0-9]+")
  bad <- is.na(name_norm) | nchar(name_norm) < 4 |
    grepl("^[0-9]", name_norm) |
    first_word %in% generic_leading_words
  out <- stringr::str_squish(seg)
  out[bad] <- NA_character_
  out
}

# Road-suffix words used to spot a street name BURIED anywhere inside an
# address, not just its leading segment - same list R/text_cleaning.R's
# analyise_text() uses to spot road names generally. Used by
# extract_buried_street() below (match_free_sources.R's last-resort
# street-centroid stage) to catch titles like "the paddock north of Foo
# Street" where positional/legal glue final_address_tidy() didn't fully
# strip leaves "the paddock" as the leading comma-segment - which is what
# extract_street_name()/the ordinary street-centroid stages key on - while
# the real street name, "Foo Street", sits later in the string.
road_suffix_words <- c(
  "Road", "Close", "Lane", "Street", "Drive", "Avenue", "Way", "Court",
  "Place", "Gardens", "Crescent", "Grove", "Hill", "Park", "Terrace",
  "Green", "Walk", "View", "Mews", "Bridge", "Rise", "Square"
)
road_suffix_rx <- paste0(
  "[A-Z][A-Za-z'-]*(\\s+[A-Z][A-Za-z'-]*){0,4}\\s+(",
  paste(road_suffix_words, collapse = "|"), ")\\b"
)

# The LAST "<Capitalised words> <road suffix>" phrase in `address` (NA if
# none) - positional/legal glue ("land to the rear of", "north of",
# "adjoining") almost always comes BEFORE the real street name in Land
# Registry text, essentially never after it, so the last match is the best
# single guess at the actual street being described.
extract_buried_street <- function(address) {
  m <- stringr::str_extract_all(address, road_suffix_rx)
  vapply(m, function(x) if (length(x) == 0) NA_character_ else x[length(x)], character(1))
}

normalise_building_key <- function(address, postcode) {
  pc <- normalise_postcode(postcode)
  name <- normalise_name(extract_building_name(address))
  key <- paste(pc, name, sep = "|")
  key[is.na(pc) | is.na(name)] <- NA_character_
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
  # a stray unmatched bracket, e.g. "10 Sunnydene, )" - happens when the
  # source register text itself has a mismatched paren next to a postcode
  # ("...Sunnydene, )LS14 6AL)..." is a genuine CCOD/OCOD typo) and the
  # postcode-boundary split in split_multi_postcode() cuts right through it,
  # leaving the orphan bracket on one side.
  unbalanced <- stringi::stri_count_fixed(x, "(") != stringi::stri_count_fixed(x, ")")
  unbalanced[is.na(unbalanced)] <- FALSE
  x[unbalanced] <- stringi::stri_replace_all_regex(
    x[unbalanced], "^[\\s,;]*\\(+|\\)+[\\s,;]*$", ""
  )
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
