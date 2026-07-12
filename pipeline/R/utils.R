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
  pc <- toupper(gsub("[^A-Za-z0-9]", "", postcode))
  pc[pc == ""] <- NA_character_
  house <- toupper(stringr::str_extract(address, "^[0-9]+[A-Za-z]?"))
  key <- paste(pc, house, sep = "|")
  key[is.na(pc) | is.na(house)] <- NA_character_
  key
}
