# Stage 6b/6c shared resource: the single canonical address lookup for
# free-text fuzzy matching, used by BOTH the CCOD/OCOD last-resort stage
# below (Stage 6f) and the Price Paid rematch in price_paid.R (Stage 6c).
# The exact-key cascades in match_free_sources.R / (formerly) price_paid.R
# require postcode+number, postcode+building-name or district+street+number
# to match EXACTLY. Real Land Registry / Price Paid text varies in ways an
# exact key can't absorb ("Rd" vs "Road", a trailing town name, a stray
# typo) even when a UPRN with that same street/number is sitting right there
# in known_uprn_addresses/uprn_infill. This stage tags every match with the
# least-trusted quality tier, "fuzzy" (below "guess"), plus the similarity
# score, so conservative consumers can filter it straight back out.
#
# Deliberately NOT folded into match_free_sources()'s exact stages: those
# are well-tested and encode real trust-level guarantees; this is fuzzier,
# newer and easier to disable on its own if the matches it recovers prove
# too noisy in practice.

# One row per UPRN with a usable text description to fuzzy-match against:
# known_uprn_addresses' parsed street/house_number/postcode (a real address
# line) where present, else uprn_infill's inferred street/building
# name/house_number/postcode/district. Built directly from these two pass-1
# tables - NOT from uprn_all_addresses - so this lookup is available before
# Stage 6d's Price Paid rematch runs (uprn_all_addresses is only published
# after that rematch completes; building the lookup from it here would be
# circular). known_uprn_addresses and uprn_infill are disjoint by UPRN by
# construction (build_infill_candidates() only ever includes UPRNs NOT in
# known_uprn_addresses), so this is a plain bind_rows, not a coalesce-join.
# uprn_places supplies postcode/district for whichever rows didn't get one
# from their own address source, same as uprn_master.R's postcode_nsul/
# district_nsul coalesce.
build_fuzzy_lookup <- function(known_uprn_addresses, uprn_infill, uprn_places) {
  kn <- known_uprn_addresses
  inf <- uprn_infill
  pl <- data.table::as.data.table(uprn_places)[, .(UPRN, postcode_nsul = postcode, district_nsul = district)]
  pl <- unique(pl, by = "UPRN")

  kn_rows <- data.frame(
    UPRN = kn$UPRN, LATITUDE = kn$LATITUDE, LONGITUDE = kn$LONGITUDE,
    street = kn$street, house_number = kn$house_number,
    building = extract_building_name(kn$addr),
    postcode = kn$postcode, district = NA_character_,
    stringsAsFactors = FALSE
  )
  inf_street <- ifelse(!is.na(inf$street), inf$street, inf$building_name)
  inf_rows <- data.frame(
    UPRN = inf$UPRN, LATITUDE = inf$LATITUDE, LONGITUDE = inf$LONGITUDE,
    street = inf_street, house_number = inf$house_number,
    building = extract_building_name(inf$building_name),
    postcode = inf$postcode, district = inf$district,
    stringsAsFactors = FALSE
  )

  combined <- dplyr::bind_rows(kn_rows, inf_rows)
  combined <- dplyr::left_join(combined, as.data.frame(pl), by = "UPRN")
  postcode <- dplyr::coalesce(combined$postcode, combined$postcode_nsul)
  district <- dplyr::coalesce(combined$district, combined$district_nsul)

  lookup <- data.frame(
    UPRN = combined$UPRN,
    LATITUDE = combined$LATITUDE, LONGITUDE = combined$LONGITUDE,
    street_norm = normalise_name(combined$street),
    # Building/institution name (e.g. "IVY COTTAGE"), independent of
    # street_norm - lets match_fuzzy_sources() fuzzy-match addresses that
    # never carry a leading house number at all (hotels, named cottages,
    # schools - see the 2026-07 Kirklees audit, ~31% of that district's
    # queue had no usable street+house-number text and so never got ANY
    # fuzzy attempt before this).
    building_norm = normalise_name(combined$building),
    house_number = toupper(combined$house_number),
    postcode = normalise_postcode(postcode),
    district = normalise_name(district),
    stringsAsFactors = FALSE
  )
  lookup[!is.na(lookup$street_norm) | !is.na(lookup$building_norm), ]
}

# Block `rows` against `lookup` on an exact (block, house-number) join -
# `rows_key`/`lookup_key` name the block column (postcode or district) on
# each side, since the queue's own columns aren't named the same as
# fuzzy_lookup's. Requiring the house number to match exactly before any
# fuzzy comparison runs keeps every candidate set small without an
# artificial global cap: a given house number within one postcode or
# district is rarely shared by more than a handful of UPRNs, unlike the
# postcode/district alone (which can be a whole city). Text is compared with
# Jaro-Winkler similarity (stringdist::stringsim()); the best-scoring
# candidate per row above `min_similarity` wins.
#
# `text_col`/`lookup_text_col` pick which pair of columns to fuzzy-compare -
# defaults to street_q vs street_norm, but the building-name stage in
# match_fuzzy_sources() passes building_q/building_norm instead so this one
# function serves both. `require_number` controls whether the join also
# requires an exact house-number match (`number_col`/`lookup_number_col`) -
# FALSE for the building-name stage, since a named building doesn't have a
# house number to key on.
#
# A (block, house-number) group that exceeds `max_block` candidates used to
# be dropped outright, before its text was ever compared - see the 2026-07
# Kirklees audit: a common house number spread across a whole metropolitan
# borough routinely has 1,000+ candidates at the district level, so this
# silently disabled district-block matching for every large city/borough,
# regardless of how distinctive the true street name was. Typos/abbreviations
# essentially never change the first few characters of a street or building
# name, so an oversized block is joined on (block, house-number, text-prefix)
# instead of just (block, house-number) - narrowing it down to just the
# candidates sharing the query's text prefix (`prefix_len` characters)
# WITHOUT ever materialising its full cross-product first. An earlier version
# of this function joined on (block, house-number) alone for every block,
# then filtered oversized ones down by prefix afterwards - that still built
# the complete, huge cross-product for every oversized block before
# discarding most of it, which is what made this stage effectively never
# finish at national scale (a common house number across a whole
# metropolitan borough can have 1,000s of UPRNs) - see the 2026-07-21
# fuzzy_match runtime incident. Block size is computed on the LOOKUP side
# alone (independent of how many query rows hit it), so which blocks need
# the narrower join is known before any join happens.
#
# `trust_unique_block`: when a (block, house-number) pairing has exactly
# ONE candidate, accept it even if its text scores below `min_similarity` -
# same trust level match_free_sources.R already gives a bare
# postcode+house-number key against epc_lookup/price_paid_lookup (no street
# comparison at all) or postcode_singleton_lookup (no street text needed at
# all). Only meaningful for postcode-style blocks - a UK postcode averages
# ~15 addresses, so an exact house number within one is normally unique. NOT
# used for district-style blocks: a district can be a whole city, so a
# single surviving (district, house-number) candidate is still just a
# coincidence worth corroborating with text similarity, not a strong
# identifier on its own.
fuzzy_match_block <- function(rows, lookup, rows_key, lookup_key, min_similarity, max_block,
                              trust_unique_block = FALSE, prefix_len = 4,
                              require_number = TRUE,
                              number_col = "house_number_q", lookup_number_col = "house_number",
                              text_col = "street_q", lookup_text_col = "street_norm") {
  empty <- data.frame(
    row_id = integer(0), UPRN = numeric(0), LATITUDE = numeric(0),
    LONGITUDE = numeric(0), score = numeric(0)
  )
  if (nrow(rows) == 0) {
    return(empty)
  }

  if (require_number) {
    rows_dt <- data.table::as.data.table(rows[, c("row_id", rows_key, number_col, text_col)])
    data.table::setnames(rows_dt, c(rows_key, number_col, text_col), c("join_key", "join_number", "text_q"))
    lookup_dt <- data.table::as.data.table(lookup[, c("UPRN", "LATITUDE", "LONGITUDE", lookup_key, lookup_number_col, lookup_text_col)])
    data.table::setnames(lookup_dt, c(lookup_key, lookup_number_col, lookup_text_col), c("join_key", "join_number", "text_norm"))
    lookup_dt <- lookup_dt[!is.na(join_key) & !is.na(join_number) & !is.na(text_norm)]
    join_cols <- c("join_key", "join_number")
  } else {
    rows_dt <- data.table::as.data.table(rows[, c("row_id", rows_key, text_col)])
    data.table::setnames(rows_dt, c(rows_key, text_col), c("join_key", "text_q"))
    lookup_dt <- data.table::as.data.table(lookup[, c("UPRN", "LATITUDE", "LONGITUDE", lookup_key, lookup_text_col)])
    data.table::setnames(lookup_dt, c(lookup_key, lookup_text_col), c("join_key", "text_norm"))
    lookup_dt <- lookup_dt[!is.na(join_key) & !is.na(text_norm)]
    join_cols <- "join_key"
  }
  if (nrow(rows_dt) == 0 || nrow(lookup_dt) == 0) {
    return(empty)
  }

  block_n <- lookup_dt[, .N, by = join_cols]
  lookup_dt <- merge(lookup_dt, block_n, by = join_cols)
  small_lookup <- lookup_dt[N <= max_block]
  small_lookup[, N := NULL]
  big_lookup <- lookup_dt[N > max_block]
  big_lookup[, N := NULL]

  rows_dt[, prefix := substr(text_q, 1, prefix_len)]

  pairs_list <- list()
  if (nrow(small_lookup) > 0) {
    pairs_list[[length(pairs_list) + 1]] <- merge(rows_dt, small_lookup, by = join_cols, allow.cartesian = TRUE)
  }
  if (nrow(big_lookup) > 0) {
    big_lookup[, prefix := substr(text_norm, 1, prefix_len)]
    pairs_list[[length(pairs_list) + 1]] <- merge(rows_dt, big_lookup, by = c(join_cols, "prefix"), allow.cartesian = TRUE)
  }
  if (length(pairs_list) == 0) {
    return(empty)
  }
  pairs <- data.table::rbindlist(pairs_list, use.names = TRUE, fill = TRUE)
  if (nrow(pairs) == 0) {
    return(empty)
  }

  n_candidates <- NULL # data.table NSE - avoid an R CMD check "no visible binding" note
  pairs[, n_candidates := .N, by = row_id]
  pairs <- pairs[n_candidates <= max_block]
  if (nrow(pairs) == 0) {
    return(empty)
  }

  score <- NULL
  pairs[, score := stringdist::stringsim(text_q, text_norm, method = "jw")]
  pairs <- if (trust_unique_block) {
    pairs[n_candidates == 1 | score >= min_similarity]
  } else {
    pairs[score >= min_similarity]
  }
  if (nrow(pairs) == 0) {
    return(empty)
  }

  data.table::setorder(pairs, row_id, -score)
  best <- pairs[!duplicated(pairs, by = "row_id"), .(row_id, UPRN, LATITUDE, LONGITUDE, score)]
  as.data.frame(best)
}

# Geographic fallback for rows whose postcode text found NO candidates at
# the exact (postcode, house-number) join in fuzzy_match_block() - i.e. the
# postcode Land Registry recorded is either wrong for that specific house
# number (see the 2026-07 Kirklees audit: "15 Benomley Crescent" registered
# under HD5 8LU when the true UPRN sits in HD5 8LT - the title text splits
# odd/even numbers between two postcodes the wrong way round) or has since
# been split/retired outright. Either way the postcode STRING can still be
# resolved to a rough location via `postcode_history` (built from current +
# historical OS postcode-polygon centroids - see
# build_postcode_history_lookup(), pipeline/R/postcode_history.R), and real
# UPRN candidates found by PROXIMITY to that point instead of by requiring
# the postcode/district text to match.
#
# Deliberately does NOT build `sf` geometry objects for `lookup` (potentially
# every UPRN in England & Wales, ~35M rows) - an earlier version used
# sf::st_is_within_distance(), which never finished after running for a full
# day (see the 2026-07-21 fuzzy_match runtime incident). Benchmarked
# alternative: even nngeo::st_nn() - which also uses a RANN kd-tree
# internally, same idea - still requires both sides as `sf` objects first,
# and constructing that for the full lookup alone took minutes at a fifth of
# this scale. Instead: sf::sf_project() transforms plain coordinate MATRICES
# to British National Grid (so Euclidean distance = metres, no geodesic
# correction needed) without ever creating `sf` geometries, and RANN::nn2()
# runs the kd-tree search directly on those matrices - together under 90
# seconds at the full ~35M-row national scale (measured 2026-07-21).
# `k_nearest` bounds how many nearest candidates come back per row before
# the max_dist_m/house-number/text-score filtering below narrows them
# further - set well above plausible same-radius address density (a dense
# UK street can have hundreds of addresses within `max_dist_m`) so the true
# match is never excluded just for not being among the closest handful.
fuzzy_match_geographic <- function(rows, lookup, postcode_history, min_similarity,
                                   max_dist_m = 750, k_nearest = 300) {
  empty <- data.frame(
    row_id = integer(0), UPRN = numeric(0), LATITUDE = numeric(0),
    LONGITUDE = numeric(0), score = numeric(0)
  )
  if (nrow(rows) == 0 || is.null(postcode_history) || nrow(postcode_history) == 0) {
    return(empty)
  }

  hist_dt <- data.table::as.data.table(postcode_history)[, .(postcode, hist_lat = LATITUDE, hist_lon = LONGITUDE)]
  rows_dt <- data.table::as.data.table(rows[, c("row_id", "postcode_q", "house_number_q", "street_q")])
  rows_dt <- merge(rows_dt, hist_dt, by.x = "postcode_q", by.y = "postcode")
  if (nrow(rows_dt) == 0) {
    return(empty)
  }

  lk <- lookup[!is.na(lookup$LATITUDE) & !is.na(lookup$LONGITUDE) & !is.na(lookup$house_number),
    c("UPRN", "LATITUDE", "LONGITUDE", "house_number", "street_norm")
  ]
  if (nrow(lk) == 0) {
    return(empty)
  }

  lk_xy <- sf::sf_project(from = "EPSG:4326", to = "EPSG:27700", pts = as.matrix(lk[, c("LONGITUDE", "LATITUDE")]))
  q_xy <- sf::sf_project(from = "EPSG:4326", to = "EPSG:27700", pts = as.matrix(rows_dt[, c("hist_lon", "hist_lat")]))

  nn <- RANN::nn2(data = lk_xy, query = q_xy, k = min(k_nearest, nrow(lk_xy)))

  out <- vector("list", nrow(rows_dt))
  for (i in seq_len(nrow(rows_dt))) {
    idx <- nn$nn.idx[i, ]
    d <- nn$nn.dists[i, ]
    keep <- idx > 0 & d <= max_dist_m
    if (!any(keep)) next
    cand <- lk[idx[keep], ]
    cand <- cand[cand$house_number == rows_dt$house_number_q[i], ]
    if (nrow(cand) == 0) next
    cand$score <- stringdist::stringsim(rows_dt$street_q[i], cand$street_norm, method = "jw")
    best_i <- which.max(cand$score)
    if (cand$score[best_i] >= min_similarity) {
      out[[i]] <- data.frame(
        row_id = rows_dt$row_id[i], UPRN = cand$UPRN[best_i],
        LATITUDE = cand$LATITUDE[best_i], LONGITUDE = cand$LONGITUDE[best_i],
        score = cand$score[best_i]
      )
    }
  }
  dplyr::bind_rows(out)
}

# `unmatched` is match_free_sources()$unmatched - same AddressLine/
# PostalCode/District/... shape used throughout this pipeline. `postcode_history`
# (optional - build_postcode_history_lookup(), pipeline/R/postcode_history.R)
# enables the geographic fallback stage; pass NULL to skip it (e.g. in tests).
#
# Stages, in order, per the 2026-07 Kirklees audit of why "obviously
# matchable" addresses were still failing:
#   1. postcode block (trust_unique_block) - as before.
#   2. geographic fallback - for postcode-bearing rows stage 1 found ZERO
#      candidates for (the postcode is wrong for this house number, or has
#      since been split/retired - see fuzzy_match_geographic()).
#   3. district block, for whatever stage 2 didn't recover ("postcode
#      mismatch" rows) plus rows that never had a postcode at all.
#   4. building-name block (postcode then district) - for rows with NO
#      house number at all (hotels, named cottages, schools - previously
#      skipped entirely, since extract_house_number()/extract_street_name()
#      both require a leading digit).
match_fuzzy_sources <- function(unmatched, fuzzy_lookup, postcode_history = NULL,
                                min_similarity = 0.9, max_block = 500,
                                geo_max_dist_m = 750, building_min_similarity = 0.92) {
  orig_cols <- names(unmatched)
  if (nrow(unmatched) == 0 || nrow(fuzzy_lookup) == 0) {
    return(list(matched = unmatched[0, ], unmatched = unmatched))
  }

  rem <- unmatched
  rem$row_id <- seq_len(nrow(rem))
  rem$house_number_q <- toupper(extract_house_number(rem$AddressLine))
  # normalise_name() the same way fuzzy_lookup's street_norm is normalised
  # (uppercase, alphanumeric only) so stringdist compares like with like -
  # otherwise a plain case difference ("Rd" vs lookup's "ROAD" casing) would
  # be scored as a mismatch before the abbreviation itself is even compared.
  rem$street_q <- normalise_name(extract_street_name(rem$AddressLine))
  rem$postcode_q <- normalise_postcode(rem$PostalCode)
  rem$district_q <- normalise_name(rem$District)

  has_text <- !is.na(rem$street_q) & !is.na(rem$house_number_q)
  has_pc <- has_text & !is.na(rem$postcode_q)
  has_district_only <- has_text & !has_pc & !is.na(rem$district_q)

  best_pc <- fuzzy_match_block(rem[has_pc, ], fuzzy_lookup, "postcode_q", "postcode", min_similarity, max_block, trust_unique_block = TRUE)
  if (nrow(best_pc) > 0) best_pc$fuzzy_block <- "postcode"

  pc_rows <- rem[has_pc, ]
  pc_failed <- pc_rows[!pc_rows$row_id %in% best_pc$row_id, ]

  best_geo <- fuzzy_match_geographic(pc_failed, fuzzy_lookup, postcode_history, min_similarity, max_dist_m = geo_max_dist_m)
  if (nrow(best_geo) > 0) {
    best_geo$fuzzy_block <- "postcode_geographic"
    pc_failed <- pc_failed[!pc_failed$row_id %in% best_geo$row_id, ]
  }

  # Rows the postcode block rejected outright (wrong postcode for this house
  # number - see the file header) get one more try at the coarser district
  # block, exactly like a postcode-less row would - but this is weaker
  # evidence than a lone postcode+house-number match, so trust_unique_block
  # stays FALSE here same as the plain district stage below.
  pc_failed <- pc_failed[!is.na(pc_failed$district_q), ]
  best_pc_district <- fuzzy_match_block(pc_failed, fuzzy_lookup, "district_q", "district", min_similarity, max_block)
  if (nrow(best_pc_district) > 0) best_pc_district$fuzzy_block <- "postcode_mismatch_district"

  best_district <- fuzzy_match_block(rem[has_district_only, ], fuzzy_lookup, "district_q", "district", min_similarity, max_block)
  if (nrow(best_district) > 0) best_district$fuzzy_block <- "district"

  best <- dplyr::bind_rows(best_pc, best_geo, best_pc_district, best_district)

  # Building-name stage: rows with no usable house number at all never had
  # ANY fuzzy path before this. Block by postcode first (a lone building
  # name within one postcode is trusted the same way a lone
  # postcode+house-number pairing is above), else district.
  no_number <- rem[!has_text, ]
  no_number <- no_number[!no_number$row_id %in% best$row_id, ]
  best_building <- data.frame(row_id = integer(0), UPRN = numeric(0), LATITUDE = numeric(0), LONGITUDE = numeric(0), score = numeric(0))
  if (nrow(no_number) > 0 && "building_norm" %in% names(fuzzy_lookup)) {
    no_number$building_q <- normalise_name(extract_building_name(no_number$AddressLine))
    has_building <- !is.na(no_number$building_q)
    bpc <- no_number[has_building & !is.na(no_number$postcode_q), ]
    bdist <- no_number[has_building & is.na(no_number$postcode_q) & !is.na(no_number$district_q), ]
    b_pc <- fuzzy_match_block(bpc, fuzzy_lookup, "postcode_q", "postcode", building_min_similarity, max_block,
      trust_unique_block = TRUE, require_number = FALSE, text_col = "building_q", lookup_text_col = "building_norm"
    )
    b_dist <- fuzzy_match_block(bdist, fuzzy_lookup, "district_q", "district", building_min_similarity, max_block,
      require_number = FALSE, text_col = "building_q", lookup_text_col = "building_norm"
    )
    best_building <- dplyr::bind_rows(b_pc, b_dist)
    if (nrow(best_building) > 0) best_building$fuzzy_block <- "building_name"
  }

  best <- dplyr::bind_rows(best, best_building)

  if (nrow(best) == 0) {
    message("0 of ", nrow(unmatched), " addresses fuzzy-matched (", nrow(unmatched), " left for the paid queue).")
    return(list(matched = unmatched[0, ], unmatched = unmatched))
  }

  matched <- unmatched[best$row_id, orig_cols]
  matched$UPRN <- best$UPRN
  matched$LATITUDE <- best$LATITUDE
  matched$LONGITUDE <- best$LONGITUDE
  matched$source <- "fuzzy_match"
  matched$match_quality <- "fuzzy"
  matched$fuzzy_score <- round(best$score, 3)
  matched$fuzzy_block <- best$fuzzy_block

  unmatched_out <- unmatched[-best$row_id, orig_cols]

  message(
    nrow(matched), " of ", nrow(unmatched),
    " addresses fuzzy-matched (", nrow(unmatched_out), " left for the paid queue). ",
    "Similarity: min=", round(min(matched$fuzzy_score), 3),
    ", median=", round(stats::median(matched$fuzzy_score), 3), ". ",
    "By block: ", paste(names(table(matched$fuzzy_block)), table(matched$fuzzy_block), sep = "=", collapse = ", ")
  )

  list(matched = matched, unmatched = unmatched_out)
}
