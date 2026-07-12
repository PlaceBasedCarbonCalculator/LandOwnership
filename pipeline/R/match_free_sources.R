# Stage 6 (tasks 5/6): before ever sending an address to the paid Azure
# geocoder, try to resolve it for free by matching against the sibling
# repo's EPC-linked and Price-Paid-linked UPRN tables. Matching key is
# (normalised postcode, leading house number) - see normalise_match_key()
# in pipeline/R/utils.R.
#
# Known limitation (documented in the sibling repo's own code comments,
# see the research notes in this session): house_price_lr_uprn's address-
# matching cascade leaves a large residual with no UPRN at all, and
# uprn_postcode_check.R found ~0.07% of domestic UPRNs have a coordinate
# that falls outside their claimed postcode area. Both sources are treated
# as "probably right, not guaranteed" - anything matched here is tagged
# with its source so bad matches can be traced back later.

build_epc_lookup <- function(uprn_historical_epc_lr) {
  dom <- uprn_historical_epc_lr$domestic
  nondom <- uprn_historical_epc_lr$nondomestic

  lookup <- dplyr::bind_rows(
    data.frame(
      key = normalise_match_key(dom$addr, dom$POSTCODE),
      UPRN = dom$UPRN, LATITUDE = dom$LATITUDE, LONGITUDE = dom$LONGITUDE,
      match_source = "epc_domestic"
    ),
    data.frame(
      key = normalise_match_key(nondom$adr1, nondom$postcode),
      UPRN = nondom$UPRN, LATITUDE = nondom$LATITUDE, LONGITUDE = nondom$LONGITUDE,
      match_source = "epc_nondomestic"
    )
  )
  lookup <- lookup[!is.na(lookup$key), ]
  # A postcode+house-number key that maps to more than one UPRN is
  # ambiguous (e.g. flats sharing a building's street number) - drop it
  # rather than guess which UPRN is meant.
  ambiguous <- lookup$key[duplicated(lookup$key)]
  lookup[!lookup$key %in% ambiguous, ]
}

build_price_paid_lookup <- function(house_price_lr_uprn) {
  hp <- house_price_lr_uprn[!is.na(house_price_lr_uprn$uprn), ]
  lookup <- data.frame(
    key = normalise_match_key(hp$address1, hp$postcode),
    UPRN = hp$uprn, LATITUDE = hp$LATITUDE, LONGITUDE = hp$LONGITUDE,
    match_source = "price_paid"
  )
  lookup <- lookup[!is.na(lookup$key), ]
  ambiguous <- lookup$key[duplicated(lookup$key)]
  lookup[!lookup$key %in% ambiguous, ]
}

# `needs_geocode` is the output of carry_forward_unchanged()$needs_geocode
# (split addresses still needing a location). Tries the EPC lookup first,
# falls back to Price Paid for anything still unmatched. Returns a list of
# `matched` (tagged with UPRN/coords/match_source) and `unmatched` (same
# columns as the input, ready for the geocode queue).
match_free_sources <- function(needs_geocode, epc_lookup, price_paid_lookup) {
  orig_cols <- names(needs_geocode)
  needs_geocode$match_key <- normalise_match_key(needs_geocode$AddressLine, needs_geocode$PostalCode)
  keyed_cols <- names(needs_geocode) # orig_cols + match_key, needed again for the second join

  step1 <- dplyr::left_join(needs_geocode, epc_lookup, by = c("match_key" = "key"))
  matched1 <- step1[!is.na(step1$UPRN), ]
  unmatched1 <- step1[is.na(step1$UPRN), keyed_cols]

  step2 <- dplyr::left_join(unmatched1, price_paid_lookup, by = c("match_key" = "key"))
  matched2 <- step2[!is.na(step2$UPRN), ]
  unmatched2 <- step2[is.na(step2$UPRN), orig_cols]

  matched <- dplyr::bind_rows(matched1, matched2)
  matched$source <- paste0(matched$match_source, "_match")
  matched$match_source <- NULL
  matched$match_key <- NULL

  list(matched = matched, unmatched = unmatched2)
}
