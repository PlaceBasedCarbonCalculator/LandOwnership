# Stage 10 (task 8): snap Azure-geocoded points to the nearest UPRN, on the
# theory that Azure's returned point for a residential/commercial address
# *is* the UPRN location. `tolerance_m` guards against snapping to a
# plausible-looking but wrong UPRN when the geocode is only approximate
# (e.g. postcode-centroid fallback rather than a real address match) -
# worth re-checking against real results once batches start coming back
# (see the plan's verification notes).

snap_geocoded_to_uprn <- function(azure_results, uprn_historical, tolerance_m = 50) {
  if (nrow(azure_results) == 0) {
    azure_results$UPRN <- character(0)
    azure_results$uprn_snap_distance_m <- numeric(0)
    azure_results$source <- character(0)
    return(azure_results)
  }

  # Nearest-UPRN search via RANN's kd-tree on plain coordinate matrices, NOT
  # sf::st_nearest_feature() on `sf` geometry objects built from the full
  # national uprn_historical table (~36-41M rows). This function used to
  # build exactly that - the same construction cost that made
  # fuzzy_match_geographic() hang for a full day before being rewritten this
  # way (see the 2026-07-21 fuzzy_match runtime incident); st_nearest_feature()
  # itself is a safer single-nearest lookup than the pairwise
  # st_is_within_distance() that actually broke, but constructing the `sf`
  # object over every UPRN in England & Wales first was still the same
  # dormant risk, flagged for fixing once real Azure batches start coming
  # back. sf::sf_project() transforms the WGS84 Azure results to British
  # National Grid (matching uprn_historical's already-BNG X/Y) as a plain
  # matrix, never creating `sf` geometries.
  azure_xy <- sf::sf_project(
    from = "EPSG:4326", to = "EPSG:27700",
    pts = as.matrix(azure_results[, c("longitude", "latitude")])
  )
  uprn_xy <- as.matrix(uprn_historical[, c("X_COORDINATE", "Y_COORDINATE")])

  nn <- RANN::nn2(data = uprn_xy, query = azure_xy, k = 1)

  out <- azure_results
  out$UPRN <- uprn_historical$UPRN[nn$nn.idx[, 1]]
  out$uprn_snap_distance_m <- nn$nn.dists[, 1]
  out$UPRN[out$uprn_snap_distance_m > tolerance_m] <- NA

  out$source <- ifelse(is.na(out$UPRN), "azure_geocoded", "azure_geocoded_uprn_snap")
  # rows whose location was copied from a flat group's representative (see
  # pipeline/R/geocode_flat_infer.R) carry that provenance through - they
  # all share the rep's point, so the snapped UPRN is the building's, not
  # the individual flat's
  if ("inferred_from" %in% names(out)) {
    out$source[!is.na(out$inferred_from)] <- "azure_geocoded_flat_inferred"
  }
  out
}
