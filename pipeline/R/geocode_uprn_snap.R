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

  pts <- sf::st_as_sf(azure_results, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  pts <- sf::st_transform(pts, 27700)

  uprn_pts <- sf::st_as_sf(
    uprn_historical[, c("UPRN", "X_COORDINATE", "Y_COORDINATE")],
    coords = c("X_COORDINATE", "Y_COORDINATE"), crs = 27700
  )

  nearest_idx <- sf::st_nearest_feature(pts, uprn_pts)
  dist <- sf::st_distance(pts, uprn_pts[nearest_idx, ], by_element = TRUE)

  pts$UPRN <- uprn_pts$UPRN[nearest_idx]
  pts$uprn_snap_distance_m <- as.numeric(dist)
  pts$UPRN[pts$uprn_snap_distance_m > tolerance_m] <- NA

  out <- sf::st_drop_geometry(pts)
  out$source <- ifelse(is.na(out$UPRN), "azure_geocoded", "azure_geocoded_uprn_snap")
  out
}
