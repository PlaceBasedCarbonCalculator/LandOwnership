# Stage 9b: infer locations for deferred flats from their building's
# geocoded representative - the free half of the flat-grouping scheme set
# up by assign_flat_groups() in geocode_queue.R.
#
# For each flat group whose representative has come back "done":
#
#   1. UPRN stack check: convert the representative's geocoded point to
#      BNG and look at every UPRN within `tolerance_m` metres. Flats in
#      one building share a single seed coordinate in the UPRN data, so a
#      real 70-flat block shows up as >= 70 UPRNs with IDENTICAL
#      coordinates. The check requires the largest identical-coordinate
#      stack near the point to be at least the group size.
#   2. Check passes: every deferred member gets a copy of the
#      representative's result (queue_key swapped, `inferred_from` +
#      `flat_uprn_stack` recording the provenance) appended to
#      azure_results.rds, and its queue status becomes "inferred".
#   3. Check fails: the grouping was wrong (e.g. houses in a cul-de-sac,
#      or the geocode landed somewhere else) - the members are released
#      back to "pending" so later batches geocode them individually, and
#      flat_check = "failed" stops assign_flat_groups() ever re-deferring
#      that group.
#
# No Azure calls are made here - this only spends results that
# run_geocode_batch() already paid for. `uprn_historical` is the sibling
# repo's UPRN table (columns UPRN, X_COORDINATE, Y_COORDINATE), e.g.:
#
#   uprn_historical <- targets::tar_read(uprn_historical)
#   source("pipeline/R/geocode_flat_infer.R")
#   infer_flat_locations(uprn_historical)

infer_flat_locations <- function(uprn_historical,
                                 queue_path = "data/geocoding/queue.rds",
                                 results_path = "data/geocoding/azure_results.rds",
                                 tolerance_m = 50) {
  if (!file.exists(queue_path)) {
    stop("No geocode queue found at ", queue_path, ".")
  }
  queue <- readRDS(queue_path)
  if (!"flat_group" %in% names(queue) || !any(queue$status == "deferred_flat")) {
    message("No deferred flat-group members to infer.")
    return(invisible(NULL))
  }
  if (!file.exists(results_path)) {
    message("No Azure results at ", results_path, " yet - geocode some representatives first.")
    return(invisible(NULL))
  }
  results <- readRDS(results_path)

  deferred <- queue$status == "deferred_flat" & !is.na(queue$flat_group)
  groups <- unique(queue$flat_group[deferred])

  # one row per group: rep key + its geocoded location (if any)
  rep_keys <- vapply(groups, function(g) {
    queue$flat_rep_key[queue$flat_group %in% g][1]
  }, character(1))
  rep_status <- queue$status[match(rep_keys, queue$queue_key)]

  # if a rep failed, promote the group's first deferred member so the next
  # manual batch retries the building with a different flat
  rep_failed <- !is.na(rep_status) & rep_status == "failed"
  for (g in groups[rep_failed]) {
    members <- which(queue$flat_group %in% g & queue$status == "deferred_flat")
    if (length(members) == 0) next
    new_rep <- members[1]
    queue$status[new_rep] <- "pending"
    queue$flat_rep_key[queue$flat_group %in% g] <- queue$queue_key[new_rep]
    message(
      "Flat group rep failed - promoted '", queue$AddressLine[new_rep],
      "' to retry the building."
    )
  }

  # groups whose rep has a usable result
  ri <- match(rep_keys, results$queue_key)
  has_result <- !is.na(ri) & !is.na(results$latitude[ri]) &
    !is.na(rep_status) & rep_status == "done"
  todo <- data.frame(
    flat_group = groups[has_result],
    rep_key = rep_keys[has_result],
    latitude = results$latitude[ri[has_result]],
    longitude = results$longitude[ri[has_result]],
    stringsAsFactors = FALSE
  )
  if (nrow(todo) == 0) {
    message(
      length(groups), " flat group(s) are waiting but none of their ",
      "representatives has a geocoded result yet."
    )
    saveRDS(queue, queue_path) # persist any rep promotions
    return(invisible(NULL))
  }

  # rep points to BNG once, then per group count the largest stack of
  # UPRNs sharing IDENTICAL coordinates within the tolerance box
  pts <- sf::st_as_sf(todo, coords = c("longitude", "latitude"), crs = 4326)
  xy <- sf::st_coordinates(sf::st_transform(pts, 27700))
  ux <- as.numeric(uprn_historical$X_COORDINATE)
  uy <- as.numeric(uprn_historical$Y_COORDINATE)

  n_inferred <- 0L
  n_released <- 0L
  new_rows <- list()
  for (i in seq_len(nrow(todo))) {
    g <- todo$flat_group[i]
    members <- which(queue$flat_group %in% g)
    def_members <- members[queue$status[members] == "deferred_flat"]
    if (length(def_members) == 0) next
    group_n <- queue$flat_group_n[members[1]]

    near <- which(abs(ux - xy[i, 1]) <= tolerance_m & abs(uy - xy[i, 2]) <= tolerance_m)
    stack <- if (length(near) == 0) {
      0L
    } else {
      max(table(paste(ux[near], uy[near], sep = "_")))
    }

    if (stack >= group_n) {
      rep_res <- results[results$queue_key == todo$rep_key[i], ]
      rep_res <- rep_res[!is.na(rep_res$latitude), ]
      rep_res <- rep_res[nrow(rep_res), ] # latest result if ever re-geocoded
      inf <- rep_res[rep(1, length(def_members)), ]
      inf$queue_key <- queue$queue_key[def_members]
      inf$inferred_from <- todo$rep_key[i]
      inf$flat_uprn_stack <- as.integer(stack)
      new_rows[[length(new_rows) + 1]] <- inf

      queue$status[def_members] <- "inferred"
      queue$flat_check[members] <- "passed"
      n_inferred <- n_inferred + length(def_members)
    } else {
      # not enough co-located UPRNs to support "these flats are one
      # building at this point" - geocode the members individually
      queue$status[def_members] <- "pending"
      queue$flat_check[members] <- "failed"
      n_released <- n_released + length(def_members)
      message(
        "Flat group '", g, "': UPRN check failed (largest identical-",
        "coordinate stack within ", tolerance_m, "m is ", stack,
        ", needed ", group_n, ") - ", length(def_members),
        " members released back to pending."
      )
    }
  }

  if (length(new_rows) > 0) {
    results <- dplyr::bind_rows(results, new_rows)
    saveRDS(results, results_path)
  }
  saveRDS(queue, queue_path)

  message(
    "Flat inference done: ", n_inferred, " locations inferred free of charge, ",
    n_released, " released for individual geocoding."
  )
  invisible(results)
}
