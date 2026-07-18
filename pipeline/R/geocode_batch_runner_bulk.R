# Stage 9 (task 4): manual, confirm-gated Azure Maps geocoding - BULK variant.
#
# Drop-in replacement for run_geocode_batch() in geocode_batch_runner.R.
# Identical signature, identical on-disk effects (queue.rds, azure_results.rds,
# azure_usage_log.csv) and identical return value, but instead of one HTTP call
# per address it uses the Azure Maps *batch* Geocoding API, which accepts up to
# 100 queries per POST:
#   https://learn.microsoft.com/en-us/rest/api/maps/search/get-geocoding-batch
# For a batch of n addresses this is ceil(n / 100) requests instead of n, which
# is dramatically faster and easier on connection overhead. Each batch item
# still counts as one billed geocode, so the monthly_cap accounting is unchanged.
#
# THIS FILE IS NOT PART OF THE TARGETS DAG - nothing in _targets.R sources it,
# so tar_make() will never spend Azure quota. To run a batch yourself:
#
#   source("R/azure_api.R")                       # build_url(), parse_feature()
#   source("pipeline/R/geocode_batch_runner.R")   # geocode_usage_this_month()
#   source("pipeline/R/geocode_batch_runner_bulk.R")
#   run_geocode_batch_bulk(n = 500, confirm = TRUE)
#
# Azure Maps' allowed quota here is ~5,000 geocodes/month, so this is still
# designed to be called in small batches spread over time.

# Pass `uprn_historical` (e.g. targets::tar_read(uprn_historical)) to
# run infer_flat_locations() automatically after the batch (see
# pipeline/R/geocode_flat_infer.R - no extra Azure spend).
run_geocode_batch_bulk <- function(n,
                                    confirm = FALSE,
                                    monthly_cap = 5000,
                                    force = FALSE,
                                    queue_path = "data/geocoding/queue.rds",
                                    results_path = "data/geocoding/azure_results.rds",
                                    usage_log_path = "logs/azure_usage_log.csv",
                                    uprn_historical = NULL) {
  if (!isTRUE(confirm)) {
    stop(
      "run_geocode_batch_bulk() refuses to run without confirm = TRUE. ",
      "This calls the paid Azure Maps Geocode API - pass confirm = TRUE ",
      "only once you've deliberately decided to spend quota on this batch."
    )
  }
  if (!file.exists(queue_path)) {
    stop(
      "No geocode queue found at ", queue_path,
      ". Run the targets pipeline through the geocode_queue target first."
    )
  }

  used_this_month <- geocode_usage_this_month(usage_log_path)
  if (!isTRUE(force) && used_this_month + n > monthly_cap) {
    stop(
      "Refusing to run: ", used_this_month, " geocodes already used this month, ",
      "requesting ", n, " more would exceed monthly_cap = ", monthly_cap, ". ",
      "Pass a smaller n, raise monthly_cap, or force = TRUE to override."
    )
  }

  queue <- readRDS(queue_path)
  pending <- queue[queue$status == "pending", ]
  if (nrow(pending) == 0) {
    message("Queue is empty - nothing to geocode.")
    return(invisible(NULL))
  }
  # postcode-bearing rows first: higher expected success per paid call
  # (audit recommendation 11)
  has_pc <- !is.na(pending$PostalCode) & pending$PostalCode != ""
  pending <- pending[order(!has_pc), ]
  batch <- pending[seq_len(min(n, nrow(pending))), ]

  message(
    "About to send ", nrow(batch), " addresses to the Azure Maps Geocode BATCH API ",
    "in ", ceiling(nrow(batch) / 100L), " request(s). ",
    "This is a paid API call. Month-to-date usage before this call: ",
    used_this_month, "/", monthly_cap, "."
  )

  results <- vector("list", nrow(batch))
  # Chunk into requests of <= 100 items (Azure's synchronous batch limit).
  chunks <- split(seq_len(nrow(batch)), ceiling(seq_len(nrow(batch)) / 100L))
  for (idx in chunks) {
    items <- lapply(idx, function(i) azure_batch_item(batch[i, ]))
    parsed <- try(azure_geocode_batch(items), silent = TRUE)

    if (inherits(parsed, "try-error") || length(parsed) != length(idx)) {
      # Whole chunk failed (request error, timeout, or a malformed response) -
      # mark every row in it failed, exactly as the per-row loop would on error.
      batch$status[idx] <- "failed"
      next
    }

    for (j in seq_along(idx)) {
      i <- idx[j]
      res <- parsed[[j]]
      if (is.null(res) || nrow(res) == 0 || is.na(res$latitude[1])) {
        batch$status[i] <- "failed"
        next
      }
      res <- res[1, ] # top candidate only
      res$queue_key <- batch$queue_key[i]
      results[[i]] <- res
      batch$status[i] <- "done"
    }
  }
  batch$attempts <- batch$attempts + 1L
  batch$last_attempt_date <- Sys.Date()

  new_results <- dplyr::bind_rows(results)
  if (file.exists(results_path)) {
    new_results <- dplyr::bind_rows(readRDS(results_path), new_results)
  }
  saveRDS(new_results, results_path)

  match_idx <- match(batch$queue_key, queue$queue_key)
  queue$status[match_idx] <- batch$status
  queue$attempts[match_idx] <- batch$attempts
  queue$last_attempt_date[match_idx] <- batch$last_attempt_date
  saveRDS(queue, queue_path)

  log_line <- data.frame(
    timestamp = Sys.time(), n_requested = nrow(batch),
    n_succeeded = sum(batch$status == "done")
  )
  write.table(log_line, usage_log_path,
    sep = ",", row.names = FALSE,
    col.names = !file.exists(usage_log_path), append = file.exists(usage_log_path)
  )

  message("Done. ", sum(batch$status == "done"), "/", nrow(batch), " succeeded.")

  if (!is.null(uprn_historical)) {
    infer_flat_locations(uprn_historical,
      queue_path = queue_path, results_path = results_path
    )
  }
  invisible(new_results)
}

# Build one GeocodingBatchRequestItem from a single queue row. Mirrors the
# arguments the per-row runner passed to azure_geocode_single(): structured
# address parts, GB country, top candidate only. NA/empty fields are dropped so
# they don't confuse the geocoder.
azure_batch_item <- function(row) {
  item <- list(
    addressLine = row$AddressLine,
    adminDistrict = row$District,
    postalCode = row$PostalCode,
    countryRegion = "GB",
    top = 1L
  )
  drop <- vapply(
    item,
    function(v) is.null(v) || length(v) == 0 || (is.atomic(v) && is.na(v)) || identical(as.character(v), ""),
    logical(1)
  )
  item[!drop]
}

# POST a list of GeocodingBatchRequestItem objects to the Azure Maps batch
# Geocoding API and return a list, one element per request item in the SAME
# order, where each element is either:
#   - a data frame of candidate matches (same columns as parse_feature()), or
#   - NULL, if that item errored or returned no features.
# Returns a length-0 list on a request-level failure so the caller can fail the
# whole chunk.
azure_geocode_batch <- function(items,
                                api_version = "2026-01-01",
                                key = Sys.getenv("AZURE_MAPS_PRIMARY_KEY")) {
  url <- build_url(
    "https://atlas.microsoft.com/geocode:batch",
    list(`api-version` = api_version, `subscription-key` = key)
  )
  body <- jsonlite::toJSON(list(batchItems = items), auto_unbox = TRUE, na = "null")

  h <- curl::new_handle()
  curl::handle_setheaders(h, "Content-Type" = "application/json")
  curl::handle_setopt(h, post = TRUE, copypostfields = body)
  resp <- try(curl::curl_fetch_memory(url, handle = h), silent = TRUE)
  if (inherits(resp, "try-error")) {
    message("Geocode batch failed: request error")
    return(list())
  }
  text <- rawToChar(resp$content)

  asjson <- try(RcppSimdJson::fparse(text), silent = TRUE)
  if ("try-error" %in% class(asjson)) {
    message("Geocode batch failed: json parse failed")
    return(list())
  }
  # A request-level error (e.g. 400 bad request, 408 timeout) comes back as a
  # top-level `error` object rather than a batchItems array.
  if (!is.null(asjson$error)) {
    message("Geocode batch failed: ", asjson$error$message)
    return(list())
  }

  bi <- asjson$batchItems
  n_items <- if (is.data.frame(bi)) nrow(bi) else length(bi)
  if (is.null(bi) || n_items == 0) {
    message("Geocode batch failed: no batchItems returned")
    return(list())
  }

  lapply(seq_len(n_items), function(i) {
    # fparse may simplify the batchItems array to a data frame (with list
    # columns) or leave it as a list, depending on the response - handle both.
    # When it simplifies to a data frame, the cell for a column an item does not
    # carry (e.g. `error` on a success, `features` on a failure) is filled with
    # an atomic NA rather than being absent, so treat atomic NA as "not present".
    if (is.data.frame(bi)) {
      err <- if ("error" %in% names(bi)) bi$error[[i]] else NULL
      feats <- if ("features" %in% names(bi)) bi$features[[i]] else NULL
    } else {
      err <- bi[[i]]$error
      feats <- bi[[i]]$features
    }
    if (!is_absent(err)) {
      return(NULL) # this item failed inside the batch
    }
    features_to_rows(feats)
  })
}

# TRUE when an fparse value is not really there: NULL, empty, or an atomic NA
# fill produced when the surrounding array was simplified into a data frame.
is_absent <- function(x) {
  is.null(x) || length(x) == 0 || (is.atomic(x) && all(is.na(x)))
}

# Turn the `features` value of one batch item into a data frame of candidate
# matches by reusing parse_feature() from R/azure_api.R. `features` arrives
# either as a data frame with list columns (fparse simplified it) or as a list
# of feature lists - the same duality azure_geocode_single() already handles.
features_to_rows <- function(feats) {
  if (is_absent(feats)) {
    return(NULL)
  }
  if (is.data.frame(feats)) {
    feats <- lapply(seq_len(nrow(feats)), function(i) {
      list(
        type = feats$type[i],
        geometry = feats$geometry[[i]],
        bbox = if ("bbox" %in% names(feats)) feats$bbox[[i]] else NULL,
        properties = feats$properties[[i]]
      )
    })
  }
  dplyr::bind_rows(lapply(feats, parse_feature))
}
