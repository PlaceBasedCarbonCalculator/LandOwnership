# Stage 9 (task 4): manual, confirm-gated Azure Maps geocoding.
#
# THIS FILE IS NOT PART OF THE TARGETS DAG. Nothing in _targets.R sources
# it or calls run_geocode_batch() - tar_make() will never spend Azure
# quota. To run a batch yourself:
#
#   source("R/azure_api.R")
#   source("pipeline/R/geocode_batch_runner.R")
#   run_geocode_batch(n = 500, confirm = TRUE)
#
# Azure Maps' allowed quota here is ~5,000 geocodes/month, far below the
# retired Bing API's 50,000/day, so this is designed to be called in small
# batches spread over time rather than run to completion in one sitting.

run_geocode_batch <- function(n,
                               confirm = FALSE,
                               monthly_cap = 5000,
                               force = FALSE,
                               queue_path = "data/geocoding/queue.rds",
                               results_path = "data/geocoding/azure_results.rds",
                               usage_log_path = "logs/azure_usage_log.csv") {
  if (!isTRUE(confirm)) {
    stop(
      "run_geocode_batch() refuses to run without confirm = TRUE. ",
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
    "About to send ", nrow(batch), " addresses to the Azure Maps Geocode API. ",
    "This is a paid API call. Month-to-date usage before this call: ",
    used_this_month, "/", monthly_cap, "."
  )

  results <- vector("list", nrow(batch))
  for (i in seq_len(nrow(batch))) {
    row <- batch[i, ]
    res <- try(
      azure_geocode_single(
        addressLine = row$AddressLine,
        adminDistrict = row$District,
        postalCode = row$PostalCode
      ),
      silent = TRUE
    )
    if (inherits(res, "try-error") || nrow(res) == 0 || is.na(res$latitude[1])) {
      batch$status[i] <- "failed"
      next
    }
    res <- res[1, ] # top candidate only
    res$queue_key <- row$queue_key
    results[[i]] <- res
    batch$status[i] <- "done"
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
  invisible(new_results)
}

geocode_usage_this_month <- function(usage_log_path) {
  if (!file.exists(usage_log_path)) {
    return(0)
  }
  log <- utils::read.csv(usage_log_path, stringsAsFactors = FALSE)
  log$timestamp <- as.POSIXct(log$timestamp)
  this_month <- format(Sys.Date(), "%Y-%m")
  sum(log$n_requested[format(log$timestamp, "%Y-%m") == this_month])
}

# Used by _targets.R (a `format = "file"` target needs the file to already
# exist) so the pipeline runs end-to-end before any geocoding has happened -
# creates an empty, correctly-shaped results file the first time only.
# NOT itself a call to Azure - just establishes the on-disk shape.
ensure_azure_results_file <- function(path = "data/geocoding/azure_results.rds") {
  if (!file.exists(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    empty <- data.frame(
      queue_key = character(0), addressLine = character(0), adminDistrict = character(0),
      formattedAddress = character(0), latitude = double(0), longitude = double(0),
      confidence = character(0), entityType = character(0), matchCodes = character(0),
      stringsAsFactors = FALSE
    )
    saveRDS(empty, path)
  }
  path
}
