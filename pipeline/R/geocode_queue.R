# Stage 8 (tasks 2/4): the persistent pending-geocode queue. Deliberately
# lives outside the pure targets DAG - see _targets.R, which tracks
# data/geocoding/queue.rds with a format = "file" target so anything
# downstream reruns whenever the file's content changes (e.g. after a
# manual geocode batch updates row statuses).
#
# Existing "done"/"failed" status is always preserved across reruns: this
# function only appends genuinely new queue_keys, it never resets progress
# that run_geocode_batch() has already made.

build_geocode_queue <- function(unmatched, queue_path = "data/geocoding/queue.rds") {
  new_rows <- unmatched
  new_rows$queue_key <- paste(new_rows$`Title Number`, new_rows$AddressLine, sep = "||")
  new_rows$status <- "pending"
  new_rows$attempts <- 0L
  new_rows$last_attempt_date <- as.Date(NA)

  if (file.exists(queue_path)) {
    existing <- readRDS(queue_path)
    to_add <- new_rows[!new_rows$queue_key %in% existing$queue_key, ]
    combined <- dplyr::bind_rows(existing, to_add)
    message(nrow(to_add), " new addresses added to the geocode queue (", nrow(existing), " already tracked).")
  } else {
    combined <- new_rows
    message(nrow(combined), " addresses added to a new geocode queue.")
  }

  dir.create(dirname(queue_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(combined, queue_path)
  combined
}
