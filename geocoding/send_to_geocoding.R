# WARNING THIS USED THE PAID API. Do not run this script unless you deliberately want to spend quota on geocoding.


source("R/azure_api.R")                      # Azure Maps client
source("pipeline/R/geocode_batch_runner.R")
source("pipeline/R/geocode_batch_runner_bulk.R")
#run_geocode_batch(n = 5, confirm = FALSE)  # Old one at a time API call, now replaced by run_geocode_batch_bulk() which is more efficient and cheaper per address.
run_geocode_batch_bulk(n = 1000, confirm = FALSE) # <-- set confirm = TRUE to actually spend quota
#Practical rhythm
# Given the ~5,000/month cap, the intended pattern is: run a batch of a few hundred, check remaining 
# work with something like
q <- readRDS("data/geocoding/queue.rds")
table(q$status)
#then repeat over days/weeks, re-running tar_make() whenever you want the downstream outputs refreshed. 
# geocode_usage_this_month("logs/azure_usage_log.csv") tells you how much quota you've used so far this month.



# Check results

res <- readRDS("data/geocoding/azure_results.rds")   # the geocodes
q   <- readRDS("data/geocoding/queue.rds")
table(q$status)                                       # progress
read.csv("logs/azure_usage_log.csv")

library(sf)
library(tmap)
tmap_mode("view")
res <- st_as_sf(res, coords = c("longitude", "latitude"), crs = 4326)
qtm(res)
