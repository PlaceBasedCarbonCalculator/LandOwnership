# Send the next batch CSV from data/for_geocoding to the Bing geocoder and
# save results to data/geocoded (see auto_geocode.R for the OneDrive
# variant used for the daily runs).
library(sf)
library(tmap)
source("R/bing_api.R")

files_todo <- list.files("data/for_geocoding", pattern = ".csv")
files_todo <- gsub(".csv","",files_todo, fixed = TRUE)
files_done <- list.files("data/geocoded")
files_done <- gsub(".Rds","",files_done, fixed = TRUE)
files_done <- files_done[!grepl("_failed",files_done)]

message("Done ",length(files_done)," of ",length(files_todo)," batches")

# Skip any done already
files_todo <- files_todo[!files_todo %in% files_done]
if(length(files_todo) == 0){
  stop("No files left to do")
}
files_todo <- files_todo[1]

message("Today's file is: ", files_todo)
dat = read.csv(paste0("data/for_geocoding/",files_todo,".csv"))
if(nrow(dat) < 50000){
  message("Short file today only ",nrow(dat)," addresses")
  
}
message(Sys.time(), " starting")
res = bing_geocode_batch(dat)
message(Sys.time(), " finished")
res_missing = res[is.na(res$latitude),]
res = res[!is.na(res$latitude),]
res = sf::st_as_sf(res, coords = c("longitude","latitude"), crs = 4326)

saveRDS(res,paste0("data/geocoded/",files_todo,".Rds"))
saveRDS(res_missing,paste0("data/geocoded/",files_todo,"_failed.Rds"))

tmap_mode("view")
tm_shape(res) +
  tm_dots(col = "entityType", popup.vars = names(res)[1:15])
table(res$confidence)
table(res$entityType)


# dups
# 
# dups <- res$addressLine.x[duplicated(res$addressLine.x)]
# dups <- res[res$addressLine.x %in% dups,]
