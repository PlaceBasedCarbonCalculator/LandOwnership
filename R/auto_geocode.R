# Daily geocoding run: pick the next batch CSV from OneDrive that has not
# been geocoded yet, run it through the Bing Locations API and save the
# results (and failures) as Rds. See also send_for_geocode.R, which does
# the same for batches stored inside the repo's data folder.
library(sf)
library(tmap)
source("R/bing_api.R")
#path <- "C:/Users/malco/OneDrive - University of Leeds/Data/Land Ownership"
path <- "D:/OneDrive - University of Leeds/Data/Land Ownership"
#for(i  in 1:100){
  # Do the Geocoding
  
  files_todo <- list.files(paste0(path,"/for_geocoding"), pattern = ".csv")
  files_todo <- gsub(".csv","",files_todo, fixed = TRUE)
  files_done <- list.files(paste0(path,"/geocoded"))
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
  dat = read.csv(paste0(path,"/for_geocoding/",files_todo,".csv"))
  if(nrow(dat) < 50000){
    message("Short file today only ",nrow(dat)," addresses")
    
  }
  
  # Sleep until 4am
  t_now <- lubridate::now("GMT")
  t_start <- lubridate::today("GMT") + 1 + lubridate::hours(3)
  t_diff <- difftime(t_start, t_now, units="secs")
  t_diff <- round(as.numeric(t_diff))
  
  #message(Sys.time()," sleeping for ",t_diff," seconds")
  #Sys.sleep(t_diff)
  
  # Do the Geocoding#
  message(Sys.time(), " starting")
  res = bing_geocode_batch(dat)
  message(Sys.time(), " finished")
  res_missing = res[is.na(res$latitude),]
  res = res[!is.na(res$latitude),]
  res = sf::st_as_sf(res, coords = c("longitude","latitude"), crs = 4326)
  
  saveRDS(res,paste0(path,"/geocoded/",files_todo,".Rds"))
  saveRDS(res_missing,paste0(path,"/geocoded/",files_todo,"_failed.Rds"))
  
  message("Got ",nrow(res)," results")
  
  
  rm(res, res_missing, dat, files_done, files_todo, t_now, t_start, t_diff)
#}


# files_todo = "UK_freehold_nopc_land_simple_batch_006"
# dat = readRDS(paste0(path,"/geocoded/",files_todo,".Rds"))
# qtm(dat, dots.col = "confidence")

  