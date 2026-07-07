# Combine all the geocoded batches and grade the results by quality:
#   good    - entityType "Address", confidence "High", in the expected LA
#   medium  - in the expected LA but lower confidence / not an address
#   low     - confidence "Low"
#   wrongla - point falls in a different Local Authority than expected
#   nola    - point falls outside England & Wales
# Saves the graded sets to data/bing_final/.
library(dplyr)
library(sf)
library(tmap)
tmap_mode("view")

path <- "D:/OneDrive - University of Leeds/Data/Land Ownership"

#fls <- "UK_freehold_nopc_simple_batch_010.Rds"
fls <- list.files(paste0(path,"/geocoded/"))
fls <- fls[!grepl("_old",fls)]

fls_fail <- fls[grepl("_failed",fls)]
fls <- fls[!grepl("_failed",fls)]

fls_fail <- fls_fail[!grepl("Overseas_split_",fls_fail)]
fls <- fls[!grepl("Overseas_split_",fls)]

# Read in worked
res <- list()
for(i in seq(1, length(fls))){
  x <- readRDS(paste0(path,"/geocoded/",fls[i]))
  x$postalCode.x <- as.character(x$postalCode.x)
  res[[i]]  <- x 
}
res <- bind_rows(res)

res_fail <- list()
for(i in seq(1, length(fls_fail))){
  x <- readRDS(paste0(path,"/geocoded/",fls_fail[i]))
  x$postalCode.x <- as.character(x$postalCode.x)
  res_fail[[i]]  <- x 
}
res_fail <- bind_rows(res_fail)

# Handle Oversees mistake
fls_over <- list.files(paste0(path,"/geocoded/"))
fls_over <- fls_over[grepl("Overseas_split_",fls_over)]

fls_over_fail <- fls_over[grepl("_failed",fls_over)]
fls_over <- fls_over[!grepl("_failed",fls_over)]

res_over <- list()
for(i in seq(1, length(fls_over))){
  x <- readRDS(paste0(path,"/geocoded/",fls_over[i]))
  x$postalCode.x <- as.character(x$postalCode.x)
  res_over[[i]]  <- x 
}
res_over <- bind_rows(res_over)

res_over_fail <- list()
for(i in seq(1, length(fls_over_fail))){
  x <- readRDS(paste0(path,"/geocoded/",fls_over_fail[i]))
  x$postalCode.x <- as.character(x$postalCode.x)
  res_over_fail[[i]]  <- x 
}
res_over_fail <- bind_rows(res_over_fail)


source("R/find_onedrive.R")
onedrive <- find_onedrive()

dir.create("tmp")
unzip(file.path(onedrive,"Land Registry/Overseas Ownership/OCOD_FULL_2022_07.zip"),
      exdir = "tmp")
lr <- readr::read_csv("tmp/OCOD_FULL_2022_07.csv", lazy = FALSE)
unlink("tmp", recursive = TRUE)
lr$Id <- seq_len(nrow(lr))

lr <- lr[,c("Id","District")]

res_over <- left_join(res_over, lr, by = "Id")
res_over$adminDistrict.x <- res_over$District
res_over$District <- NULL

res_over_fail <- left_join(res_over_fail, lr, by = "Id")
res_over_fail$adminDistrict.x <- res_over_fail$District
res_over_fail$District <- NULL

res <- rbind(res, res_over)
res_fail <- rbind(res_fail, res_over_fail)


message(nrow(res)," geocoded")
message(nrow(res_fail)," failed")

bounds <- read_sf("data/la_bounds.geojson")
names(bounds) <- c("la_point","geometry")


# Split out by quality
# Good ones are entityType == Address, confidence == High, and within correct LA

res$check_id <- 1:nrow(res)

res <- st_join(res, bounds)
summary(is.na(res$la_point))

res_nola <- res[is.na(res$la_point), ]
res <- res[!is.na(res$la_point), ]
res_wrongla <- res[res$la_point != res$adminDistrict.x, ]
res <- res[res$la_point == res$adminDistrict.x, ]


res_medium <- res[!(res$entityType == "Address" &  res$confidence == "High"),]
res_good <- res[(res$entityType == "Address" &  res$confidence == "High"),]
res_low <- res_medium[res_medium$confidence == "Low",]
res_medium <- res_medium[res_medium$confidence != "Low",]

message(nrow(res_good)," good points out of ",sum(c(nrow(res_low),nrow(res_medium),nrow(res_good),nrow(res_wrongla),nrow(res_nola))))

# res_medium_good <- res_medium
# res_medium_good$matchCodes <- sapply(res_medium_good$matchCodes, function(x){
#   paste(x, collapse = " ")
# })
# table(res_medium_good$matchCodes)
# res_medium_good <- res_medium_good[res_medium_good$matchCodes == "Good",]
# res_medium_good <- res_medium_good[res_medium_good$entityType == "Address",]

# These are not 100% good but a lot are and so may be worth capturing
# tm_shape(res_medium_good[1:1000,])+
#   tm_dots(col = "confidence", popup.vars = names(res_medium)[1:19])


# Medium ones are mixed bag of good and bad
# summary(res_good$postalCode.x == res_good$postalCode.y)
# summary(res_medium$postalCode.x == res_medium$postalCode.y)
# foo = res_good[res_good$postalCode.x != res_good$postalCode.y,]
# bar = res_medium[res_medium$postalCode.x != res_medium$postalCode.y,]
# 
# res_poor <- res[!res$check_id %in% c(res_good$check_id, res_medium$check_id),]

# Check for duplicates in the good results
summary(duplicated(st_drop_geometry(res_good[,c("Title.Number","addressLine.x")])))

# foo <- res_good[duplicated(st_drop_geometry(res_good[,c("Title.Number","addressLine.x")])),]
# foo <- res_good[res_good$Title.Number %in% foo$Title.Number,]


saveRDS(res_good,"data/bing_final/bing_geocoded_good.Rds")
saveRDS(res_medium,"data/bing_final/bing_geocoded_medium.Rds")
saveRDS(res_low,"data/bing_final/bing_geocoded_low.Rds")
saveRDS(res_fail,"data/bing_final/bing_geocoded_fail.Rds")
saveRDS(res_nola,"data/bing_final/bing_geocoded_nola.Rds")
saveRDS(res_wrongla,"data/bing_final/bing_geocoded_wrongla.Rds")

message(nrow(res_good) + nrow(res_medium),"/",sum(c(nrow(res_low),nrow(res_medium),nrow(res_good),nrow(res_wrongla),nrow(res_nola)))," good points")
