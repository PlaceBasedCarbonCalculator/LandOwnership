# Retry the addresses that Bing failed to geocode using the Google Maps
# geocoder (free quota is much smaller, so only the failures are sent).
# NOTE: expects data/bing_final/bing_geocoded_poor.Rds; the current
# process_geocodeing_results.R saves the failures as bing_geocoded_fail.Rds
# (plus _low/_medium/_nola/_wrongla), so combine/rename as appropriate.
library(mapsapi)
library(sf)
library(tmap)
res_poor <- readRDS("data/bing_final/bing_geocoded_poor.Rds")
res_poor <- res_poor[!duplicated(res_poor$Id),]

n <- 10000
nr <- nrow(res_poor)
list_split <- split(res_poor, rep(1:ceiling(nr/n), each=n, length.out=nr))

for(i in seq_len(length(list_split))){
  sub <- list_split[[i]]
  path <- paste0("data/for_google/bing_poor_batch_",stringr::str_pad(i,3, pad = "0"),".Rds")
  saveRDS(sub, path)
}


bounds <- readRDS("data/EnglandWalesBuff.Rds")
bounds <- st_transform(bounds, 4326)
bbox <- st_bbox(bounds)


res_google <- mp_geocode(
  addresses = paste0(sub$addressLine.x,", ",sub$adminDistrict.x),
  region = sub$countryRegion.x,
  postcode = sub$postalCode.x,
  bounds = bbox,
  key = Sys.getenv("Google_maps_key"),
  quiet = TRUE,
  timeout = 10
)

res_google2 <- mp_get_points(res_google)

tm_shape(res_google2[res_google2$status == "OK",]) +
  tm_dots(col = "location_type",
          popup.vars = names(res_google2)[1:5])
