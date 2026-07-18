library(targets)
library(dplyr)
library(sf)
library(tmap)
source("../../PlaceBasedCarbonCalculator/build/R/pmtiles.R")

tar_load(c("epc_lookup", "price_paid_lookup",
         "building_lookup", "street_lookup", "infill_lookup",
         "postcode_singleton_lookup", "street_centroid_lookup"))

tar_load(known_uprn_addresses) # UPRNS with known addresses 24 million
tar_load(uprn_historical) # All UPRNS 41 million (built in this repo since Jul 2026)
tar_load(uprn_places) # UPRNS with Postcode and district 37  million
tar_load(uprn_infill) # UPRNs from OSM and guess missing numbers 15 million


uprn_all = left_join(uprn_historical, uprn_places, by = "UPRN")
uprn_all = uprn_all[uprn_all$date_last == lubridate::ymd("2025-11-01"),] # Only current UPRNS
uprn_all = left_join(uprn_all, known_uprn_addresses[,c( "UPRN", "addr", "postcode", "address_source", "house_number","street")], by = "UPRN")

uprn_all_join = uprn_all[uprn_all$UPRN %in% uprn_infill$UPRN,]
uprn_all_join = uprn_all_join[is.na(uprn_all_join$addr),]
uprn_all_join = left_join(uprn_all_join[,c("UPRN", "date_first", "date_last", "X_COORDINATE", "Y_COORDINATE", "LATITUDE", "LONGITUDE", "postcode.x", "district")], 
  uprn_infill, by = "UPRN")

names(uprn_all_join)[names(uprn_all_join) == "postcode"] = "postcode.y"

uprn_all = bind_rows(uprn_all_join, uprn_all[!uprn_all$UPRN %in% uprn_all_join$UPRN,])

uprn_all = st_as_sf(uprn_all, coords = c("X_COORDINATE", "Y_COORDINATE"), crs = 27700)

point = c( -1.75442, 53.64036)# huddersfield
point = st_point(point, dim = "XY")
point = st_sfc(point, crs = 4326)
point = st_transform(point, 27700)
point = st_buffer(point, dist = 2000, nQuadSegs = 1)

uprn_sample = uprn_all[point,]

uprn_sample$address_source[uprn_sample$address_source == "usrn_street" &
  !is.na(uprn_sample$house_number)] = "number_guessed"

table(uprn_sample$address_source, useNA = "always")

tmap_mode("view")
qtm(uprn_sample, 
  fill = "address_source", 
  popup.vars = c("UPRN", "addr", "postcode.x", "address_source", "house_number","street"))

table(uprn_all$address_source, useNA = "always")

foo = uprn_sample[uprn_sample$address_source == "usrn_street",]
