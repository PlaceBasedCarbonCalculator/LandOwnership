library(targets)
library(dplyr)
library(sf)
library(tmap)
library(nngeo)
source("../../PlaceBasedCarbonCalculator/build/R/pmtiles.R")

tar_load(uprn_all_addresses) # every known UPRN, ~41.7 million
tar_load(uprn_all_addresses_coverage)
tar_load(queue_current)

uprn_all_addresses = st_as_sf(uprn_all_addresses, coords = c("X_COORDINATE", "Y_COORDINATE"), crs = 27700)

# best_address_source only covers "real" addresses (EPC/Price Paid/DEC/2022
# geocode); UPRNs with none of those can still carry an OSM-building or
# USRN-street inference in infill_address_source. address_status combines
# both into one field so "has an address" means either.
uprn_all_addresses = uprn_all_addresses |>
  mutate(
    address_status = case_when(
      !is.na(best_address_source) ~ best_address_source,
      !is.na(infill_address_source) ~ infill_address_source,
      TRUE ~ "no_address"
    ),
    has_address = address_status != "no_address"
  )

table(uprn_all_addresses$address_status, useNA = "always")

point = c( -1.75442, 53.64036)# huddersfield
point = st_point(point, dim = "XY")
point = st_sfc(point, crs = 4326)
point = st_transform(point, 27700)

nn = st_nn(point, uprn_all_addresses, maxdist = 3000, k = nrow(uprn_all_addresses))

uprn_sample = uprn_all_addresses[nn[[1]],]
uprn_sample$UPRN = as.character(uprn_sample$UPRN)

table(uprn_sample$address_status, useNA = "always")

tmap_mode("view")

# Every UPRN with an address (real or inferred), coloured by where it came from
qtm(uprn_sample[uprn_sample$has_address, ],
  fill = "address_status",
  popup.vars = c(
    "best_address", "best_postcode", "best_house_number", "best_street",
    "infill_house_number", "infill_street", "infill_building_name", "infill_postcode",
    "address_status"
  )
)
# UPRNs still with no address at all, in red for contrast
qtm(uprn_sample[!uprn_sample$has_address, ], fill = "red")

queue_sample = queue_current[queue_current$District == "KIRKLEES", ]

tm_shape(uprn_sample) +
  tm_dots(
  fill = "address_status",
  popup.vars = c(
    "UPRN","best_address", "best_postcode", "best_house_number", "best_street",
    "infill_house_number", "infill_street", "infill_building_name", "infill_postcode",
    "address_status"
  )
)