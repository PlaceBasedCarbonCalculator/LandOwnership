# One-off extract of the good/medium geocoded points within the London
# boroughs, for sharing.
library(sf)
library(dplyr)
library(tmap)
tmap_mode("view")

dat = readRDS("data/bing_final/bing_geocoded_good.Rds")
datm = readRDS("data/bing_final/bing_geocoded_medium.Rds")

dat = rbind(dat, datm)


bounds = st_read("data/la_bounds.geojson")
la_london = c("Barking and Dagenham","Barnet","Bexley","Brent","Bromley","Camden",
              "Croydon","Ealing","Enfield","Greenwich","Hackney","Hammersmith and Fulham",
              "Haringey","Harrow","Havering","Hillingdon","Hounslow","Islington","Kensington and Chelsea",
              "Kingston upon Thames","Lambeth","Lewisham","Merton","Newham","Redbridge",
              "Richmond upon Thames","Southwark","Sutton","Tower Hamlets","Waltham Forest",
              "Wandsworth","City of Westminster"
)
la_london = toupper(la_london)
bounds = bounds[bounds$name %in% la_london,]

dat_london = dat[bounds,]

saveRDS(dat_london,"data/london_extract_for_susannah.Rds")
