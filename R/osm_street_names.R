# Extract unique road and place names for Great Britain from an
# OpenStreetMap extract, used to help identify road/place names within
# Land Registry address strings.
library(osmextract)
library(sf)
path = "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain-NTEM/great-britain-latest.osm.pbf"

highway = oe_read(path,
                  layer = "lines")
highway <- st_drop_geometry(highway)
highway <- highway[,c("name","highway")]
highway <- highway[!is.na(highway$highway),]
highway <- highway[!is.na(highway$name),]

hwy <- c("living_street","motorway","motorway_link",
         "pedestrian","primary","primary_link" ,
         "residential","road","secondary","secondary_link","service",       
         "services","tertiary","tertiary_link","track","trunk",
         "trunk_link","unclassified")

highway <- highway[highway$highway %in% hwy,]

highway_nms <- highway[!duplicated(highway$name),]
highway_nms <- highway_nms[order(highway_nms$name),]
highway_nms <- highway_nms[180:nrow(highway_nms),]
highway_nms <- highway_nms[highway_nms$name != "парк",]

write.csv(highway_nms, "data/osm_unique_road_names.csv", row.names = FALSE)


places = oe_read(path,
                  layer = "points",
                 extra_tags = "place")

places <- st_drop_geometry(places)
places <- places[,c("name","place")]
places <- places[!is.na(places$place),]
places <- places[!is.na(places$name),]

plc <- c("hamlet","village","suburb",
         "neighbourhood","town","city" ,
         "square","quarter","city_block","county",
         "country")

places <- places[places$place %in% plc,]

places_nms <- places[!duplicated(places$name),]
places_nms <- places_nms[order(places_nms$name),]

write.csv(places_nms, "data/osm_unique_place_names.csv", row.names = FALSE)
