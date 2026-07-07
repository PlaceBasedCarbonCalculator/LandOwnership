# Build a single buffered (1km) boundary of England & Wales, used as a
# bounding box hint when geocoding. Requires the OS Boundary-Line GeoPackage
# zip from the saferactive project checked out alongside this repo.
library(sf)

dir.create("tmp")
unzip("../../saferactive/saferactive/data/bdline_gpkg_gb.zip",
      exdir = "tmp")

bounds <- st_read("tmp/data/bdline_gb.gpkg", layer = "district_borough_unitary")
bounds <- bounds[substr(bounds$Census_Code,1,1) %in% c("E","W"),]
bounds <- bounds[,c("Name")]
bounds <- st_union(bounds)
bounds <- st_buffer(bounds, 1000)

unlink("tmp", recursive = TRUE)

saveRDS(bounds, "data/EnglandWalesBuff.Rds")
