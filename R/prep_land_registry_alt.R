# Import the INSPIRE polygon zips (one per Local Authority), clean them by
# merging polygons that have been artificially split along the 500m British
# National Grid lines of old paper maps, then export cleaned geojson for
# building vector tiles. This is the improved version of prep_land_registry.R.
library(sf)
library(tmap)
library(data.table)
library(plyr)
library(geojsonsf)

source("R/find_onedrive.R")
onedrive = find_onedrive()

path = file.path(onedrive,"Land Registry/INSPIRE Polygons")
zips = list.files(path)

polys <- list()
# fails at 68 city of london
# Splitting a Line by a GeometryCollection is unsupported

for(i in 1:length(zips)){
  dir.create("tmp")
  
  unzip(file.path(onedrive,"Land Registry/INSPIRE Polygons",zips[i]),
        exdir = "tmp")
  poly <- read_sf("tmp/Land_Registry_Cadastral_Parcels.gml")
  poly <- poly[,c("INSPIREID","VALIDFROM","BEGINLIFESPANVERSION")]
  message(Sys.time()," ",zips[i]," ",nrow(poly)," polygons")
  unlink("tmp", recursive = TRUE)
  
  # Make a grid
  grd <- st_bbox(poly)
  grd[1] <- round_any(grd[1], 500, f = floor)
  grd[2] <- round_any(grd[2], 500, f = floor)
  grd[3] <- round_any(grd[3], 500, f = ceiling)
  grd[4] <- round_any(grd[4], 500, f = ceiling)
  grd <- st_as_sfc(grd)
  grd <- st_make_grid(grd, c(500,500))
  
  # Select polys that may be split by the grid
  sub <- poly[grd, , op = st_touches] 
  
  # Make the grid any polys into lines
  grd_line <- st_cast(grd,"LINESTRING")
  suppressWarnings(sub_line <- st_cast(sub,"LINESTRING"))
  
  # COnvert Grid to Lines
  grd_line <- st_as_sf(grd_line)
  grd_line$id <- 1
  grd_line <- stplanr::overline2(grd_line, "x", simplify = FALSE, quiet = TRUE)
  sub2 <- sub_line[grd_line,,op=st_overlaps]
  
  #Problem with square boxes, so capture them 
  sub3 <- sub_line[grd_line,,op=st_covers]
  sub3 <- sub3[!sub3$INSPIREID %in% sub2$INSPIREID,]
  if(nrow(sub3) > 0){
    sub2 <- rbind(sub2, sub3)
  }
  rm(sub3)
  
  # qtm(sub2) + qtm(sub3, lines.col = "red")
  # 
  # foo = sub_line[sub_line$INSPIREID == 25826067,]
  # bar = grd_line[foo,,op=st_intersects]
  # qtm(foo) + qtm(bar[5,], lines.col = "red")
  # st_contains(foo, bar)
  # fizz = st_overlaps(bar, foo)
  # 
  # foo = as.data.frame(st_coordinates(foo))
  # bar = as.data.frame(st_coordinates(bar))
  # 
  # st_intersects(foo, grd_line)
  # plot(foo$GEOMETRY)
  # plot(bar, add = TRUE, col = "red")
  
  if(nrow(sub2) > 0){
    # Idea: Split the grid_line2 at the polygon boundaires so each line is only the join between two polygons
    # Then in a loop for each line select the two polygons an merge
    suppressWarnings(sub2_pt <- st_cast(sub2,"POINT"))
    sub2_pt <- sub2_pt[grd_line, ]
    sub2_pt <- sub2_pt[!duplicated(sub2_pt$GEOMETRY),]
    sub2_pt <- st_combine(sub2_pt)
    
    grd_line <- lwgeom::st_split(grd_line, sub2_pt)
    grd_line <- st_collection_extract(grd_line, "LINESTRING")
    grd_line <- st_as_sf(grd_line)
    grd_line$id <- as.character(sample(1:nrow(grd_line), nrow(grd_line)))
    grd_line <- grd_line[sub2, , op = st_covered_by]
    
    sub2 <- poly[poly$INSPIREID %in% sub2$INSPIREID,]
    sub2_new <- sub2
    
    for(j in seq_len(nrow(grd_line))){
      #message(Sys.time()," ",j)
      lin <- grd_line[j,]
      sub2_sel <- sub2_new[lin,, op = st_covers]
      # Should only have two polygons
      if(nrow(sub2_sel) > 2){
        sub2_sel <- sub2_new[lin,, op = st_intersects]
      }
      
      if(nrow(sub2_sel) <= 1){
        #message("only one geom")
        next
      } else if(nrow(sub2_sel) > 2){
        #message("more than two geom")
        next
      }
      
      sub2_new <- sub2_new[!sub2_new$INSPIREID %in% sub2_sel$INSPIREID,]
      sub2_sel_geom <- st_union(sub2_sel)
      sub2_sel <- sub2_sel[1,]
      sub2_sel$GEOMETRY <- sub2_sel_geom
      sub2_new <- rbind(sub2_new, sub2_sel)
    }
    
    poly_new <- poly[!poly$INSPIREID %in% sub2$INSPIREID, ]
    poly_new <- rbind(poly_new, sub2_new)
  } else {
    poly_new <- poly
  }
  
  poly_new$area <- as.numeric(st_area(poly_new))
  poly_new$perimiter <- as.numeric(lwgeom::st_perimeter(poly_new))
  
  # Final pass for any perfect 500m x 500m grid squares
  # (area 250,000 m2 AND perimeter 2,000 m)
  poly_squares <- poly_new[poly_new$area == 250000 & poly_new$perimiter == 2000,]
  
  if(nrow(poly_squares) > 0){
    poly_new <- poly_new[!poly_new$INSPIREID %in% poly_squares$INSPIREID,]

    for(j in seq_len(nrow(poly_squares))){
      #message(Sys.time()," ",j)
      sqr <- poly_squares[j,]
      poly_sel <- poly_new[sqr,, op = st_intersects]
      poly_new <- poly_new[!poly_new$INSPIREID %in% poly_sel$INSPIREID,]
      poly_sel <- rbind(poly_sel, sqr)
      poly_sel_geom <- st_union(poly_sel)
      poly_sel <- poly_sel[1,]
      poly_sel$GEOMETRY <- poly_sel_geom
      poly_new <- rbind(poly_new, poly_sel)
    }
  }
  
  # check class
  if("sfc_POLYGON" %in% class(poly_new$GEOMETRY)){
    # DO nothing
  } else {
    poly_mp <- poly_new[st_geometry_type(poly_new) == "MULTIPOLYGON",]
    poly_new <- poly_new[st_geometry_type(poly_new) == "POLYGON",]
    poly_mp <- st_cast(poly_mp, "POLYGON")
    poly_new <- rbind(poly_new, poly_mp)
    rm(poly_mp)
  }
  
  poly_new$area <- round(as.numeric(st_area(poly_new)))
  poly_new <- st_transform(poly_new, 4326)
  poly_new <- sf::st_make_valid(poly_new)
  polys[[i]] <- poly_new
  
  # plot(poly$GEOMETRY)
  # plot(st_transform(poly_new$GEOMETRY, 27700), add = T, border = "red")
  
  rm(poly, poly_new, grd, grd_line, lin, poly_sel, poly_sel_geom, poly_squares,
     sqr, sub,sub_line, sub2, sub2_new, sub2_pt, sub2_sel, sub2_sel_geom, j)
}

names(polys) <- gsub(".zip","",zips)

for(i in 1:length(polys)){
  x <- polys[[i]]
  x <- x[!st_is_empty(x),]
  x <- st_cast(x, "MULTIPOLYGON")
  polys[[i]] <- x
  rm(x)
}

polys_all <- rbindlist(polys, idcol = "local_authority", fill=TRUE)
polys_all <- st_as_sf(polys_all)

#TODO: add cleaning of polys_all that cross LA boundaries

polys_all <- polys_all[!duplicated(polys_all$GEOMETRY),]


saveRDS(polys_all, "data/INSPIRE_polygons_cleaned_v2.Rds")

# Select Large areas
polys_all <- polys_all[,c("INSPIREID","local_authority","area")]

polys_large <- polys_all[polys_all$area > 404686,] # 100 acres
polys_medium <- polys_all[polys_all$area > 40468,] # 10 acres

polys_all <- st_make_valid(polys_all)

st_precision(polys_all) <- 10000000
st_precision(polys_large) <- 10000000
st_precision(polys_medium) <- 10000000

st_write(polys_large,"data/tilegeojson/inspire_clean_large_v2.geojson", delete_dsn = FALSE)
st_write(polys_medium,"data/tilegeojson/inspire_clean_medium_v2.geojson", delete_dsn = FALSE)
#st_write(polys,"data/tilegeojson/inspire_clean.geojson", delete_dsn = FALSE)

write_geojson <- function(x, path, digits = 7){
  headder <- paste0('{\n"type":"FeatureCollection",\n"name": "',deparse(substitute(x)),'",')
  if(sf::st_is_longlat(x)){
    headder <- paste0(headder,'"crs": { "type": "name", "properties": { "name": "urn:ogc:def:crs:OGC:1.3:CRS84" } },\n')
  }
  headder <- paste0(headder,'"features":[')
  footer <- ']\n}'
  commas <- c(rep(',', nrow(x) - 1),'')
  x <- c(headder, 
         paste0(geojsonsf::sf_geojson(x, atomise = TRUE, digits = digits),commas),
         footer)
  data.table::fwrite(list(x), path, quote  = FALSE)
}

message(Sys.time())
write_geojson(polys_all, "data/tilegeojson/inspire_clean_v2.geojson")
message(Sys.time())

# Null Geometry in resutls for
# INSPIREID 373965 in Ashfield



# write_geojson(polys[1:10,], "data/tilegeojson/inspire_test.geojson")
# 
# 
# 
# dat <- read_sf("data/tilegeojson/listedbuildings.geojson")
# dat <- polys[1:100000,]
# 
# write_geojson(dat, "data/tilegeojson/inspire_test.geojson")
# write_sf(dat, "data/tilegeojson/inspire_test_sf.geojson", delete_dsn = TRUE)
# foo <- st_read("data/tilegeojson/inspire_test.geojson")
# 
# 
# 
# library(microbenchmark)
# 
# microbenchmark(
#   geojsonsf = {
#     write_geojson(dat, "data/tilegeojson/inspire_test.geojson")
#   },
#   sf = {
#     sf::write_sf(dat, "data/tilegeojson/inspire_test_sf.geojson", delete_dsn = TRUE, quiet = TRUE)
#   },
#   times = 2
# )





# names(polys) <- gsub(".zip","",zips)
# 
# 
#   tm_shape(foo) +
#   tm_fill() +
#     tm_shape(grd) +
#     tm_borders("red")
# 
# sub <- poly[grd, , op = st_touches]
# grd_line <- st_cast(grd,"LINESTRING")
# sub_line <- st_cast(sub,"LINESTRING")
# # Split to straight lines
# grd_line <- st_as_sf(grd_line)
# grd_line$id <- 1
# 
# grd_line <- stplanr::overline2(grd_line, "x", simplify = FALSE)
# 
# fizz = st_overlaps(grd_line, sub2)
# 
# 
# 
# 
# # Try a merge
# sub_merge = sub[sub$INSPIREID %in% sub2$INSPIREID,]
# sub_merge = st_union(sub_merge)
# sub_merge = st_cast(sub_merge, "POLYGON")
# 
# fizz <- st_crosses(grd_line2, sub_merge)
# unique(unlist(fizz))
# sub_merge_appprove = sub_merge[unique(unlist(fizz))]
# 
# # This works except for sub$INSPIREID %in% c(34538736,34538576) 
# 
# 
# qtm(sub2, fill = "blue") + qtm(sub_merge, fill = "red") + qtm(sub_merge_appprove, fill = "green")
# 
# sel <- sub[sub$INSPIREID %in% c(34538736,34538576),]
# fizz <- st_touches(sel, grd)
# buzz <- st_overlaps(sub_line[sub_line$INSPIREID == 34538736,], grd_line)
# 
# plot(sel$geometry,  col = "red")
# plot(grd[c(74,93)], add = TRUE)
# 
# 
# # Touch A but instersects A and B 34537708
# 
# sub_pt <- st_cast(sub,"POINT")
# sub_pt <- sub_pt[grd, ,op = st_touches]
# 
# qtm(sub) + qtm(sub_pt)
# 
# sub2 <- st_union(sub)
# sub2 <- st_cast(sub2, "POLYGON")
# 
# qtm(sub, fill = "red") +
#   qtm(sub2, fill = "blue")
#   
# summary(duplicated(poly$INSPIREID))
# 
# poly <- st_transform(poly, 4326)
# 
# 
# qtm(poly)
# 
