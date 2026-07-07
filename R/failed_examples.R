# Export examples of titles that could not be parsed or geocoded, for
# review and for sharing with others.
library(sf)

f1 <- readRDS("data/UK_freehold_nopc_complex.Rds")
f2 <- readRDS("data/UK_freehold_pc_single_long_complex.Rds")

f2 <- f2[,names(f1)]
fall <- rbind(f1,f2)


fall = fall[!grepl("substation", fall$`Property Address`,ignore.case = TRUE),]
fall = fall[!grepl("sub-station", fall$`Property Address`,ignore.case = TRUE),]
fall = fall[!grepl("sub station", fall$`Property Address`,ignore.case = TRUE),]

f2 <- f2[,1:6]
write.csv(f2,"data/failed_examples.csv", row.names = FALSE)


res_medium <- readRDS("data/bing_final/bing_geocoded_medium.Rds")
res_low <- readRDS("data/bing_final/bing_geocoded_low.Rds")
res_fail <- readRDS("data/bing_final/bing_geocoded_fail.Rds")
res_nola <- readRDS("data/bing_final/bing_geocoded_nola.Rds")
res_wrongla <- readRDS("data/bing_final/bing_geocoded_wrongla.Rds")

nms <- c("Title.Number","Property.Address","addressLine.x","adminDistrict.x",
         "postalCode.x","formattedAddress","confidence","entityType")

res_medium <- st_drop_geometry(res_medium[,nms])
res_low <- st_drop_geometry(res_low[,nms])
res_nola <- st_drop_geometry(res_nola[,nms])
res_wrongla <- st_drop_geometry(res_wrongla[,nms])
res_fail <- st_drop_geometry(res_fail[,nms])

res_all <- rbind(res_fail, res_nola, res_wrongla, res_low, res_medium)

write.csv(res_all,"data/failed_examples2.csv", row.names = FALSE, na = "")
