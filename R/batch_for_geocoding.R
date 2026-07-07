# Break a prepared address dataset (data/<type>.Rds) into CSVs of 50,000
# rows - the Bing daily free quota - saved to OneDrive for_geocoding/,
# where auto_geocode.R picks them up one per day.

type = "UK_leashold_split"

addr <- readRDS(paste0("data/",type,".Rds"))

cols <- c("Id","Title Number","Property Address","AddressLine","AdminDistrict","PostalCode")
addr <- addr[,cols]
names(addr) = c("Id","Title Number","Property Address","addressLine","adminDistrict","postalCode")
addr$countryRegion = "GB"

path_main <- "D:/OneDrive - University of Leeds/Data/Land Ownership"

# split into batches of 50,000
n <- 50000
nr <- nrow(addr)
list_split <- split(addr, rep(1:ceiling(nr/n), each=n, length.out=nr))


for(i in seq_len(length(list_split))){
#for(i in 1){
  sub <- list_split[[i]]
  path <- paste0(path_main,"/for_geocoding/",type,"_batch_",stringr::str_pad(i,3, pad = "0"),".csv")
  write.csv(sub, path,
            row.names = FALSE,
            fileEncoding = "UTF-8",
            na = "")
  # headder <- paste0("Bing Spatial Data Services, 2.0, ",type,"_batch_",i)
  # headder <- c(headder, paste(names(sub), collapse = ", "))
  # write.table(headder, file = path,
  #             row.names=FALSE, 
  #             col.names=FALSE,
  #             quote = FALSE,
  #             fileEncoding = "UTF-8")
  # write.table(sub,  
  #             file= path, 
  #             append = TRUE, 
  #             sep=',', 
  #             row.names=FALSE, 
  #             col.names=FALSE,
  #             na = "",
  #             fileEncoding = "UTF-8")
  
}


# names(addr) <- c("TitleNumber(Edm.String)",
#                  "PropertyAddress(Edm.String)",
#                  "AddressLine(Edm.String)",
#                  "AdminDistrict(Edm.String)",
#                  "PostalCode(Edm.String)",
#                  "CountryRegion(Edm.String)")
# names(addr) <- c("Id",
#                  "GeocodeRequest/Address/AddressLine",
#                  "GeocodeRequest/Address/AdminDistrict",
#                  "GeocodeRequest/Address/PostalCode",
#                  "GeocodeRequest/Address/CountryRegion")


# addr$`Latitude(Edm.Double)` <- NA
# addr$`Longitude(Edm.Double)` <- NA
# addr$`Key(Edm.String,primaryKey)` <- seq(1, nrow(addr))

# addr$`GeocodeResponse/Point/Latitude` <- NA
# addr$`GeocodeResponse/Point/Longitude` <- NA
# addr$`GeocodeResponse/EntityType` <- NA
# addr$`GeocodeResponse/MatchCodes` <- NA
# addr$`GeocodeResponse/Confidence` <- NA
# addr$`GeocodeResponse/BoundingBox/SouthLatitude` <- NA
# addr$`GeocodeResponse/BoundingBox/WestLongitude` <- NA
# addr$`GeocodeResponse/BoundingBox/NorthLatitude` <- NA
# addr$`GeocodeResponse/BoundingBox/EastLongitude` <- NA
# addr$StatusCode  <- NA
# addr$FaultReason  <- NA
# addr$TraceId <- NA




# addr <- addr[,c("Latitude(Edm.Double)",
#                 "Longitude(Edm.Double)",
#                 "Key(Edm.String,primaryKey)",
#                 "TitleNumber(Edm.String)",
#                 "PropertyAddress(Edm.String)",
#                 "AddressLine(Edm.String)",
#                 "AdminDistrict(Edm.String)",
#                 "PostalCode(Edm.String)",
#                 "CountryRegion(Edm.String)")]
