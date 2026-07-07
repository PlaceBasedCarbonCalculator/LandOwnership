# Build Local Authority boundaries (data/la_bounds.geojson) with names
# matched to the AdminDistrict names used in the Land Registry data.
# Includes merging the old Northamptonshire districts into the North/West
# Northamptonshire unitaries. Used to sanity-check geocoded points.
library(sf)
library(tmap)
library(dplyr)
library(tidyr)
tmap_mode("view")

dir.create("tmp")
unzip("../../saferactive/saferactive/data/bdline_gpkg_gb.zip",
      exdir = "tmp")

la_lower <- st_read("tmp/data/bdline_gb.gpkg", layer = "district_borough_unitary")
unlink("tmp", recursive = TRUE)


la_lower <- la_lower[,c("Name","Census_Code")]
la_lower <- la_lower[substr(la_lower$Census_Code,1,1) %in% c("E","W"),]
la_lower <- st_transform(la_lower, 4326)

# Check against LR data

lr <- readRDS("data/UK_freehold_pc_single_short.Rds")

la_lower$Name2 <- toupper(la_lower$Name)
la_lower$Name2 <- gsub(" \\(B\\)","",la_lower$Name2)
la_lower$Name2 <- gsub(" BORO","",la_lower$Name2)
la_lower$Name2 <- gsub(" DISTRICT","",la_lower$Name2)
#la_lower$Name2 <- gsub("CITY OF ","",la_lower$Name2)
la_lower$Name2 <- gsub(" LONDON","",la_lower$Name2)

la_lower$Name2[la_lower$Name2 == "ABERTAWE - SWANSEA"] <- "SWANSEA"
la_lower$Name2[la_lower$Name2 == "BLAENAU GWENT - BLAENAU GWENT"] <- "BLAENAU GWENT"
la_lower$Name2[la_lower$Name2 == "BRO MORGANNWG - THE VALE OF GLAMORGAN"] <- "THE VALE OF GLAMORGAN"
la_lower$Name2[la_lower$Name2 == "CAERDYDD - CARDIFF"] <- "CARDIFF"
la_lower$Name2[la_lower$Name2 == "CAERFFILI - CAERPHILLY"] <- "CAERPHILLY"
la_lower$Name2[la_lower$Name2 == "CITY AND COUNTY OF THE CITY OF"] <- "CITY OF LONDON"
la_lower$Name2[la_lower$Name2 == "CASTELL-NEDD PORT TALBOT - NEATH PORT TALBOT"] <- "NEATH PORT TALBOT"
la_lower$Name2[la_lower$Name2 == "CITY OF LEICESTER"] <- "LEICESTER"
la_lower$Name2[la_lower$Name2 == "CITY OF PORTSMOUTH"] <- "PORTSMOUTH"
la_lower$Name2[la_lower$Name2 == "CITY OF SOUTHAMPTON" ] <- "SOUTHAMPTON" 
la_lower$Name2[la_lower$Name2 == "CITY OF STOKE-ON-TRENT" ] <- "STOKE-ON-TRENT"
la_lower$Name2[la_lower$Name2 == "CITY OF WOLVERHAMPTON"  ] <- "WOLVERHAMPTON"
la_lower$Name2[la_lower$Name2 == "THE CITY OF BRIGHTON AND HOVE" ] <- "BRIGHTON AND HOVE"
la_lower$Name2[la_lower$Name2 == "CONWY - CONWY"] <- "CONWY"
la_lower$Name2[la_lower$Name2 == "GWYNEDD - GWYNEDD"] <- "GWYNEDD"
la_lower$Name2[la_lower$Name2 == "CASNEWYDD - NEWPORT" ] <- "NEWPORT" 
la_lower$Name2[la_lower$Name2 == "MERTHYR TUDFUL - MERTHYR TYDFIL"  ] <- "MERTHYR TYDFIL"
la_lower$Name2[la_lower$Name2 == "POWYS - POWYS"  ] <- "POWYS"
la_lower$Name2[la_lower$Name2 == "PEN-Y-BONT AR OGWR - BRIDGEND"  ] <- "BRIDGEND"
la_lower$Name2[la_lower$Name2 == "SIR BENFRO - PEMBROKESHIRE"   ] <- "PEMBROKESHIRE" 
la_lower$Name2[la_lower$Name2 == "COUNTY OF HEREFORDSHIRE"   ] <- "HEREFORDSHIRE"
la_lower$Name2[la_lower$Name2 == "SIR GAERFYRDDIN - CARMARTHENSHIRE"    ] <- "CARMARTHENSHIRE"
la_lower$Name2[la_lower$Name2 == "SIR FYNWY - MONMOUTHSHIRE"    ] <- "MONMOUTHSHIRE"
la_lower$Name2[la_lower$Name2 == "WRECSAM - WREXHAM"   ] <- "WREXHAM"
la_lower$Name2[la_lower$Name2 == "SIR DDINBYCH - DENBIGHSHIRE"   ] <- "DENBIGHSHIRE"
la_lower$Name2[la_lower$Name2 == "SIR Y FFLINT - FLINTSHIRE"     ] <- "FLINTSHIRE"  
la_lower$Name2[la_lower$Name2 == "RHONDDA CYNON TAF - RHONDDA CYNON TAF"     ] <- "RHONDDA CYNON TAFF"
la_lower$Name2[la_lower$Name2 == "SIR YNYS MON - ISLE OF ANGLESEY"     ] <- "ISLE OF ANGLESEY"
la_lower$Name2[la_lower$Name2 == "ST. HELENS"     ] <- "ST HELENS"
la_lower$Name2[la_lower$Name2 == "ST. ALBANS"      ] <- "ST ALBANS" 
la_lower$Name2[la_lower$Name2 == "TORFAEN - TORFAEN"     ] <- "TORFAEN"
la_lower$Name2[la_lower$Name2 == "SIR CEREDIGION - CEREDIGION"     ] <- "CEREDIGION"
la_lower$Name2[la_lower$Name2 == "TELFORD AND WREKIN"     ] <- "WREKIN"

nn <- la_lower[la_lower$Name2 %in% c("CORBY", "EAST NORTHAMPTONSHIRE", "KETTERING", "WELLINGBOROUGH"),]
wn <- la_lower[la_lower$Name2 %in% c("DAVENTRY","NORTHAMPTON","SOUTH NORTHAMPTONSHIRE"),]

nn <- st_union(nn)
nn <- st_as_sf(data.frame(Name = "NORTH NORTHAMPTONSHIRE",
                          Census_Code = "",
                          geom = nn,
                          Name2 = "NORTH NORTHAMPTONSHIRE"))

wn <- st_union(wn)
wn <- st_as_sf(data.frame(Name = "WEST NORTHAMPTONSHIRE",
                          Census_Code = "",
                          geom = wn,
                          Name2 = "WEST NORTHAMPTONSHIRE"))


la_lower2 <- la_lower[!la_lower$Name2 %in% c("CORBY", "EAST NORTHAMPTONSHIRE", "KETTERING", "WELLINGBOROUGH",
                                             "DAVENTRY","NORTHAMPTON","SOUTH NORTHAMPTONSHIRE"),]
la_lower2 <- la_lower2[,c("Name","Census_Code", "Name2","geom")]
names(nn) <- names(la_lower2)
st_geometry(nn) <- "geom"
la_lower2 <- rbind(la_lower2, nn)

names(wn) <- names(la_lower2)
st_geometry(wn) <- "geom"
la_lower2 <- rbind(la_lower2, wn)


lr_nm <- unique(lr$AdminDistrict)
la_nm <- unique(la_lower2$Name2)

la_nm <- la_nm[order(la_nm)]
lr_nm <- lr_nm[order(lr_nm)]

summary(la_nm %in% lr_nm)
summary(lr_nm %in% la_nm)

lr_nm[!lr_nm %in% la_nm]
la_nm[!la_nm %in% lr_nm]

la_lower2 <- la_lower2[,c("Name2")]
names(la_lower2) <- c("name","geom")

st_write(la_lower2, "data/la_bounds.geojson", delete_dsn = TRUE)
