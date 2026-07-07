# Join the good/medium geocoded points back onto the ownership details
# (proprietor, company number, category) and export geojson for the map.
library(sf)
library(dplyr)
library(tmap)
tmap_mode("view")

dat = readRDS("data/bing_final/bing_geocoded_good.Rds")
datm = readRDS("data/bing_final/bing_geocoded_medium.Rds")

dat = rbind(dat, datm)

source("R/find_onedrive.R")
#source("R/address_functions.R")
onedrive <- find_onedrive()

dir.create("tmp")
unzip(file.path(onedrive,"Land Registry/UK Ownership/CCOD_FULL_2022_07.zip"),
      exdir = "tmp")

lr <- readr::read_csv("tmp/CCOD_FULL_2022_07.csv", lazy = FALSE)
unlink("tmp", recursive = TRUE)

dir.create("tmp")
unzip(file.path(onedrive,"Land Registry/Overseas Ownership/OCOD_FULL_2022_07.zip"),
      exdir = "tmp")

lro <- readr::read_csv("tmp/OCOD_FULL_2022_07.csv", lazy = FALSE)
unlink("tmp", recursive = TRUE)

lr <- lr[,c("Title Number","Tenure","Property Address",
            "Proprietor Name (1)","Company Registration No. (1)",
            "Proprietorship Category (1)")]
names(lr) <- c("Title","Tenure","Property_Address",
               "Proprietor","Company_No",
               "Category")

lro <- lro[,c("Title Number","Tenure","Property Address",
            "Proprietor Name (1)","Company Registration No. (1)",
            "Proprietorship Category (1)","Country Incorporated (1)")]
names(lro) <- c("Title","Tenure","Property_Address",
               "Proprietor","Company_No",
               "Category","Country")
lr$Country <- "UK"

lr <- rbind(lr, lro)

dat = dat[,c("Title.Number",
             "formattedAddress","entityType")]
names(dat) <- c("Title","geocoded_address","geocode_type","geometry")

dat2 = left_join(dat, lr, by = "Title")
qtm(dat2[sample(1:nrow(dat2), 1000),])

st_write(dat2,"data/tilegeojson/uk_owners2.geojson", delete_dsn = TRUE)
st_write(dat2[sample(1:nrow(dat2), nrow(dat2)/10),],"data/tilegeojson/uk_owners_sample2.geojson", delete_dsn = TRUE)
