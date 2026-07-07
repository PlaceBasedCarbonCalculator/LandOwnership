# Export the titles that never received a good/medium geocode
# (data/UK_Owners_Missing.csv and data/Overseas_Owners_Missing.csv) so the
# failures can be reviewed or handed to someone else to work on.
library(sf)
library(dplyr)
library(tmap)
tmap_mode("view")

dat = readRDS("data/bing_final/bing_geocoded_good.Rds")
datm = readRDS("data/bing_final/bing_geocoded_medium.Rds")

dat = rbind(dat, datm)

source("R/find_onedrive.R")
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


title_numbers = unique(dat$Title.Number)

lr_missing <- lr[!lr$`Title Number` %in% title_numbers, ]
lro_missing <- lro[!lro$`Title Number` %in% title_numbers, ]


lr_missing <- lr_missing[,c("Title Number","Tenure","Property Address",
                            "District","County","Region","Postcode")]

lro_missing <- lro_missing[,c("Title Number","Tenure","Property Address",
                            "District","County","Region","Postcode")]

write.csv(lr_missing, "data/UK_Owners_Missing.csv", row.names = FALSE)
write.csv(lro_missing, "data/Overseas_Owners_Missing.csv", row.names = FALSE)

