# Split the freehold titles containing multiple postcodes (from
# import_UK_owners.R) into one row per postcode section, then one row per
# property. Saves data/UK_freehold_pc_multi_split.Rds.
library(dplyr)
library(purrr)
library(stringr)
source("R/address_functions.R")

freehold_pc_multi = readRDS("data/UK_freehold_pc_multi.Rds") # 9034

# Split at the end of eahc postcode
postcode_rx = c("\\b(?:[A-Za-z][A-HJ-Ya-hj-y]?[0-9][0-9A-Za-z]? ?[0-9][A-Za-z]{2}|[Gg][Ii][Rr] ?0[Aa]{2})\\b")


split_locs <- stringr::str_locate_all(freehold_pc_multi$`Property Address`, postcode_rx)

freehold_pc_multi <- split(freehold_pc_multi, seq_len(nrow(freehold_pc_multi)))

res <- list()

for(i in seq_len(length(freehold_pc_multi))){
  
  df_sub <- freehold_pc_multi[[i]]
  split_sub <- split_locs[[i]]
  breaks <- c(split_sub[seq(1, nrow(split_sub) - 1),2], nchar(df_sub$`Property Address`))
  starts <- c(1, breaks[seq(1, length(breaks) - 1)] + 1)
  sections <- matrix(
    c(starts,breaks)
    , ncol = 2)
  
  pa <- list()
  for(j in seq(1, nrow(sections))){
    pa[[j]] <- substr(df_sub$`Property Address`,sections[j,1],sections[j,2])
  }
  pa <- unlist(pa)
  df2 <- df_sub[rep(1, times = length(pa)),]
  df2$AddressLine <- pa
  df2$PostalCode <- stringr::str_match(df2$AddressLine, postcode_rx)[,1]
  res[[i]] <- df2
  
}

res_clean <- bind_rows(res)

# Extract the postcodes
res_clean$AddressLine <- str_replace(res_clean$AddressLine, postcode_rx, "")

# Clean up spare brackets and joining words
res_clean$AddressLine <- sub("^and\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^\\s\\sand\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^\\sand\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^[[:punct:]] and\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^\\s[[:punct:]] and\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^\\)\\s[[:punct:]] and\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^\\)[[:punct:]] ","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^[[:punct:]]\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^[[:punct:]]","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("\\s\\(\\)$","",res_clean$AddressLine)
res_clean$AddressLine <- sub("\\s\\($","",res_clean$AddressLine)



# # Clean up the postcodes
AddressLine <- map(res_clean$AddressLine, split_numbers_try)
reps <- lengths(AddressLine)
AddressLine <- unlist(AddressLine)

res_clean2 <- res_clean[rep(1:nrow(res_clean), times = reps),]
res_clean2$AddressLine <- AddressLine

res_clean2$CountryRegion <- "GB"


saveRDS(res_clean2,"data/UK_freehold_pc_multi_split.Rds") #170,370
