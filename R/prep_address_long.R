# Split the long single-postcode freehold addresses (from
# import_UK_owners.R) into one row per property. Addresses still longer
# than 90 characters after splitting (mostly chains of flats) are saved
# separately as too complex.
library(dplyr)
library(purrr)
library(stringr)
source("R/address_functions.R")

freehold_pc_single_long <- readRDS("data/UK_freehold_pc_single_long.Rds")# 124,347

hist(freehold_pc_single_long$n_char, breaks = seq(1,900,10))


# Even the very long ones are mostly valid addresses
# use a try approach

AddressLine <- map(freehold_pc_single_long$AddressLine, split_numbers_try)

reps <- lengths(AddressLine)
AddressLine <- unlist(AddressLine)

long2 <- freehold_pc_single_long[rep(1:nrow(freehold_pc_single_long), times = reps),]
long2$AddressLine <- AddressLine



long2$n_char2 <- nchar(long2$AddressLine)
hist(long2$n_char2, breaks = seq(1,400,10))

long2_long = long2[long2$n_char2 > 90,] # Some are still too long
long2 = long2[long2$n_char2 <= 90,]

long_fail = freehold_pc_single_long[!freehold_pc_single_long$Id %in% long2$Id,] # Chains of flats

saveRDS(long2, "data/UK_freehold_pc_single_long_simple.Rds") #458,020
saveRDS(long_fail, "data/UK_freehold_pc_single_long_complex.Rds") #2311
