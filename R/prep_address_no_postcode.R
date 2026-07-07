# Split the no-postcode freehold addresses (from import_UK_owners.R) into
# one row per property. Substations and "part of" titles are set aside as
# too complex/not geocodable.
library(purrr)
library(furrr)
source("R/address_functions.R")

freehold_nopc = readRDS("data/UK_freehold_nopc.Rds") # 477,044

# SOme of these have clear addresses

# Remove some problems
freehold_nopc$n_char <- nchar(freehold_nopc$`Property Address`)
#freehold_nopc_good <- freehold_nopc[freehold_nopc$n_char < 84,]

freehold_nopc_good = freehold_nopc[!grepl("substation", freehold_nopc$`Property Address`,ignore.case = TRUE),]
freehold_nopc_good = freehold_nopc_good[!grepl("sub-station", freehold_nopc_good$`Property Address`,ignore.case = TRUE),]
freehold_nopc_good = freehold_nopc_good[!grepl("sub station", freehold_nopc_good$`Property Address`,ignore.case = TRUE),]
freehold_nopc_good = freehold_nopc_good[!grepl("part of", freehold_nopc_good$`Property Address`,ignore.case = TRUE),]

message(Sys.time())
plan(multisession, workers = 30)
AddressLine = future_map(freehold_nopc_good$`Property Address`, split_numbers_try, .progress = TRUE)
plan(sequential)

reps <- lengths(AddressLine)
AddressLine <- unlist(AddressLine)

freehold_nopc_good2 <- freehold_nopc_good[rep(1:nrow(freehold_nopc_good), times = reps),]
freehold_nopc_good2$AddressLine <- AddressLine

# QA check: any addresses where the "inclusive" qualifier was not parsed out
# foo = freehold_nopc_good2[grepl("\\binclusive\\b",freehold_nopc_good2$AddressLine),]

freehold_nopc_bad <- freehold_nopc[!freehold_nopc$Id %in% freehold_nopc_good2$Id,]

saveRDS(freehold_nopc_good2, "data/UK_freehold_nopc_simple.Rds") # 1,818,607
saveRDS(freehold_nopc_bad, "data/UK_freehold_nopc_complex.Rds") # 111,211 - lots of sub stations
