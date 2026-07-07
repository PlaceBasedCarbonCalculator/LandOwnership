# Stage 1 of the UK (CCOD) pipeline: read the "UK companies that own
# property in England and Wales" CSV and split the freehold titles into
# categories by how hard they are to geocode:
#   * single postcode, short address  -> data/UK_freehold_pc_single_short.Rds
#   * single postcode, long address   -> data/UK_freehold_pc_single_long.Rds
#   * multiple postcodes              -> data/UK_freehold_pc_multi.Rds
#   * "land" with a postcode          -> data/UK_freehold_pc_land.Rds
#   * no postcode                     -> data/UK_freehold_nopc.Rds
#   * "land" without a postcode       -> data/UK_freehold_nopc_land.Rds
# The short single-postcode addresses are also split into one row per
# property (e.g. "10-14 Example Street" becomes three rows).
library(readr)
library(purrr)

source("R/find_onedrive.R")
source("R/address_functions.R")
onedrive <- find_onedrive()

dir.create("tmp")
unzip(file.path(onedrive,"Land Registry/UK Ownership/CCOD_FULL_2022_07.zip"),
      exdir = "tmp")

lr <- readr::read_csv("tmp/CCOD_FULL_2022_07.csv", lazy = FALSE)
unlink("tmp", recursive = TRUE)

lr$Id <- seq_len(nrow(lr))

freehold <- lr[lr$Tenure == "Freehold",]
freehold <- freehold[,c(1,3:7,36)]

# Need to clean up addres to match Bing API needs
# AddressLine,	        Locality,	  AdminDistrict,	PostalCode,	CountryRegion
# 120 Herrington Road, 	Washington, 	           ,	NE99 9ZZ,	  United Kingdom

names(freehold)[6] <- "PostalCode"
names(freehold)[3] <- "AdminDistrict"
freehold$CountryRegion <- "United Kingdom"

# Drop the 171 Freehold titles with no address
freehold <- freehold[!is.na(freehold$`Property Address`),]


# Split into 4 types
# Simple Address
# Land
# No postcode

# Some have multiple postcodes
freehold$n_postcode <- stringi::stri_count_regex(freehold$`Property Address`, "\\b(?:[A-Za-z][A-HJ-Ya-hj-y]?[0-9][0-9A-Za-z]? ?[0-9][A-Za-z]{2}|[Gg][Ii][Rr] ?0[Aa]{2})\\b")
freehold$land <- grepl("\\bland\\b", freehold$`Property Address`, ignore.case = TRUE)

freehold_pc <- freehold[!is.na(freehold$PostalCode),]
freehold_nopc <- freehold[is.na(freehold$PostalCode),]

summary(freehold_pc$n_postcode)
summary(freehold_nopc$n_postcode) # no postcode means no postcode

freehold_pc_land <- freehold_pc[freehold_pc$land,]
freehold_pc <- freehold_pc[!freehold_pc$land,]
freehold_nop_land <- freehold_nopc[freehold_nopc$land,]
freehold_nopc <- freehold_nopc[!freehold_nopc$land,]

# Simple - Postcode not land - 1,431,806
message("Simple property ",nrow(freehold_pc)," addresses ",round(nrow(freehold_pc)/nrow(freehold)*100,2)," %")
message("Simple Land ",nrow(freehold_pc_land)," addresses ",round(nrow(freehold_pc_land)/nrow(freehold)*100,2)," %")
message("Compex Property ",nrow(freehold_nopc)," addresses ",round(nrow(freehold_nopc)/nrow(freehold)*100,2)," %")
message("Complex Land ",nrow(freehold_nop_land)," addresses ",round(nrow(freehold_nop_land)/nrow(freehold)*100,2)," %")

# Start with the simple half of the data
freehold_pc_multi <- freehold_pc[freehold_pc$n_postcode >= 2, ]
freehold_pc_single <- freehold_pc[freehold_pc$n_postcode <= 1, ]

# Remove postcodes from the Address
# Somtimes in brackets
freehold_pc_single$AddressLine <- map2_chr(freehold_pc_single$`Property Address`, freehold_pc_single$PostalCode,
                 function(x,y){
                   x <- gsub(y,"",x, fixed = TRUE)
                   x <- gsub("()","",x, fixed = TRUE)
                   x
                 } )

# Some addresses are really long
freehold_pc_single$n_char <- nchar(freehold_pc_single$AddressLine)
summary(freehold_pc_single$n_char)

# but 90% are less than 54 characters
freehold_pc_single_long <- freehold_pc_single[freehold_pc_single$n_char > 54,]
freehold_pc_single_short <- freehold_pc_single[freehold_pc_single$n_char <= 54,]

# Old method jus the first number
#freehold_pc_single_short$AddressLine <- map_chr(freehold_pc_single_short$AddressLine, clean_numbers)

# New Method split each address out
# 137,000 titles have more than one numbered addrees e.g. 10-20 Example Street
freehold_pc_single_short$n_numb <- stringi::stri_count_regex(freehold_pc_single_short$AddressLine, '\\d+')

freehold_pc_single_short_multinumb = freehold_pc_single_short[freehold_pc_single_short$n_numb > 1,]
freehold_pc_single_short = freehold_pc_single_short[freehold_pc_single_short$n_numb <= 1,]


AddressLine <- map(freehold_pc_single_short_multinumb$AddressLine, split_numbers)
reps <- lengths(AddressLine)
AddressLine <- unlist(AddressLine)

freehold_pc_single_short_multinumb <- freehold_pc_single_short_multinumb[rep(1:nrow(freehold_pc_single_short_multinumb), times = reps),]
freehold_pc_single_short_multinumb$AddressLine <- AddressLine

freehold_pc_single_short <- rbind(freehold_pc_single_short, freehold_pc_single_short_multinumb)

# save the groups
saveRDS(freehold_pc_single_short,"data/UK_freehold_pc_single_short.Rds") # 1,776,987
saveRDS(freehold_pc_single_long,"data/UK_freehold_pc_single_long.Rds")# 124,000 spit into 456,337 and 2,316 too complex
saveRDS(freehold_pc_multi,"data/UK_freehold_pc_multi.Rds") # 9034 split into 168,911
saveRDS(freehold_pc_land,"data/UK_freehold_pc_land.Rds") # 122,000
saveRDS(freehold_nopc,"data/UK_freehold_nopc.Rds") # 477,000 split into 1,830,024 and 111,191 too complex
saveRDS(freehold_nop_land,"data/UK_freehold_nopc_land.Rds") # 878,000



# some have multiple numbers which is ok if they are in a block

# Check for muliple postcodes

 #https://stackoverflow.com/questions/164979/regex-for-matching-uk-postcodes








