# Clean the "land without a postcode" freehold titles (from
# import_UK_owners.R): strip mines/minerals boilerplate, compass phrases,
# land phrases and known strings (data/long_strings.xlsx and
# data/clean_strings.xlsx), then split into one row per property.
library(purrr)
library(combinat)
library(dplyr)
library(stringr)
library(furrr)
library(tm)
library(tau)
library(corpus)

source("R/address_functions.R")
postcode_rx = c("\\b(?:[A-Za-z][A-HJ-Ya-hj-y]?[0-9][0-9A-Za-z]? ?[0-9][A-Za-z]{2}|[Gg][Ii][Rr] ?0[Aa]{2})\\b")
freehold_no_land = readRDS("data/UK_freehold_nopc_land.Rds")
freehold_no_land$ORIG <- freehold_no_land$`Property Address`

text_rem <- readxl::read_excel("data/long_strings.xlsx")
text_rem$nchar <- nchar(text_rem$term)
text_rem <- text_rem[order(text_rem$nchar, decreasing = TRUE),]
text_rem <- text_rem[!is.na(text_rem$term),]


message(Sys.time())
plan(multisession, workers = 30)
freehold_no_land$`Property Address` = future_map_chr(freehold_no_land$`Property Address`, remove_strings, y = text_rem$term, .progress = TRUE)
plan(sequential)
message(Sys.time())

# Remove the really long strings
mines_start <- c("all and singular the mines","all and singular and such of the mines",
                 "all and every the mines","all and all manner of mines",
                 "all and singular the stone and underground mines","all beds",
                 "all carboniferous minerals","all clay","all coal iron",
                 "all gypsum and","all gypsum within","all iron stone",
                 "all Ironstone and","all lead","all manner of mines",
                 "all metal ores","all metallic minerals","all metals ores",
                 "all minerals","all mines","all other Mines","all quarries",
                 "all Royal Mines","all sand","all slate","all stone",
                 "All such clay","all such ironstone","all such mines","all surface clay",
                 "all that gypsum","all the mines","all the Royal Mines",
                 "all those mines","all those sand","all those ungotten",
                 "all Tin","all waste material","an Easement","any mines",
                 "and to be opened","as to such parts","Being the sratum","gypsum and anhydrite",
                 "in all mines","Ironstone and","mines and minerals",
                 "minerals and mineral","mines beds",
                 "mines minerals","Mines of","mines quarries","mines seams",
                 "mines, minerals","minerals lying below",
                 "of the land tinted","of using so","of using the","Royal mines","sand gravel",
                 "sand, gravel","shown and","shown by a red","shown by red",
                 "shown edged with red","so much of the","stratum of",
                 "the beds seams","The Freehold","the gypsum",
                 "the lead","the mine","the minerals","the mines",
                 "the ore mines","the Royal Mines","the strata of",
                 "the stratrum of","using the subsoil",
                 "clay slate stone")

mines_end <- c("filed at the Registry","construction thereof",
               "marked X on the filed plan",
               "marked Y on the filed plan",
               "deed dated",
               "registered under Title",
               "the level of")

mines_start <- paste0("(",paste(mines_start, collapse = ")|("),")")
mines_end <- paste0("(",paste(mines_end, collapse = ")|("),")")
mines <- paste0("(",mines_start,").*(",mines_end,")")
#mines <- "(all mines).*(at the registry)"
# x = "all mines and minerals whatsoever together with the mineral substrata lying at a greater depth than 60.96 metres (200 feet) from the surface under the land shown edged with red on the plan of the above title filed at the Registry being land at Durley"
# stringi::stri_replace_all_regex(str = x, 
#                                 pattern =  mines,
#                                 replacement = "@MINES", 
#                                 opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))



freehold_no_land$`Property Address` <- stringi::stri_replace_all_regex(str = freehold_no_land$`Property Address`, 
                                                                       pattern =  mines,
                                                                       replacement = "@MINES", 
                                                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))

freehold_no_land$`Property Address` <- gsub("as described below:-   House Numbers Road Names","",freehold_no_land$`Property Address`, fixed = TRUE)
freehold_no_land$`Property Address` <- gsub("as described below:-   Road Names House Numbers","",freehold_no_land$`Property Address`, fixed = TRUE)


# Clean compass directions
compass = c("northerly","easterly","southerly","westerly",
            "northern","eastern","southern","western",
           "northeast","southeast","northwest","southwest",
           "northeastern","southeastern","northwestern","southwestern",
           "north-east","south-east","north-west","south-west",
           "north east","south east","north west","south west",
           "north-eastern","south-eastern","north-western","south-western",
           "north eastern","south eastern","north western","south western",
           "north","east","south","west")

compass_col <- paste0("(",paste(compass, collapse = ")|("),")")
compass_end <- c("of","side of","sides of","corner of","junction of","side","boundary of")
compass_end <- paste0("(",paste(compass_end, collapse = ")|("),")")

compass_start <- c("the","on")
compass_start <- paste0("(",paste(compass_start, collapse = ")|("),")")
compass_col <- paste0("(",compass_start,") (",compass_col,") (",compass_end,")")

freehold_no_land$`Property Address` <- stringi::stri_replace_all_regex(str = freehold_no_land$`Property Address`, 
                                                                   pattern =  compass_col,
                                                                   replacement = "@COMPASS", 
                                                                   opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))

compass_col <- paste0("(",paste(compass, collapse = ")|("),")")
compass2 = paste0("(",compass_start,") (",
                  compass_col,
                  ")(( and )|( side and )| )(",
                  compass_col,
                  ") (",compass_end,")"
                  )
freehold_no_land$`Property Address` <- stringi::stri_replace_all_regex(str = freehold_no_land$`Property Address`, 
                                                                       pattern =  compass2,
                                                                       replacement = "@COMPASS", 
                                                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))


#fizz = freehold_no_land[grepl("north",freehold_no_land$`Property Address`, ignore.case = TRUE ),]

# Clean spelling errors
spell_adjoining <- c("\\badjoing\\b",
                     "\\badjoing\\b",
                     "\\badjoingin\\b",
                     "\\badjoinining\\b",
                     "\\bADJOINNG\\b",
                     "\\badjoning\\b",
                     "\\bajoining\\b")
spell_adjoining <- paste0("(",paste(spell_adjoining, collapse = ")|("),")")

freehold_no_land$`Property Address` <- stringi::stri_replace_all_regex(str = freehold_no_land$`Property Address`, 
                                                                       pattern =  spell_adjoining,
                                                                       replacement = "adjoining", 
                                                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))

spell_situated <- c("\\bsituate\\b")
#spell_situated <- paste0("(",paste(spell_adjoining, collapse = ")|("),")")
freehold_no_land$`Property Address` <- stringi::stri_replace_all_regex(str = freehold_no_land$`Property Address`, 
                                                                       pattern =  spell_situated,
                                                                       replacement = "situated", 
                                                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
spell_substation <- c("\\ba sub-station\\b",
                      "\\ba sub station\\b",
                      "\\ban electricity sub station\\b",
                      "\\ban electricity sub-station\\b")
spell_substation <- paste0("(",paste(spell_substation, collapse = ")|("),")")
freehold_no_land$`Property Address` <- stringi::stri_replace_all_regex(str = freehold_no_land$`Property Address`, 
                                                                       pattern =  spell_substation,
                                                                       replacement = "substation", 
                                                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))

# Clean land phrases
# lnd = c("land","land and buildings","land and building","land an buildings","the land and buildings",
#         "garden land","front garden land","glebe land","land and buildings at the back of and", "land at Access Road",
#         "being land and buildings","forming part of the","Land and Apartments","land and Apartment Block",
#         "land and barn","land and barns","land and flats","land and garage","land and garages",
#         "Land and premises","land and properties","the land","highway land","amenity land",
#         "two parcels of land","three parcels of land","four parcels of land","a strip of land","strip of land",
#         "being land","and adjoining land","land lying to","former sites of",
#         "land, farms and buildings","land, buildings and premises situate at")

l1 <- c("","the","a","being","and adjoining")
l2 <- c("","garden","front garden","glebe","highway","amenity","strip of")
l3 <- c("land")

lall = expand.grid(l1,l2,l3)
lall = paste0(lall$Var1," ",lall$Var2," ",lall$Var3)
lall = gsub("\\s+"," ",lall)
lall = trimws(lall)
lall = unique(lall)
lall <- paste0("\\b((",paste(lall[order(nchar(lall), decreasing = TRUE)], collapse = ")|("),"))\\b")

freehold_no_land$`Property Address` <- stringi::stri_replace_all_regex(str = freehold_no_land$`Property Address`, 
                                                                       pattern =  lall,
                                                                       replacement = "@LND", 
                                                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))


l4 <- c("and building","and buildings","and barn","and barns","and flat","and flats","and apartment","and apartments","and apartment block",
        "and premises","and properties","and property","and garage","and garages",
        "and parking","and car parking","and car parking spaces","and parking spaces",
        "and factory","and factory buildings","and footpaths","and foreshore",
        "and houses","and open spaces","and part of building","and part of",
        "farm house and buildings","and farm house")
l4 = unique(l4)
l4 <- paste0("\\b((",paste(l4[order(nchar(l4), decreasing = TRUE)], collapse = ")|("),"))\\b")
freehold_no_land$`Property Address` <- stringi::stri_replace_all_regex(str = freehold_no_land$`Property Address`, 
                                                                       pattern =  l4,
                                                                       replacement = "@EXTRA", 
                                                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))

l5 <- c("lying","lying to","lying at","lying between","lying on",
        "at",
        #"in" can't have in cause barrow in furness
        #"to" can't have to because break 1 to 4 etc.
        "to the rear of",
        "on","on the","on the side of","on the site of",
        "at the back","at the back of","at the corner of","at the junction of",
        "associated with",
        "adjacent to",
        "adjoining","adoining on",
        "abbuting","abbuting on","abbuting the",
        "fronting",
        "forming","forming part of","forming the site of","forming the forecourt of",
        "forming the forecourts of","forming the roadway",
        "formerly known as","formerly part of","formerly the site of",
        "known as",
        "part of",
        "situated at","situated on",
        "site of",
        "side of",
        "either side of",
        "and the")

l5 = unique(l5)
l5 <- paste0("\\b((",paste(l5[order(nchar(l5), decreasing = TRUE)], collapse = ")|("),"))\\b")
freehold_no_land$`Property Address` <- stringi::stri_replace_all_regex(str = freehold_no_land$`Property Address`, 
                                                                       pattern =  l5,
                                                                       replacement = "@POS", 
                                                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))






#Removed Known phrases
text_rem <- readxl::read_excel("data/clean_strings.xlsx")
names(text_rem) = c("term","flag")
text_rem$nchar <- nchar(text_rem$term)
text_rem <- text_rem[order(text_rem$nchar, decreasing = TRUE),]
text_rem <- text_rem[!is.na(text_rem$term),]


message(Sys.time())
plan(multisession, workers = 30)
AddressLine = future_map_chr(freehold_no_land$`Property Address`, remove_strings, y = text_rem$term, .progress = TRUE)
plan(sequential)
message(Sys.time())


AddressLine = gsub("numbered [0-9]+ on the filed plan","",AddressLine)
AddressLine = gsub("((Absolute)?)(\\s*)(Â£[0-9]+)","",AddressLine) #Wierd character strings
rgx_date = "\\b([1-9]|[0][1-9]|[12][0-9]|3[01])[- /\\.](0[1-9]|1[012])[- /\\.](19|20)\\d\\d\\b"
AddressLine = gsub(rgx_date,"",AddressLine) #Dated
AddressLine = str_squish(AddressLine)


freehold_no_land$AddressLine <- AddressLine

# # Places
# 
# common_roads = c("Road","Close","Lane","Street","Drive","Avenue","Way","Court",
#                  "Place","Gardens","Crescent","Grove","Hill","Park","Terrace",
#                  "Green","Walk","View","Mews","Bridge","Rise","Square")
# common_roads_rx <- paste0("(\\w+) ((",paste(common_roads, collapse = ")|("),"))")
# 
# freehold_no_land$`Property Address` <- stringi::stri_replace_all_regex(str = freehold_no_land$`Property Address`, 
#                                                                        pattern =  common_roads_rx,
#                                                                        replacement = "@RD", 
#                                                                        opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
# 
# place = read.csv("data/osm_unique_place_names.csv")
# place = place[!place$place %in% c("village","hamlet"),]
# place$nchar <- nchar(place$name)
# place <- place[order(place$nchar, decreasing = TRUE),]
# 
# message(Sys.time())
# plan(multisession, workers = 30)
# freehold_no_land$`Property Address` = future_map_chr(freehold_no_land$`Property Address`, 
#                      replace_strings, y = place$name, 
#                      rep = "@TWN", .progress = TRUE)
# plan(sequential)
# message(Sys.time())
# 
# 
# freehold_no_land$nchar <- nchar(freehold_no_land$`Property Address`)
# summary(freehold_no_land$nchar)
# fizz = freehold_no_land[freehold_no_land$nchar > 50,] #3840
# fizz$`Property Address` = str_remove_all(fizz$`Property Address`,"[0-9]+")
# fizz$`Property Address` = str_remove_all(fizz$`Property Address`,"@RD")
# fizz$`Property Address` = str_remove_all(fizz$`Property Address`,"@TWN")
# fizz$`Property Address` = str_remove_all(fizz$`Property Address`,"odd")
# fizz$`Property Address` = str_remove_all(fizz$`Property Address`,"even")
# fizz$`Property Address` = str_remove_all(fizz$`Property Address`,",")
# fizz$`Property Address` = str_remove_all(fizz$`Property Address`,"inclusive")
# fizz$`Property Address` = str_squish(fizz$`Property Address`)
# fizz$nchar = nchar(fizz$`Property Address`)
# fizz = fizz[fizz$nchar > 50,]
# write.csv(fizz, "data/long_terms_nopc3.csv", row.names = FALSE)
# 
# stop("Finished")
# 
# # Analyse the text for common words/phrases
# TextDoc <- Corpus(VectorSource(freehold_no_land$`Property Address`))
# text_sats <- term_stats(TextDoc, ngrams = 3:100)
# text_sats <- text_sats[text_sats$count > 5,]
# text_sats$support <- NULL
# text_sats$nchar <- nchar(text_sats$term)
# text_sats <- text_sats[order(text_sats$nchar, text_sats$count, decreasing = TRUE),]
# 
# 
# # Remove strings that are shorter versions of longer strings
# sub_check <- function(x,y){
#   r <- sum(stringi::stri_detect_fixed(y, x, max_count = 2), na.rm = TRUE)
#   if(r > 1){
#     return(TRUE)
#   } else {
#     return(FALSE)
#   }
# }
# 
# message(Sys.time())
# plan(multisession, workers = 30)
# sub = future_map_lgl(text_sats$term, sub_check, y = text_sats$term, .progress = TRUE)
# plan(sequential)
# message(Sys.time())
# 
# text_sats_unique <- text_sats[!sub,]
# text_sats_unique <- text_sats_unique[!grepl("[0-9]",text_sats_unique$term),]
# 
# write.csv(text_sats_unique, "data/common_land_terms_nopc3.csv", row.names = FALSE)
# 
#  
# 
# 
# foo = freehold_no_land[grepl(text_sats_unique$term[1], 
#                              freehold_no_land$`Property Address`,
#                              ignore.case = TRUE),]


freehold_no_land$n_postcode <- stringi::stri_count_regex(freehold_no_land$AddressLine, postcode_rx)
summary(freehold_no_land$n_postcode)
freehold_no_land$AddressLine <- str_replace(freehold_no_land$AddressLine, postcode_rx, "")
freehold_no_land$AddressLine <- str_replace(freehold_no_land$AddressLine, "\\(\\)", "")

AddressLine <- map(freehold_no_land$AddressLine, split_numbers_try)

reps <- lengths(AddressLine)
AddressLine <- unlist(AddressLine)

long2 <- freehold_no_land[rep(1:nrow(freehold_no_land), times = reps),]
long2$AddressLine <- AddressLine

long2$n_char2 <- nchar(long2$AddressLine)
hist(long2$n_char2, breaks = seq(0,700,10))

long2$`Property Address` <- long2$ORIG

long2 <- long2[,c("Title Number","Property Address","AdminDistrict","County","Region","PostalCode",
                  "Id","CountryRegion","AddressLine","n_char2")]

long2_simple <- long2[long2$n_char2 < 150,]
long2_complex <- long2[long2$n_char2 >= 150,]


saveRDS(long2_simple, "data/UK_freehold_nopc_land_simple.Rds") #1189172
saveRDS(long2_complex, "data/UK_freehold_nopc_land_complex.Rds") #1375

# Started with 878335



