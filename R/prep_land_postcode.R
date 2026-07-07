# Clean the "land with a postcode" freehold titles (from
# import_UK_owners.R): split up multi-postcode titles, normalise compass
# directions and spelling, remove known boilerplate phrases
# (data/common_land_terms.xlsx plus the list below), then split into one
# row per property. Saves data/UK_freehold_pc_land_simple.Rds.
library(purrr)
library(combinat)
library(dplyr)
library(stringr)
library(furrr)
library(tm)
library(tau)
library(corpus)

source("R/address_functions.R")
freehold_pc_land = readRDS("data/UK_freehold_pc_land.Rds")
summary(freehold_pc_land$n_postcode)

freehold_pc_land_multi = freehold_pc_land[freehold_pc_land$n_postcode > 1,]
freehold_pc_land = freehold_pc_land[freehold_pc_land$n_postcode <= 1,]

# Split at the end of eahc postcode
postcode_rx = c("\\b(?:[A-Za-z][A-HJ-Ya-hj-y]?[0-9][0-9A-Za-z]? ?[0-9][A-Za-z]{2}|[Gg][Ii][Rr] ?0[Aa]{2})\\b")


split_locs <- stringr::str_locate_all(freehold_pc_land_multi$`Property Address`, postcode_rx)

freehold_pc_land_multi <- split(freehold_pc_land_multi, seq_len(nrow(freehold_pc_land_multi)))

res <- list()

for(i in seq_len(length(freehold_pc_land_multi))){
  
  df_sub <- freehold_pc_land_multi[[i]]
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

# Clean up spare brackets and joing words
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

res_clean$`Property Address` <- res_clean$AddressLine
res_clean$AddressLine <- NULL

freehold_pc_land <- rbind(freehold_pc_land, res_clean)
rm(res_clean, res, pa, split_sub, breaks, df2, sections, split_locs, i, j, starts, df_sub,freehold_pc_land_multi)

# Clean compass directions
freehold_pc_land$`Property Address` <- gsub("northern","north",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("southern","south",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("eastern","east",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("western","west",freehold_pc_land$`Property Address`, ignore.case = TRUE)

freehold_pc_land$`Property Address` <- gsub("northly","north",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("southernly","south",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("easterly","east",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("westerly","west",freehold_pc_land$`Property Address`, ignore.case = TRUE)

freehold_pc_land$`Property Address` <- gsub("north-east","northeast",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("south-east","southeast",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("north-west","northwest",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("south-west","southwest",freehold_pc_land$`Property Address`, ignore.case = TRUE)

freehold_pc_land$`Property Address` <- gsub("north east","northeast",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("south east","southeast",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("north west","northwest",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("south west","southwest",freehold_pc_land$`Property Address`, ignore.case = TRUE)

# Clean spelling errors
freehold_pc_land$`Property Address` <- gsub("\\badjoing\\b","adjoining",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("\\badjoingin\\b","adjoining",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("\\badjoinining\\b","adjoining",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("\\bADJOINNG\\b","adjoining",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("\\badjoning\\b","adjoining",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("\\bajoining\\b","adjoining",freehold_pc_land$`Property Address`, ignore.case = TRUE)

# Analyse the text for common words/phrases

# TextDoc <- Corpus(VectorSource(freehold_pc_land$`Property Address`))
# text_sats <- term_stats(TextDoc, ngrams = 3:100)
# text_sats <- text_sats[text_sats$count > 5,]
# text_sats$support <- NULL
# text_sats$nchar <- nchar(text_sats$term)
# text_sats <- text_sats[order(text_sats$nchar, text_sats$count, decreasing = TRUE),]


# Remove strings that are shorter versions of longer strings
sub_check <- function(x,y){
  r <- sum(stringi::stri_detect_fixed(y, x, max_count = 2), na.rm = TRUE)
  if(r > 1){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# message(Sys.time())
# plan(multisession, workers = 30)
# sub = future_map_lgl(text_sats$term, sub_check, y = text_sats$term, .progress = TRUE)
# plan(sequential)
# message(Sys.time())
# 
# text_sats_unique <- text_sats[!sub,]
# text_sats_unique <- text_sats_unique[!grepl("[0-9]",text_sats_unique$term),]
# 
# write.csv(text_sats_unique, "data/common_land_terms.csv", row.names = FALSE)

# Build a term-document matrix
# TextDoc <- Corpus(VectorSource(freehold_pc_land$`Property Address`))
# TextDoc_dtm <- TermDocumentMatrix(TextDoc)
# dtm_m <- as.matrix(TextDoc_dtm)
# # Sort by descearing value of frequency
# dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
# dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# # Display the top 5 most frequent words
# head(dtm_d, 5)

text_rem <- readxl::read_excel("data/common_land_terms.xlsx")
text_rem$remove[is.na(text_rem$remove)] = "f"
text_rem1 <- unique(text_rem$term[text_rem$remove == "t"])
text_rem2 <- unique(text_rem$possible)
text_rem2 <- text_rem2[!is.na(text_rem2)]
text_rem3 <- c("Land lying to the South, East and northwest of",
               "highway land",
               "being land and buildings at ",
               "all mines minerals and quarries (except stone quarries) under the land shown edged with red on the plan of the above title filed at the Registry and being",
               "and other land",
               "The Freehold mines and minerals mines quarries minerals and mineral substances other than coal and coal mines whatsoever whether opened or unopened under the land shown edged red on the plan to the above title filed at the Land Registry being ",
               "minerals and mineral like substances under the land shown edged with red on the plan of the above title filed at the Registry and being ",
               "and land and garages at",
               "Garages and other land",
               "and land being the site of a",
               "excluding from the title so much (if any) of that land as comprises highway which is maintained at the public expense (but only so much of the relevant land which is vested in the relevant highway authority)",
               "NOTE; The land tinted green is not included in this title",
               "The Freehold land shown edged with red on the plan of the above title filed at the Registry and being",
               "and land associated with",
               "The Freehold mines and minerals lying below the land shown tinted pink on the plan of the above title filed at the Registry and being",
               "other land to the north, northwest and northeast",
               "land and buildings on the southwest",
               "and land and sub-station on the south",
               "all mines minerals and mineral substrata lying at a greater depth than 60.96 metres (200 feet) from the surface under land shown edged with red on the plan of the above title filed at the Registry and being",
               "The Freehold mines and minerals lying under the land shown tinted pink on the plan of the above title filed at the Registry and being  William King Flour Mill, Willow Road, Denham are included in the title",
               "The Freehold mines and minerals under the land shown edged and numbered 1, 2 and 3 in blue on the plan of the above title filed at the Registry and being 604, 606 and 608 Commercial Road are included in the title",
               "all mines minerals and mineral substrata lying at a greater depth than 60.96 metres (200 feet) from the surface under land shown edged with red on the plan of the above title filed at the Registry and being",
               "Railway Tunnels, Cables, Wires and Equipment situate in the subsoil and a band of subsoil three metres in width surrounding the external walls of the said tunnels and equipment beneath the land edged red on the filed plan, beneath",
               "The Freehold mines and minerals lying within or beneath the property within the land shown edged with yellow and edged with blue on the plan of the above title filed at the Registry and being  Pentre Uchaf are included in the title",
               "The Freehold royal mines and other minerals and mineral like substances, except the mines of plaster gypsum and alabaster, under the land shown edged and numbered SF567208",
               "all mines quarries minerals and mineral substances whatsoever including sand and gravel and other surface minerals whether opened or unopened within and under the land edged red on the plan of the above title filed at the Registry being",
               "beds of gypsum, anhydrite and associated interbedded mudstone (the Minerals) at depths between levels 25 metres and 45 metres above Ordnance Datum under the land shown edged with red on the plan of the above title filed at the Registry being",
               "As to the land tinted blue on the filed plan the shop numbered",
               "all that stratum of sub-soil 11 metres in depth below the land edged red on the filed plan The upper surface of which stratum is at a level equivalent to 2407 metres beneath the point marked X on the filed plan, underneath  the front of",
               "any , excluding any mines and minerals which vested in the Coal Authority pursuant to the Coal Act 1938, under the land shown edged red on the plan of the above title filed at the Registry and being ",
               "The Freehold mines and minerals lying upon and under the land shown edged and numbered EX804825 on the plan of the above title filed at the Registry and being",
               "all mines quarries minerals and mineral substances opened or unopened within and under the land shown edged with red on the plan of the above title filed at the Registry excluding any mines and minerals which vested in the Coal Commission pursuant to the Coal Act 1938 and being",
               "all Royal mines of gold and silver and all other mines minerals and mineral like substances (other than coal and mines of coal vested in the Coal Commission pursuant to the Coal Act 1938) land shown edged with red on the plan of the above title filed at the Registry and being",
               "all mines and minerals being evaporites including potash polyhalite salt and intermingled minerals and other minerals lying below or at a depth in excess of 600 metres from the surface thereof under land shown edged with red on the plan of the above title filed at the Registry being",
               "are included in the title.  As to the land tinted mauve on the filed plan only the first floor flat belonging to and forming",
               "using the subsoil or undersurface of the said land and of constructing (but by underground workings only) and maintaining in or through such subsoil or undersurface tunnels or works authorised by the said Act of 1936. Together with the space occupied by such tunnels and works  subsoil excavated in the construction thereof. The land affected by such easement or right is",
               "all mines and minerals metal  substance or product whatsoever beyond a distance of 60.96m (200 feet) from the surface and whether opened or unopened worked or unworked, excluding any mines and minerals which vested in the Coal Authority pursuant to the Coal Act 1938, under land shown edged with red on the plan of the above title filed at the Registry and being",
               "As to the part edged and numbered 3 in blue on the title plan, excluded from the title is the  the property constructed at first floor level upwards  air space thereby and situate over the land edged and numbered 3 in blue on the title plan, but including all parts of the land edged and numbered 3 in blue on the title plan below the under surface of the property constructed at first floor level",
               "including the following ancillary powers of working granted by a Conveyance of the same  dated 30 July 1969 made between (1) Martha Massey (Vendor) and (2) Imperial Chemical Industries Limited (Purchaser):-  Including full right and liberty for the Purchaser and its assigns and all persons authorised by it to take all usual and necessary means for searching getting pumping and taking away  through any adjoining properties but subject to the payment by the Purchaser and its assigns to the Vendor and her successors in title or other person or persons entitled thereto of compensation for all damage or injury which she or they or her or their tenants might sustain by reason of the working of the said mines and minerals mineral substances salt rock or salt brine including any damage or injury occasioned to the surface of the land or to any buildings for the time being erected thereon",
               "using the subsoil or undersurface of the said land and of constructing (but by underground workings only) and maintaining in or through such subsoil or undersurface tunnels or works authorised by the said Act of 1936. Together with the space occupied by such tunnels and works  subsoil excavated in the construction thereof. The land affected by such easement or right is",
               "The Freehold mines and minerals lying under or upon the land shown hatched mauve on the plan filed at the Registry and being Lower Forest, Treflach, Oswestry SY10 9HT are included in this title.   The Freehold mines and minerals lying under or upon the land tinted mauve on the plan filed at the Registry and being Middle Forest, Treflach, Oswestry are included in this title",
               "The Freehold mines and minerals lying under the land shown edged and numbered in green and on the plan of the above title filed at the Registry and being land at",
               "all mines minerals metal  substance or product whatsoever beyond the distance of 60.96m (200 feet) from the surface and whether opened or unopened worked or unworked, excluding any mines and minerals which vested in the Coal Authority pursuant to the Coal Act 1938, under the land shown edged with red on the plan of the above title filed at the Registry and being ",
               "all mines minerals metal  substance or product whatsoever beyond the distance of 60.96 metres (200 feet) from the surface and whether opened or unopened worked or unworked, excluding any mines and minerals which vested in the Coal Authority pursuant to the Coal Act 1938, under the land shown edged with red on the plan of the above title filed at the Registry and being  ",
               "all the mines and minerals under or within the land comprised in a conveyance dated 2 August 1944 made between (1) G B Holt and (2) G W Conner and a conveyance dated 15 March 1968 made between (1) T Foxton (2) Lloyds Bank Limited and (3) G W Conner Limited lying under the land shown edged with red on the plan of the above title filed at the Registry and being",
               "all mines quarries minerals and mineral substances whatsoever (including sand and gravel and all other surface minerals  avoidance of doubt the air space or void created from time to time by the winning working getting and carrying away of any of the said minerals and mineral substances upon and under the land shown edged with red on the plan of the above title filed at the Registry and being  ",
               "all mines, quarries, minerals and mineral substances whatsoever including all stone, shales clays and common strata of the district and all sand and gravel and other surface minerals and any underground air space or void created by the winning and working getting and carrying away of the minerals under the land shown edged with red on the plan of the above Title filed at the Registry and being  ",
               "As to the part edged and numbered 3 in blue on the title plan, excluded from the title is the  the property constructed at first floor level upwards  air space thereby and situate over the land edged and numbered 3 in blue on the title plan, but including all parts of the land edged and numbered 3 in blue on the title plan below the under surface of the property constructed at first floor level",
               "under the land shown edged with red on the plan of the above Title filed at the Registry and being",
               "the subsoil or undersurface of a piece of  the retained land (as hereinafter defined underground structure or tunnel about twelve feet in diameter or Firstly hereinbefore described in a northwest direction towards the east corner of the buildings and premises known as the St. Margarets Bay Telephone Repeater Station as marked by the letters TRS on the said plan at a depth thereunder of not less than forty feet measured from the mean natural ground level to the uppermost  such structure or tunnel",
               "including the following ancillary powers of working reserved by a Conveyance of the land dated 8 February 1971 made between (1) Imperial Chemical Industries Limited (Vendor) and (2) Olive Leslie Hollinshead:-  EXCEPT AND RESERVED as is in the Schedule more particularly mentioned                        THE SCHEDULE referred to            Exceptions and Reservations in favour of the Vendor  ALL mines and minerals and underground substances of every description including salt salt rock and brine and brine springs including the right to search for win and work ",
               "following ancillary powers of working  16 January 1963 made between (1) Imperial Chemical Industries Limited (Vendors) and (2) Gerard Henry Bird and Mary Bird:-  EXCEPT AND RESERVING unto  in fee simple all mines and minerals  of every description including salt salt rock and brine  under the property hereby assured and all easements powers and rights (including the right to lay pipes) necessary or proper to enable  and persons authorised by them to win work get pump and take away ",
               "in green  plan of the above title filed at the Registry and being",
               "The description of the registered estate is an entry made under rule 5(a) of the Land Registration Rules 2003 and it is not a note to which paragraph 2 of Schedule 8 to the Land Registration Act 2002 refers that the registered estate includes the mines or minerals under the land edged and numbered in green on the title plan. The mines and minerals under the said l title plan are only included in the registration to the extent that they were so included before the Transfers of the said land edged and numbered in green",
               ", including the following ancillary powers of working granted by a Conveyance of the same  dated 30 July 1969 made between (1) Martha Massey (Vendor) and (2) Imperial Chemical Industries Limited (Purchaser):-  Including full right and liberty for the Purchaser and its assigns and all persons authorised by it to take all usual and necessary means for searching getting pumping and taking away  through any adjoining properties but subject to the payment by the Purchaser and its assigns to the Vendor and her successors in title or other person or persons entitled thereto of compensation for all damage or injury which she or they or her or their tenants might sustain by reason of the working of the said mines and minerals mineral substances salt rock or salt brine including any damage or injury occasioned to the surface of the land or to any buildings for the time being erected thereon",
               "all mines minerals and quarries (except stone quarries) under the land shown edged with red on the plan of the above title filed at the Registry and being",
               "he description of the registered estate is an entry made under rule 5(a) of the Land Registration Rules 2003 and it is not a note to which paragraph 2 of Schedule 8 to the Land Registration Act 2002 refers that the registered estate includes the mines or minerals under the land edged and numbered in green on the title plan. The mines and minerals under the said l title plan are only included in the registration to the extent that they were so included before the Transfers of the said land edged and numbered in green",
               "() are included in the title.                                         The description of the registered estate is an entry made under rule 5(a) of the Land Registration Rules 2003 and it is not a note to which paragraph 2 of Schedule 8 to the Land Registration Act 2002 refers that the registered estate includes the mines or minerals under the land edged and numbered in green on the title plan. The mines and minerals under the said land on the title plan are only included in the registration to the extent that they were so included before the Transfers of the said land edged and numbered in green"
               )


text_rem <- data.frame(term = c(text_rem1,text_rem2,text_rem3))
text_rem$nchar <- nchar(text_rem$term)
text_rem <- text_rem[order(text_rem$nchar, decreasing = TRUE),]
text_rem <- text_rem[!is.na(text_rem$term),]

remove_strings <- function(x, y){
  for(i in seq_along(y)){
    for(j in 1:3){
      x <- stringi::stri_replace_all_fixed(x, y[i],"", 
                                           opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE))
    }
  }
  x
}

# y = text_rem$term
# x = freehold_pc_land$`Property Address`[40000]
# 
# x
# remove_strings(x,y)

message(Sys.time())
plan(multisession, workers = 30)
AddressLine = future_map_chr(freehold_pc_land$`Property Address`, remove_strings, y = text_rem$term, .progress = TRUE)
plan(sequential)
message(Sys.time())

# Do it 
# AddressLine <- data.frame(AddressLine = AddressLine)
# AddressLine$nchar <- nchar(AddressLine$AddressLine)
# AddressLine$`Property Address` <- freehold_pc_land$`Property Address`


# Checks
# foo = AddressLine[grepl("\\bbuilding\\b",AddressLine$AddressLine, ignore.case = TRUE),]
# foo = foo[order(foo$nchar, decreasing = TRUE),]
# 
# write.csv(foo, "data/common_land_terms2.csv", row.names = FALSE)

freehold_pc_land$AddressLine <- AddressLine
freehold_pc_land <- freehold_pc_land[,c("Id","Title Number","Property Address","AddressLine","AdminDistrict","PostalCode")]
freehold_pc_land$nchar <- nchar(freehold_pc_land$AddressLine)

# foo <- freehold_pc_land[freehold_pc_land$nchar > 100,]
# write.csv(foo, "data/common_land_terms3.csv", row.names = FALSE)

freehold_pc_land$n_postcode <- stringi::stri_count_regex(freehold_pc_land$AddressLine, postcode_rx)
summary(freehold_pc_land$n_postcode)
freehold_pc_land$AddressLine <- str_replace(freehold_pc_land$AddressLine, postcode_rx, "")
freehold_pc_land$AddressLine <- str_replace(freehold_pc_land$AddressLine, "\\(\\)", "")

AddressLine <- map(freehold_pc_land$AddressLine, split_numbers_try)

reps <- lengths(AddressLine)
AddressLine <- unlist(AddressLine)

long2 <- freehold_pc_land[rep(1:nrow(freehold_pc_land), times = reps),]
long2$AddressLine <- AddressLine

long2$n_char2 <- nchar(long2$AddressLine)
hist(long2$n_char2, breaks = seq(0,420,10))

# long2_long = long2[long2$n_char2 > 90,] # Some are still too long
# long2 = long2[long2$n_char2 <= 90,]
# 
# long_fail = freehold_pc_land[!freehold_pc_land$Id %in% long2$Id,] # 

saveRDS(long2, "data/UK_freehold_pc_land_simple.Rds") #178183
# saveRDS(long_fail, "data/UK_freehold_pc_land_complex.Rds") #

# write.csv(long_fail, "data/common_land_terms3.csv", row.names = FALSE)




stop()

# # Some of these addresses nolonger refer to land
# res_clean$land <- grepl("\\bland\\b", res_clean$AddressLine, ignore.case = TRUE)
# summary(res_clean$land)
# 
# res_clean_land <- res_clean[res_clean$land,]
# res_clean <- res_clean[!res_clean$land,]
# 
# # Some places are  address" and land  "
# #TODO: and  the land and buildings at the rear
# andland_regex = "\\band (other )?(adjoining )?(adjacent )?(surrounding )?(amenity )?(associated )?(garden )?land\\b"
# 
# freehold_pc_land$andLand <- grepl(andland_regex, 
#                                   freehold_pc_land$`Property Address`, ignore.case = TRUE)
# summary(freehold_pc_land$andLand)
# 
# freehold_pc_land_andland <- freehold_pc_land[freehold_pc_land$andLand,]
# freehold_pc_land <- freehold_pc_land[!freehold_pc_land$andLand,]
# 
# res_clean_land$andLand <- grepl(andland_regex, 
#                                 res_clean_land$AddressLine, ignore.case = TRUE)
# summary(res_clean_land$andLand)
# 
# res_clean_land_andland <- res_clean_land[res_clean_land$andLand,]
# res_clean_land <- res_clean_land[!res_clean_land$andLand,]
# 
# 
# # Common land phrases
# # land adjoining
# # land lying to the south of
# # land on the west side of
# # land on the south west side of
# # land at the back of 
# # land at the rear of 
# # land associated with 
# # ... and land and buildings at
# # ... and land and building on the North side ..
# # ... and land adjoining
# # ... and other land
# # land at 
# # land fronting on
# # ...  and land at the rear 
# 
# 
# 
# # Some have text after the postcode, and removing poscoe reduce string length
# postcode <- str_extract_all(freehold_pc_land$`Property Address`, postcode_rx)
# 
# postcode[lengths(postcode) == 0] <- NA
# summary(lengths(postcode))
# postcode <- unlist(postcode)
# summary(postcode == freehold_pc_land$PostalCode)
# 
# pa <- str_split(freehold_pc_land$`Property Address`, postcode_rx)
# summary(lengths(pa))
# pa1 <- sapply(pa,`[[`, 1)
# pa2 <- sapply(pa[lengths(pa) == 2],`[[`, 2)
# #unique(pa2)
# pa2 <- pa2
# 
# freehold_pc_land$`Property Address` <- pa1
# 
# freehold_pc_land$land <- grepl("\\bland\\b",freehold_pc_land$`Property Address`, ignore.case = TRUE)
# 
# freehold_pc_land_notland <- freehold_pc_land[!freehold_pc_land$land,]
# freehold_pc_land <- freehold_pc_land[freehold_pc_land$land,]
# 
# 
# # Produce possible introductions
# compas = c("north","east","south","west",
#            "northeast","southeast","northwest","southwest",
#            "front","back")
# 
# compas2 = expand.grid(compas, compas)
# compas2 = compas2[compas2$Var1 != compas2$Var2,]
# 
# compas <- c(compas, paste0(compas2$Var1," and ",compas2$Var2), paste0(compas2$Var1," side and ",compas2$Var2))
# 
# 
# lnd = c("land ","land and buildings ","land and building ","land an buildings ","the land and buildings ",
#         "garden land ","front garden land ","glebe land ",
#         "land and buildings at the back of and ", "land at Access Road ",
#         "being land and buildings ","forming part of the ",
#         "Land and Apartments ","land and Apartment Block ","",
#         "land and barn ","land and barns ",
#         "land and flats ","land and garage ","land and garages ",
#         "Land and premises ","land and properties ",
#         "the land ","highway land ","amenity land ",
#         "two parcels of land ","three parcels of land ","four parcels of land ",
#         "a strip of land ","strip of land ",
#         "being land ",
#         "all mines minerals and quarries (except stone quarries) under the land shown edged with red on the plan of the above title filed at the Registry and being ",
#         "all mines minerals and quarries (except stone quarries) under the land shown edged with red on the plan of the above title filed at the Registry and being land ",
#         "Mines of Coal Lead Ores Metals and Minerals Rock Salt and Brine Springs under the land shown edged with red on the plan of the above title filed at the Registry and being ")
# # Identify the type of land description
# # Idea one identified can be removed to get geocodeable address
# 
# jn1 = c("lying to the ","lying on the ","lying to the back ","lying to the read ","lying to ",
#         "on the ","to the ","on ","adjoining on the ","adjoining the ","at the ","adjoining ",
#         "fronting to the ")
# jn2 = c("at the back ","at back ","at the rear ","at rear ","to the rear of",
#         "at the back and side ","at the rear and side ",
#         "lying to the back ", "lying to the rear ",
#         "associated with ","association with ",
#         "at ","at, ","in ","and ","","of ", "of the ","off ",
#         "known as ","formerly known as ",
#         "being the former site of ","being part ",
#         "comprising plots ","comprising ",
#         "at the side of ","adjoining ","adjoining","adjoining on the ",
#         "adjoining at the back ","adjoining the back and south east side ",
#         "adjoining the front ",
#         "adjacent to",
#         "at the front ","fronting on ","fronting ","in the front ","in front ","the front ","at front ",
#         "at the front of and garage at the back ",
#         "at the front and side ",
#         "the front and back ","at the front and back ","front garden ","the roadway in front ",
#         "forming part ","comprising part ",
#         "forming part of the highway fronting ",
#         "forming part of the highway at the back ",
#         "forming part of the highway and lying to the front ",
#         "forming part of the roadway in front ",
#         "forming part of the road at the back ",
#         "forming part of the site ",
#         "forming part of the nature area at the back ",
#         "forming part of the garden at the back ",
#         "at the front of and parking areas at the back ",
#         "part of ",
#         "numbered ")
# end = c("of ","side ","side of ","side and back of ","side of and at the back of ","",",","and back of ","and at the back of ",
#         "and West sides of and at the back of ","side and at the back of ")
# 
# perms = expand.grid(lnd, jn1, paste0(compas," "),end)
# perms = paste0(perms$Var1, perms$Var2, perms$Var3, perms$Var4)
# 
# perms2 = expand.grid(lnd, jn2, end)
# perms2 = paste0(perms2$Var1, perms2$Var2, perms2$Var3)
# 
# perms_all = c(perms, perms2)
# perms_all = unique(perms_all)
# perms_all = perms_all[perms_all != ""]
# 
# # Not every combinations exists, Check if the perms_all are in the data
# # The longer perms_all is increases the number of checks exponentially
# # So this pre-check if worth it if it is first.
# 
# # check_present <- function(x, y){
# #   
# #   int_fun <- function(a, b){
# #     grepl(a, b, ignore.case = TRUE)
# #   }
# #   
# #   for(i in 1:length(y)){
# #     if(int_fun(x, y[i])){
# #       return(TRUE)
# #     }
# #   }
# #   return(FALSE)
# # }
# # 
# # check_present2 <- function(x, y){
# #   purrr::detect(.x = y, .f = grepl, pattern = x, ignore.case = TRUE)
# # }
# # 
# # check_present3 <- function(x, y){
# #   any(grepl(x, y, ignore.case = TRUE))
# # }
# 
# check_present4 <- function(x, y){
#   any(stringi::stri_detect_fixed(y, x, max_count = 1, case_insensitive = TRUE))
# }
# 
# # f1 = stringi::stri_detect_fixed(freehold_pc_land$`Property Address`, perms_all, max_count = 1, case_insensitive = TRUE)
# # summary(f1)
# # 
# # bench::mark(f1 = any(grepl(perms_all[1], freehold_pc_land$`Property Address`, ignore.case = TRUE)),
# #             f2 = check_present(x = perms_all[1], y = freehold_pc_land$`Property Address`),
# #             f3 = check_present2(x = perms_all[1], y = freehold_pc_land$`Property Address`),
# #             f4 = check_present3(x = perms_all[1], y = freehold_pc_land$`Property Address`),
# #             f5 = stringi::stri_detect_fixed(freehold_pc_land$`Property Address`, perms_all[100], max_count = 1, case_insensitive = TRUE),
# #             check = FALSE)
# # 
# # system.time(f1 <- map_dbl(perms_all[1:1000], check_present3, y = freehold_pc_land$`Property Address`))
# # system.time(f1 <- map_dbl(perms_all[1:1000], check_present4, y = freehold_pc_land$`Property Address`))
# 
# # FOr a quick match
# # A tibble: 5 × 13
# # expression      min       median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time              
# # <bch:expr>    <bch:tm>  <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm>           
# #   1 f1         106.95ms  107.5ms      9.30     458KB     0        5     0      538ms  
# #   2 f2          906.5µs  969.5µs   1019.          0B     2.37   430     1      422ms 
# #   3 f3           1.05ms    1.1ms    895.          0B     2.37   378     1      422ms 
# #   4 f4         106.38ms  108.6ms      9.19     458KB     0        5     0      544ms 
# #   5 f5          51.72ms   52.2ms     18.2      458KB     0       10     0      549ms
# 
# # For a no match
# # A tibble: 5 × 13
# # expression          min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time                
# # <bch:expr>     <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm>          
# #   1 f1         109.36ms 110.05ms     9.07      458KB     0        5     0   551.15ms  
# #   2 f2            2.92s    2.92s     0.342        0B     1.71     1     5      2.92s 
# #   3 f3            3.16s    3.16s     0.317        0B     1.90     1     6      3.16s 
# #   4 f4         112.23ms 114.22ms     8.76      458KB     0        5     0   570.82ms 
# #   5 f5          51.53ms  52.21ms    19.0       458KB     0       10     0   526.81ms
# # My methos is fastest when there is a match but really so slow when no match
# # Stringi is 52 ms regardless - which is odd
# 
# 
# message(Sys.time()) # Take about 50 minutes
# plan(multisession, workers = 30)
# perms_all_real = future_map_lgl(perms_all, check_present4, y = freehold_pc_land$`Property Address`, .progress = TRUE)
# plan(sequential)
# message(Sys.time())
# 
# perms_sub = perms_all[perms_all_real]
# 
# message(Sys.time()) # Takes bout 5 minutes
# plan(multisession, workers = 30)
# freehold_pc_land$land_type = future_map_chr(freehold_pc_land$`Property Address`, check_match2, perms = perms_sub, .progress = TRUE)
# plan(sequential)
# message(Sys.time())
# 
# # bar <- as.data.frame(table(freehold_pc_land$land_type))
# summary(is.na(freehold_pc_land$land_type))
# 
# freehold_pc_land$land_type_multi <- grepl("|",freehold_pc_land$land_type, fixed = TRUE) 
# 
# freehold_pc_land_complex <- freehold_pc_land[is.na(freehold_pc_land$land_type) | 
#                                                freehold_pc_land$land_type_multi,]
# freehold_pc_land_simple <- freehold_pc_land[!is.na(freehold_pc_land$land_type) & 
#   !freehold_pc_land$land_type_multi,]
# 
# 
# freehold_pc_land_simple$AddressLine  <- map2_chr(.x = freehold_pc_land_simple$land_type, 
#                                           .y = freehold_pc_land_simple$`Property Address`, 
#                                           .f = gsub, replacement = "", ignore.case = TRUE)
# 
# 
# # TODO: nearly there clean up, split addresses, and send for geocoding
# #foo = freehold_pc_land_simple$AddressLine[grepl("\\bside\\b",freehold_pc_land_simple$AddressLine, ignore.case = FALSE)]
# # A few left  but into diminishing returns
# 
# # Clena out trailing (
# freehold_pc_land_simple$AddressLine <- gsub("\\($","",freehold_pc_land_simple$AddressLine,)
# freehold_pc_land_simple$n_numbers <- stringi::stri_count_regex(freehold_pc_land_simple$AddressLine, '\\d+')
# # Most of these are Land near address so one point per title make more sence
# # But some are multiple addresses
# # So split
# 
# freehold_pc_land_simple_multinumb <- freehold_pc_land_simple[freehold_pc_land_simple$n_numbers > 1,]
# freehold_pc_land_simple <- freehold_pc_land_simple[freehold_pc_land_simple$n_numbers <= 1,]
# 
# 
# 
# freehold_pc_land_simple_multinumb$tosplit <- !grepl(" (side)?(front)?(back)?(rear)?(north)?(south)?(east)?(west)?(adjoining)? ", 
#                                                     freehold_pc_land_simple_multinumb$land_type, ignore.case = TRUE)
# summary(freehold_pc_land_simple_multinumb$tosplit)
# 
# freehold_pc_land_simple_multinumb_split <- freehold_pc_land_simple_multinumb[freehold_pc_land_simple_multinumb$tosplit, ]
# freehold_pc_land_simple_multinumb_nosplit <- freehold_pc_land_simple_multinumb[!freehold_pc_land_simple_multinumb$tosplit, ]
# 
# # Part 1
# freehold_pc_land_simple_multinumb_nosplit$AddressLine <- map_chr(freehold_pc_land_simple_multinumb_nosplit$AddressLine, clean_numbers)
# 
# # Part 2
# AddressLine <- map(freehold_pc_land_simple_multinumb_split$AddressLine, split_numbers_try)
# reps <- lengths(AddressLine)
# AddressLine <- unlist(AddressLine)
# freehold_pc_land_simple_multinumb_split <- freehold_pc_land_simple_multinumb_split[rep(1:nrow(freehold_pc_land_simple_multinumb_split), times = reps),]
# freehold_pc_land_simple_multinumb_split$AddressLine <- AddressLine
# 
# # Part 3
# #freehold_pc_land_simple$AddressLine <- map_chr(freehold_pc_land_simple$AddressLine, clean_numbers)
# 
# #Part 4
# # DOn't knwo what to do with these
# #freehold_pc_land_complex
# 
# # Part 5
# #freehold_pc_land_andland
# p1 = c("","and ")
# p2 = c("land ")
# p3 = c("","and buildings ","and building ")
# p4 = c("","at the back ","at the rear ","at the side ","adjoining ")
# p5 = c("","of ")
# 
# perms = expand.grid(p1,p2,p3,p4,p5)
# perms = paste0(perms$Var1, perms$Var2, perms$Var3, perms$Var4, perms$Var5)
# perms = unique(perms)
# 
# stop("To Do Idetify all the perms of this df")
# 
# message(Sys.time()) # Takes bout 1 minutes
# plan(multisession, workers = 30)
# freehold_pc_land_andland$land_type = future_map_chr(freehold_pc_land_andland$`Property Address`, check_match2, perms = perms, .progress = TRUE, startonly = FALSE)
# plan(sequential)
# message(Sys.time())
# summary(is.na(freehold_pc_land_andland$land_type))
# 
# 
# AddressLine <- map(freehold_pc_land_andland$AddressLine, split_numbers_try)
# reps <- lengths(AddressLine)
# AddressLine <- unlist(AddressLine)
# freehold_pc_land_andland <- freehold_pc_land_andland[rep(1:nrow(freehold_pc_land_andland), times = reps),]
# freehold_pc_land_andland$AddressLine <- AddressLine
# 
# 
# 
# #  Part 6
# res_clean_land_andland
# res_clean_land
# res_clean