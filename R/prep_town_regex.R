# Explore common words in OSM place names (see also prep_roadname_regex.R,
# which does the same for road names). Strips common road-type suffixes to
# find distinctive name stems.
place = read.csv("data/osm_unique_place_names.csv")
place = place$name

words = strsplit(place," ")
words = unlist(words)

feq = as.data.frame(table(words))
feq = feq[order(feq$Freq, decreasing = TRUE),]

common_roads = c("Road","Close","Lane","Street","Drive","Avenue","Way","Court",
                 "Place","Gardens","Crescent","Grove","Hill","Park","Terrace",
                 "Green","Walk","View","Mews","Bridge","Rise","Square")

common_roads_rx <- paste0("\\b((",paste(common_roads, collapse = ")|("),"))$")
place2 <- stringi::stri_replace_all_regex(str = place,
                                         pattern =  common_roads_rx,
                                         replacement = "",
                                         opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
place2 <- trimws(place2)
place3 <- unique(place2)

place_stems = data.frame(place3 = place3)
