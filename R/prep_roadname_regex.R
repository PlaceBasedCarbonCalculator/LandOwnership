# Explore common words in OSM road names (see also prep_town_regex.R).
# Strips the common road-type suffixes (Road, Street, ...) to find the
# distinctive name stems.
roads = read.csv("data/osm_unique_road_names.csv")
roads = roads$name

words = strsplit(roads," ")
words = unlist(words)

feq = as.data.frame(table(words))
feq = feq[order(feq$Freq, decreasing = TRUE),]

common_roads = c("Road","Close","Lane","Street","Drive","Avenue","Way","Court",
                 "Place","Gardens","Crescent","Grove","Hill","Park","Terrace",
                 "Green","Walk","View","Mews","Bridge","Rise","Square")

common_roads_rx <- paste0("\\b((",paste(common_roads, collapse = ")|("),"))$")
roads2 <- stringi::stri_replace_all_regex(str = roads, 
                                         pattern =  common_roads_rx,
                                         replacement = "", 
                                         opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
roads2 <- trimws(roads2)
roads3 <- unique(roads2)

foo = data.frame(roads3 = roads3)
