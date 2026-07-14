# Cleaning functions for Land Registry `Property Address` strings.
# Each clean_*() function replaces a category of legal boilerplate with a
# short tag (e.g. @MNS, @COMPASS, @LND) so that the remaining text is closer
# to a geocodable address, and prints how many occurrences of the key word
# remain afterwards.

# Replace "the airspace above ..." style descriptions with @ASP
clean_airspace <- function(x){
  
  # Remove the really long strings
  airspace_start <- c("the airspace","airspace","being the air space",
                      "the air space","air space",
                      "Being airspace")
  
  
  
  
  
  airspace_end <- c("being",
                 "being roof",
                 "roof of",
                 "foor of",
                 "the roof", 
                 "surface of",
                 "roof surface",
                 "above",
                 "over")
  
  airspace_start <- paste0("(",paste(airspace_start, collapse = ")|("),")")
  airspace_end <- paste0("(",paste(airspace_end, collapse = ")|("),")")
  # Lazy and bounded (audit F3): the old greedy ".*" ran to the LAST end
  # marker in the string - "being" is an end marker and a very common word,
  # so whole addresses could be swallowed. Lazy stops at the first end
  # marker; the 150-char bound stops a start marker pairing with an end
  # marker in a different clause entirely.
  airspace <- paste0("\\b(",airspace_start,")\\b.{0,150}?\\b(",airspace_end,")\\b")
  
  x <- stringi::stri_replace_all_regex(str = x, pattern =  airspace,
                                       replacement = "@ASP",
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  
  message(sum(grepl("\\bairspace\\b",x,ignore.case = TRUE))," occurances of the word 'airspace' remain")
  return(x)
  
}


# Replace long "all mines and minerals ... filed at the Registry" style
# legal descriptions with @MNS
clean_mines <- function(x){

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
                   "clay slate stone",
                   "evaporites including")
  
  mines_end <- c("filed at the Registry","construction thereof",
                 "marked X on the filed plan",
                 "marked Y on the filed plan",
                 "deed dated",
                 "registered under Title",
                 "the level of")
  
  mines_start <- paste0("(",paste(mines_start, collapse = ")|("),")")
  mines_end <- paste0("(",paste(mines_end, collapse = ")|("),")")
  # Lazy and bounded (audit F3, see clean_airspace). Mines clauses are long,
  # so the bound is generous - but no longer unlimited: the old greedy ".*"
  # could consume a real address sandwiched between two mines clauses
  # (seen in the 2026 queue: "...being 17B Curzon Street, London within the
  # area @MNS ..." where the middle of the address was eaten).
  mines <- paste0("\\b(",mines_start,")\\b.{0,300}?\\b(",mines_end,")\\b")
  
  x <- stringi::stri_replace_all_regex(str = x, pattern =  mines,
                                       replacement = "@MNS",
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  
  message(sum(grepl("\\bmines\\b",x,ignore.case = TRUE))," occurances of the word 'mines' remain")
  return(x)
  
}


# Replace compass phrases such as "on the north side of" with @COMPASS
clean_compass <- function(x){
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
  
  x <- stringi::stri_replace_all_regex(str = x, 
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
  x <- stringi::stri_replace_all_regex(str = x, pattern =  compass2,
                                       replacement = "@COMPASS",
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  
  message(sum(grepl("\\b(north|south|east|west)\\b",x,ignore.case = TRUE))," occurances of the words 'north/south/east/west' remain")
  return(x)
}


# Correct common spelling errors/variants (adjoining, situated, substation,
# airspace)
clean_spelling <- function(x){
  # Clean spelling errors
  spell_adjoining <- c("\\badjoing\\b",
                       "\\badjoing\\b",
                       "\\badjoingin\\b",
                       "\\badjoinining\\b",
                       "\\bADJOINNG\\b",
                       "\\badjoning\\b",
                       "\\bajoining\\b")
  spell_adjoining <- paste0("(",paste(spell_adjoining, collapse = ")|("),")")
  
  x <- stringi::stri_replace_all_regex(str = x,
                                       pattern =  spell_adjoining,
                                       replacement = "adjoining", 
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))

  spell_situated <- c("\\bsituate\\b")
  #spell_situated <- paste0("(",paste(spell_adjoining, collapse = ")|("),")")
  x <- stringi::stri_replace_all_regex(str = x, 
                                       pattern =  spell_situated,
                                       replacement = "situated", 
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  spell_substation <- c("\\ba sub-station\\b",
                        "\\ba sub station\\b",
                        "\\ban electricity sub station\\b",
                        "\\ban electricity sub-station\\b")
  spell_substation <- paste0("(",paste(spell_substation, collapse = ")|("),")")
  x <- stringi::stri_replace_all_regex(str = x, 
                                       pattern =  spell_substation,
                                       replacement = "substation", 
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  
  spell_airspace <- c("\\bairpsace\\b","\\bairpsace\\b","\\baorspace\\b","\\bairpace\\b")
  spell_airspace <- paste0("(",paste(spell_airspace, collapse = ")|("),")")
  x <- stringi::stri_replace_all_regex(str = x, 
                                       pattern =  spell_airspace,
                                       replacement = "airspace", 
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  
  return(x)
}

# Replace "land"-type phrases with @LND, trailing extras ("and buildings",
# "and garages", ...) with @EXTRA, and positional phrases ("adjoining",
# "to the rear of", ...) with @POS
clean_land <- function(x){
  l1 <- c("","the","a","being","and adjoining")
  l2 <- c("","garden","front garden","glebe","highway","amenity","strip of")
  l3 <- c("land")
  
  lall = expand.grid(l1,l2,l3)
  lall = paste0(lall$Var1," ",lall$Var2," ",lall$Var3)
  lall = gsub("\\s+"," ",lall)
  lall = trimws(lall)
  lall = unique(lall)
  lall <- paste0("\\b((",paste(lall[order(nchar(lall), decreasing = TRUE)], collapse = ")|("),"))\\b")
  
  x <- stringi::stri_replace_all_regex(str = x, 
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
  x <- stringi::stri_replace_all_regex(str = x, 
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
  x <- stringi::stri_replace_all_regex(str = x, 
                                       pattern =  l5,
                                       replacement = "@POS", 
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  
  message(sum(grepl("\\bland\\b",x,ignore.case = TRUE))," occurances of the word 'land' remain")
  return(x)
}

# Remove known boilerplate phrases from x in parallel. `text_rem` is a
# two-column data frame (term, flag), e.g. read from data/clean_strings.xlsx;
# longer terms are removed first. Also strips filed-plan references, mangled
# currency strings and dates.
#
# The `flag` column: rows flagged "f" are directional fragments ("@lnd @pos
# east of", "to @compass", ...) whose removal deletes the positional meaning
# of the address. They were being removed anyway because no code ever read
# the flag (audit F6); they are now excluded - the residual "east of" style
# glue is trimmed harmlessly by final_address_tidy() instead.
clean_phrases <- function(x, text_rem, workers = getOption("pipeline.clean_workers", 30)){

  names(text_rem) = c("term","flag")
  text_rem <- text_rem[!is.na(text_rem$term),]
  text_rem <- text_rem[is.na(text_rem$flag) | text_rem$flag != "f",]
  text_rem$nchar <- nchar(text_rem$term)
  text_rem <- text_rem[order(text_rem$nchar, decreasing = TRUE),]
  # removal is case-insensitive, so dedupe case-insensitively too
  text_rem <- text_rem[!duplicated(tolower(text_rem$term)),]


  message("Starting at ",Sys.time()," with ",workers," workers")
  future::plan(future::multisession, workers = workers)
  x = furrr::future_map_chr(x, remove_strings, y = text_rem$term, .progress = TRUE)
  future::plan(future::sequential)
  message("\nFinished at ",Sys.time())


  x = gsub("numbered [0-9]+ on the filed plan","",x)
  x = gsub("((Absolute)?)(\\s*)((Â?£|GBP ?)[0-9,.]+)","",x) # currency, incl. 2022 mojibake form
  rgx_date = "\\b([1-9]|[0][1-9]|[12][0-9]|3[01])[- /\\.](0[1-9]|1[012])[- /\\.](19|20)\\d\\d\\b"
  x = gsub(rgx_date,"",x) #Dated (numeric form)
  # Spelled-out dates: only removed with an unambiguous day-month-year shape
  # or a "dated"/"deed dated" prefix - bare month words can be street names.
  months_rx = "(January|February|March|April|May|June|July|August|September|October|November|December)"
  x = gsub(paste0("\\b[0-9]{1,2}(st|nd|rd|th)?( day of)? ", months_rx, ",? (19|20)[0-9]{2}\\b"), "", x, ignore.case = TRUE)
  x = gsub("\\b(deed |lease |conveyance |transfer )?dated( the)?\\b", "", x, ignore.case = TRUE)
  x = stringr::str_squish(x)

  return(x)
}

# Replace road and place names in x with @RD / @TWN tags, then report the
# common 3+ word phrases that remain (candidates for the removal lists).
# `place` is a data frame of OSM place names, e.g.
# read.csv("data/osm_unique_place_names.csv").
# If longest_only = TRUE, phrases that are substrings of longer phrases are
# dropped from the result.
analyise_text <- function(x, place, workers = 30, longest_only = TRUE){
  # Places
  common_roads = c("Road","Close","Lane","Street","Drive","Avenue","Way","Court",
                   "Place","Gardens","Crescent","Grove","Hill","Park","Terrace",
                   "Green","Walk","View","Mews","Bridge","Rise","Square")
  common_roads_rx <- paste0("(\\w+) ((",paste(common_roads, collapse = ")|("),"))")

  x <- stringi::stri_replace_all_regex(str = x,
                                       pattern =  common_roads_rx,
                                       replacement = "@RD",
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))

  place = place[!place$place %in% c("village","hamlet"),]
  place$nchar <- nchar(place$name)
  place <- place[order(place$nchar, decreasing = TRUE),]

  message("Starting at ",Sys.time()," with ",workers," workers")
  future::plan(future::multisession, workers = workers)
  x = furrr::future_map_chr(x,replace_strings, y = place$name,
                            rep = "@TWN", .progress = TRUE)
  future::plan(future::sequential)
  message("\nFinished at ",Sys.time())
  
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

  # Analyse the text for common words/phrases
  TextDoc <- tm::Corpus(tm::VectorSource(x))
  text_sats <- corpus::term_stats(TextDoc, ngrams = 3:100)
  text_sats <- text_sats[text_sats$count > 5,]
  text_sats$support <- NULL
  text_sats$nchar <- nchar(text_sats$term)
  text_sats <- text_sats[order(text_sats$nchar, text_sats$count, decreasing = TRUE),]


  # Remove strings that are shorter versions of longer strings
  sub_check <- function(x,y){
    r <- sum(stringi::stri_detect_fixed(y, x, max_count = 2), na.rm = TRUE)
    if(r > 1){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  if(longest_only){
    message("Starting at ",Sys.time()," with ",workers," workers")
    future::plan(future::multisession, workers = workers)
    sub = furrr::future_map_lgl(text_sats$term, sub_check, y = text_sats$term, .progress = TRUE)
    future::plan(future::sequential)
    message("\nFinished at ",Sys.time())
    
    text_sats_unique <- text_sats[!sub,]
    text_sats_unique <- text_sats_unique[!grepl("[0-9]",text_sats_unique$term),]
    
    return(text_sats_unique)
  } else {
    return(text_sats)
  }
  
  
}

# Collapse a vector of strings into a single word-bounded alternation regex,
# longest strings first so they match in preference to their substrings.
regex_collapse <- function(x){
  paste0("\\b((",paste(x[order(nchar(x), decreasing = TRUE)], collapse = ")|("),"))\\b")
}


# Replace floor/flat descriptions such as "ground to second floor flats
# being" with @FTS
clean_flats <- function(x){
  
  # ground to second floor flats being
  flat_start <- c("lower ground","ground","the flat","the","a")
  flat_join <- c("to","and")
  numbers <- c("first","second","third","fourth","fifth","sixth","seventh",
               "eighth","eight","ninth","tenth","eleventh",
               "twelfth","thirteenth","fourteenth",
               "1st","2nd","3rd","4th","5th","6th","7th",
               "8th","9th","10th","11th",
               "12th","13th","14th",
               "ground","basement","mezzanine","lower ground",
               "sub-basement","top")
  flat_flat <- c("premises",
                 "room?",
                 "unit?",
                 "suite?",
                 "office?",
                 "flats?",
                 "apartments?",
                 "storage pod?","store pods?",
                 "shops?",
                 "shop and rear yard",
                 "basement?")
  flat_end1 <- c("being","floors","floors being",
                "floor being","floors being","of")
  
  flat_end2 <- paste0("floor ",flat_flat)
  flat_end2 <- c(flat_end2, paste0(flat_end2,c(" being")))
  
  flat_end <- c(flat_end1, flat_end2)

  flat_start <- regex_collapse(flat_start)
  flat_start <- paste0("((",flat_start,")?)")
  numbers <- regex_collapse(numbers)
  flat_join <- regex_collapse(flat_join)
  flat_end <- regex_collapse(flat_end)
  
  rx_all <- paste0(flat_start," ",flat_join," ",numbers," ",flat_end)
  
  x <- stringi::stri_replace_all_regex(str = x,
                                       pattern =  rx_all,
                                       replacement = "@FTS",
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  
  # the flat fifth and sixth floors
  rx_all <- paste0(flat_start," ",numbers," ",flat_join," ",numbers," ",flat_end)
  
  x <- stringi::stri_replace_all_regex(str = x,
                                       pattern =  rx_all,
                                       replacement = "@FTS",
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  
  
  # the first floor flat being
  rx_all <- paste0(flat_start," ",numbers," ",flat_end)
  x <- stringi::stri_replace_all_regex(str = x,
                                       pattern =  rx_all,
                                       replacement = "@FTS",
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  
  #the first and second floor flats being
  rx_all <- paste0("^(",flat_start," )?",numbers," ",flat_join," ",numbers," ",flat_end)
  x <- stringi::stri_replace_all_regex(str = x,
                                       pattern =  rx_all,
                                       replacement = "@FTS",
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  
  # ground floor premises
  flat_flat <- regex_collapse(flat_flat)
  flat_end1 <- regex_collapse(flat_end1)
  rx_all <- paste0("^(the )?",numbers," floor ",flat_flat,"( ",flat_end1,")?")
  x <- stringi::stri_replace_all_regex(str = x,
                                       pattern =  rx_all,
                                       replacement = "@FTS",
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))

  # the ground floor of
  rx_all <- paste0("^(the )?",numbers," floor( level| flats?)?,?( \\b(of|being|offices?|hallways?|garages?)\\b)?")
  x <- stringi::stri_replace_all_regex(str = x,
                                       pattern =  rx_all,
                                       replacement = "@FTS",
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  
  # The flat is first floor
  rx_all <- paste0("^(the )?",flat_flat," is ",numbers," floor")
  x <- stringi::stri_replace_all_regex(str = x,
                                       pattern =  rx_all,
                                       replacement = "@FTS",
                                       opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  
  
  
  return(x)
}


