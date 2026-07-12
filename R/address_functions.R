# Functions for parsing addresses


# Some addresses have multiple numbers
# e.g. 
# 50 to 67 (inclusive) 
# 1 to 4
# 44, 46 and 48 
# 297-299
# Flats A-H,
# 30 and 30a
# "188 Seven Sisters Road, Roadway at Rear of 178 to 188A (even Nos ) Seven Sisters Road, London "
# "15, 16, 19, 23, & 26, Marshal Road  42, 52, 54, Milne Road, Broadstone (BH17 7ND)"
# 40, 40A, 44 and 44A Tudor Drive, Romford, - note this becomes 40A which is ok in this case but might not be

# Reduce an address containing a range of house numbers to just the first
# number, e.g. "50 to 67 Example Street" becomes "50 Example Street".
# `withbrakets` controls whether the odd/even/inclusive qualifiers to strip
# are expected in brackets, e.g. "(odd)", or as bare words, e.g. "odd".
# Superseded by split_numbers() which expands the full range instead.
clean_numbers <- function(x, withbrakets = TRUE){
  # Check for multiple numbers
  if(stringi::stri_count_regex(x, '\\d+') <= 1){
    return(x)
  }
  
  # Otherwise return the first number
  locs <- stringr::str_locate_all(x,"\\d+")[[1]]
  rem <- c(locs[1,2] + 1, locs[nrow(locs),2]) # Remove from the end of the first number to the end of the last number
  rem <- substr(x, rem[1], rem[2])
  y <- gsub(rem,"",x, fixed = TRUE)
  
  if(withbrakets){
    # Remove any other words
    y <- gsub("\\(inclusive\\)","",y, ignore.case = TRUE)
    y <- gsub("\\(inc\\)","",y, ignore.case = TRUE)
    y <- gsub("\\(all\\)","",y, ignore.case = TRUE)
    y <- gsub("\\(odd\\)","",y, ignore.case = TRUE)
    y <- gsub("\\(odd nos \\)","",y, ignore.case = TRUE)
    y <- gsub("\\(odd numbers\\)","",y, ignore.case = TRUE)
    y <- gsub("\\(even\\)","",y, ignore.case = TRUE)
    y <- gsub("\\(even nos \\)","",y, ignore.case = TRUE)
    y <- gsub("\\(even numbers\\)","",y, ignore.case = TRUE)
  } else {
    # Remove any other words
    y <- gsub("\\binclusive\\b","",y, ignore.case = TRUE)
    y <- gsub("\\binc\\b","",y, ignore.case = TRUE)
    y <- gsub("\\bodd nos\\b","",y, ignore.case = TRUE)
    y <- gsub("\\bodd numbers\\b","",y, ignore.case = TRUE)
    y <- gsub("\\bodd\\b","",y, ignore.case = TRUE)
    y <- gsub("\\beven nos\\b","",y, ignore.case = TRUE)
    y <- gsub("\\beven numbers\\b","",y, ignore.case = TRUE)
    y <- gsub("\\beven\\b","",y, ignore.case = TRUE)
  }
  
  
  
  
  return(y)
  
}



# Classify a fragment of an address string as one of:
# "empty", "number" (e.g. "42"), "number_letter" (e.g. "42A"),
# "modify" (an odd/even/inclusive marker), "link" (a range separator like
# "to" or "-"), "sep" (a list separator), "unit" (contains the word "unit"),
# "road" (longer text, assumed to be a road name) or "other".
# Used by split_numbers() to parse number ranges out of addresses.
class_string <- function(str){
  n_char <- nchar(str)
  if(n_char == 0){
    return("empty")
  }
  
  suppressWarnings(numb <- as.numeric(str))
  if(!is.na(numb)){
    return("number")
  }
  
  suppressWarnings(numb <- as.numeric(substr(str,1,nchar(str)-1)))
  if(!is.na(numb)){
    return("number_letter")
  }
  
  if(str %in% c("(INC)", "(ODD)","(EVN)")){
    return("modify")
  }
  
  
  if(str %in% c(" to ", "-"," To "," TO "," - ")){
    return("link")
  }
  
  if(str %in% c(" ", ",",", "," and ",", and "," and, ",", & "," & "," &, ","/")){
    return("sep")
  }
  
  ws <- trimws(str)
  if(nchar(ws) == 0){
    return("empty")
  }
  
  if(grepl("\\bunit\\b", str, ignore.case = TRUE)){
    return("unit")
  }
  
  if(n_char > 5){
    return("road")
  }
  
  return("other")
}



# Expand a table of house numbers (built by split_numbers) into the full
# vector of individual numbers. Rows flagged as `link` are ranges and are
# expanded, stepping by 1 ("(INC)") or 2 ("(ODD)"/"(EVN)").
parse_number_table <- function(spt_tab){
  if(nrow(spt_tab) == 0){
    return(character(0))
  }
  nmbs <- list()
  for(i in rev(seq_len(nrow(spt_tab)))){
    n <- spt_tab$number[i]
    if(spt_tab$link[i]){
      mv <- "(INC)"
      if(spt_tab$mod[i]){
        mv <- spt_tab$modval[i]
      }
      if(mv == "(INC)"){
        mvn = -1
      } else{
        mvn = -2
      }
      
      # Make sequences
      nn <- as.numeric(gsub('[a-zA-Z]',"",n))
      nnm1 <- as.numeric(gsub('[a-zA-Z]',"",spt_tab$number[i - 1]))
      
      n2 <- try(seq(nn, nnm1, by = mvn), silent = TRUE)
      if("try-error" %in% class(n2)){
        n2 <- seq(nn, nnm1, by = -mvn)
      }
      n2 <- as.character(n2)
      n2[1] <- n
      n2[length(n2)] <- spt_tab$number[i - 1]
      n <- n2
    }
    nmbs[[i]] <- as.character(n)
    suppressWarnings(rm(n, mvn, mv, n2))
  }
  nmbs <- unlist(nmbs)
  nmbs <- nmbs[!duplicated(nmbs)]
  nmbs <- nmbs[order(nmbs)]
  return(nmbs)
  
}


# Split an address containing multiple house numbers into one address per
# property, expanding ranges, e.g. "10-14 (even) Example Street" becomes
# c("10 Example Street", "12 Example Street", "14 Example Street").
# Handles multiple roads within one string, odd/even/inclusive qualifiers
# and letter suffixes (e.g. "103a and 103b").
# The postcode must be removed from x first.
# Returns x unchanged if there is nothing to split; may throw an error on
# unparseable strings, so prefer split_numbers_try() for bulk use.
split_numbers <- function(x){
  
  # x = "15, 16, 19, 23, & 26, Marshal Road  42, 52, 54, Milne Road, Broadstone"
  # x = Unit 3, 819 Bath Road, Brislington, Bristol 
  # x = 103a and 103b Archel Road, London
  # x = 60, 60a And, 60b Ormiston Grove, London
  # x = 54 and 54a Newgate Street, Bishop Auckland,
  # x = 298 and, 300 Rawlinson Street, Barrow-In-Furness
  # x = 4/4a, Channel View, Bexhill-On-Sea 
  
  numb_count <- stringi::stri_count_regex(x, '\\d+')
  
  # Check for multiple numbers
  if(numb_count <= 1){
    return(x)
  }
  
  if(numb_count == 2 & grepl("Unit",x, ignore.case = FALSE)){
    return(x)
  }
  
  #Spelling errors
  x <- gsub("numberes","numbers",x, ignore.case = TRUE)
  x <- gsub("nombers","numbers",x, ignore.case = TRUE)
   
  # Standardise odd/even/inc
  x <- gsub("inclusive \\(odd\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("inclusive \\(odds\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("inclusive \\(odd numbers\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("inclusive \\(odd numbers only\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("inclusive \\(even\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("inclusive \\(evens\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("inclusive \\(even numbers\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("inclusive \\(even numbers only\\)","(EVN)",x, ignore.case = TRUE)
  
  x <- gsub("\\(odd\\) inclusive","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odds\\) inclusive","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd numbers\\) inclusive","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd numbers only\\) inclusive","(ODD)",x, ignore.case = TRUE)
  
  x <- gsub("\\(even\\) inclusive","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(evens\\) inclusive","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even numbers\\) inclusive","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even numbers only\\) inclusive","(EVN)",x, ignore.case = TRUE)
  
  x <- gsub("odd \\(inclusive\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("odds \\(inclusive\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("odd numbers \\(inclusive\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("odd nos \\(inclusive\\)","(ODD)",x, ignore.case = TRUE)
  
  x <- gsub("even \\(inclusive\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("evens \\(inclusive\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("even numbers \\(inclusive\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("even nos \\(inclusive\\)","(EVN)",x, ignore.case = TRUE)
  
  x <- gsub("\\(inclusive\\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(consecutive\\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(both inclusive\\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(all inclusive\\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(Odd & Even\\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(odd and even, inclusive\\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(odd and even inclusive\\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(even and odd, inclusive\\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(even and odd inclusive\\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(inc\\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(inc \\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(incl\\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(incl \\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(all\\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(consec\\)","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\(Consecutive\\)","(INC)",x, ignore.case = TRUE)
  
  x <- gsub("\\(odd\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd \\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odds\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd only\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd nos \\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd nos\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd no\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd no \\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd no's\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd no's only\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd numbers\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd numbers-inc\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd and inclusive\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd numbers inclusive\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd number inclusive\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd numbers, inclusive\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd number, inclusive\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd inclusive\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd, inclusive\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(inclusive odd\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(inc odd\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd nos inclusive\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd numbers only\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(odd incl\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(all odd numbers\\)","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\(consec odd no's\\)","(ODD)",x, ignore.case = TRUE)
  
  x <- gsub("\\(even\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even \\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(evens\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\( evens \\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even only\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even nos \\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even nos\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even no\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even no \\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even no's\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even no's only\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even numbers\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even numbers-inc\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even inclusive\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even, inclusive\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(inclusive even\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(inc even\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even and inclusive\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even numbers inclusive\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even number inclusive\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even numbers, inclusive\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even number, inclusive\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even nos inclusive\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even numbers only\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(even incl\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(all even numbers\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(consec even No's\\)","(EVN)",x, ignore.case = TRUE)

  # Last Chance
  # These will catch any missed but a too broad
  x <- gsub("odd\\)","(ODD)",x, ignore.case = FALSE)
  x <- gsub("even\\)","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\(odd\\b","(ODD)",x, ignore.case = FALSE)
  x <- gsub("\\(even\\b","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\binclusiveodd numbers\\b","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\binclusiveeven numbers\\b","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\bodd numbers inclusive\\b","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\beven numbers inclusive\\b","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\bodd inclusive\\b","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\beven inclusive\\b","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\bodd and inclusive\\b","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\beven and inclusive\\b","(EVN)",x, ignore.case = TRUE)
  x <- gsub("\\bodd numbers\\b","(ODD)",x, ignore.case = TRUE)
  x <- gsub("\\beven numbers\\b","(EVN)",x, ignore.case = TRUE)
  
  x <- gsub("\\binclusive\\b","(INC)",x, ignore.case = TRUE)
  x <- gsub("\\bodd\\b ","(ODD)",x, ignore.case = FALSE)
  x <- gsub("\\beven\\b ","(EVN)",x, ignore.case = TRUE)
  
  x <- gsub(" nos ","",x, ignore.case = TRUE)
  x <- gsub("\\bthe site of\\b","",x, ignore.case = TRUE)
  x <- gsub("\\bparts of\\b","",x, ignore.case = TRUE)

  # Split and calssify the part of the string
  a <- stringr::str_split(x, "(\\d+|\\(INC\\)|\\(ODD\\)|\\(EVN\\))")[[1]] 
  b <- stringr::str_extract_all(x, "(\\d+|\\(INC\\)|\\(ODD\\)|\\(EVN\\))")[[1]]
  spt = c(a, b)[order(c(seq_along(a)*2 - 1, seq_along(b)*2))]
  names(spt) <- sapply(spt, class_string)
  spt = spt[!names(spt) %in% c("sep","empty")]

  if("unit" %in% names(spt)){
    warning("Multiple units: ",x)
    return(x)
  }
  
  if(names(spt[1]) == "modify"){
    # Opening modifier ignore
    spt <- spt[seq(2,length(spt))]
  }
  
  # the "38 to 56 Hither Bath Bridge (even), Brislington " case
  if("modify" %in% names(spt)){
    modpos <- seq_len(length(spt))[names(spt) == "modify"]
    for(i in seq_len(length(modpos))){
      if(names(spt)[modpos[i] - 1] == "road"){
        spt[seq(modpos[i] - 1, modpos[i])] <- spt[seq(modpos[i], modpos[i] - 1)]
        names(spt) <- sapply(spt, class_string)
        
        # check for muliple roads after
        aft = modpos[i] + 1
        if(aft <= length(spt)){
          if(names(spt)[aft] == "road"){
            spt[modpos[i]] <- paste0(spt[modpos[i]], spt[modpos[i] + 1])
            spt[modpos[i] + 1] <- ""
            names(spt) <- sapply(spt, class_string)
            spt = spt[!names(spt) %in% c("sep","empty")]
          }
        }
      }
    }
  }
  
  #Handel 1A and 2B
  numpos <- seq_len(length(spt))[names(spt) == "number"]
  letpos <- numpos + 1
  letcheck <- substr(spt[letpos],1,2)
  letcheck2 <- grepl('[A-Za-z][\\s\\,]',letcheck, perl = TRUE)
  if(any(letcheck2)){
    for(i in seq_len(length(numpos))){
      if(letcheck2[i]){
        spt[numpos[i]] <- paste0(spt[numpos[i]], substr(spt[letpos[i]],1,1))
        spt[letpos[i]] <- substr(spt[letpos[i]],2,nchar(spt[letpos[i]]))
      }
    }
    names(spt) <- sapply(spt, class_string)
    spt = spt[!names(spt) %in% c("sep","empty")]
  }
  
  if(names(spt)[1] == "road"){
    names(spt)[1] <- "house"
  }
  
  
  # split by road
  rds <- seq_len(length(spt))[names(spt) == "road"]
  
  if(length(rds) == 0){
    rds <- matrix(c(1, length(spt)), ncol = 2)
    no_road_flag <- TRUE
  } else {
    no_road_flag <- FALSE
    rds <- matrix(c(1,(rds + 1)[seq_len(length(rds) - 1)], rds), ncol = 2)
  }
  
  res <- list()
  for(i in seq_len(nrow(rds))){
    spt_sub <- spt[seq(rds[i,1],rds[i,2])]
    spt_tab <- names(spt_sub) %in% c("number","number_letter")
    spt_tab <- data.frame(position = seq_len(length(spt_sub))[spt_tab],
                          number = spt_sub[spt_tab])
    
    
    #spt_tab$number <- as.numeric(spt_tab$number)
    # Identify number to link
    linknos <- seq_len(length(spt_sub))[names(spt_sub) == "link"]
    linknos = linknos + 1
    spt_tab$link <- spt_tab$position %in% linknos
    # Identify number with mods
    modnos <- seq_len(length(spt_sub))[names(spt_sub) == "modify"]
    modnos = modnos - 1
    spt_tab$mod <- spt_tab$position %in% modnos
    spt_tab$modval <- ifelse(spt_tab$mod,spt_sub[spt_tab$position + 1],"")
    
    numb_vec = parse_number_table(spt_tab)
    if(no_road_flag){
      res_sub = as.character(numb_vec)
    } else {
      res_sub = paste0(numb_vec," ",spt_sub[length(spt_sub)])
    }
    
    res[[i]] = res_sub
    
  }
  res = unlist(res)
  
  # Final Clean
  res <- gsub(" and $","",res)
  
  return(res)
  
}


# Error-tolerant wrapper around split_numbers(); returns NULL (and prints a
# message) for strings that fail to parse instead of stopping.
split_numbers_try <- function(x){
  r <- try(split_numbers(x), silent = TRUE)
  if(any(class(r)=="try-error")){
    message("\n Failed on: ", x)
    return(NULL)
  } else {
    return(r)
  }
}


# Find which of the candidate phrases `perms` occur in the string `x`.
# Returns NA if none match, the single phrase if one matches, or the
# matching phrases collapsed with "|" if several distinct ones match
# (matches that are substrings of a longer match are dropped).
# If startonly = TRUE, phrases only count when at the start of x.
# Superseded by check_match2(), which is faster.
check_match <- function(x, perms, startonly = TRUE){

  if(startonly){
    perms = paste0("^",perms)
  }

  y = vapply(perms, grepl, TRUE, x = x, USE.NAMES = FALSE, ignore.case = TRUE)
  y = perms[y]
  ly = length(y)

  if(ly == 0){
    return(NA_character_)
  }
  if(ly == 1){
    if(startonly){
      y = substr(y,2,nchar(y))
    }
    return(y)
  }

  if(startonly){
    y = substr(y,2,nchar(y))
  }

  # check for short matches in long matches
  y = data.frame(y = y)
  y$nchar <- nchar(y$y)
  if(sum(y$nchar == max(y$nchar)) > 1){
    y <- paste0(y$y, collapse = "|")
    return(y)
  } else {
    y$sub <- vapply(y$y, grepl, TRUE, x = y$y[y$nchar == max(y$nchar)], USE.NAMES = FALSE, ignore.case = TRUE)
    y <- y$y[!y$sub | y$nchar == max(y$nchar)]
  }
  
  if(length(y) > 1){
    y <- paste0(y, collapse = "|")
  }
  y
}


# Faster version of check_match() using stringi. Same return behaviour:
# NA if no phrase in `perms` matches x, the phrase if exactly one matches,
# or the distinct matches collapsed with "|".
check_match2 <- function(x, perms, startonly = TRUE){
  
  if(startonly){
    perms = paste0("^",perms)
    y = stringi::stri_detect_regex(x, perms, case_insensitive = TRUE)
  } else{
    y = stringi::stri_detect_fixed(x, perms, case_insensitive = TRUE)
  }
  
  
  y = perms[y]
  ly = length(y)
  
  if(ly == 0){
    return(NA_character_)
  }
  
  if(startonly){
    y = substr(y,2,nchar(y))
  }
  
  if(ly == 1){
    return(y)
  }
  
  # check for short matches in long matches
  y = data.frame(y = y)
  y$nchar <- nchar(y$y)
  if(sum(y$nchar == max(y$nchar)) > 1){
    y <- paste0(y$y, collapse = "|")
    return(y)
  } else {
    y$sub = stringi::stri_detect_fixed(y$y[y$nchar == max(y$nchar)], y$y, case_insensitive = TRUE)
    #y$sub <- vapply(y$y, grepl, TRUE, x = y$y[y$nchar == max(y$nchar)], USE.NAMES = FALSE, ignore.case = TRUE)
    y <- y$y[!y$sub | y$nchar == max(y$nchar)]
  }
  
  if(length(y) > 1){
    y <- paste0(y, collapse = "|")
  }
  y
}

# Remove every occurrence of each string in `y` from `x` (case-insensitive).
# Each string is removed three times over because removing one phrase can
# expose another occurrence of it (e.g. nested/overlapping boilerplate).
remove_strings <- function(x, y){
  for(i in seq_along(y)){
    for(j in 1:3){
      x <- stringi::stri_replace_all_fixed(x, y[i],"", 
                                           opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE))
    }
  }
  x
}


# Replace each whole-word occurrence of the strings in `y` with `rep`
# (case-insensitive) in x.
replace_strings <- function(x, y, rep){
  for(i in seq_along(y)){
    x <- stringi::stri_replace_all_regex(x, paste0("\\b",y[i],"\\b"),rep, 
                                           opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  }
  x
}
