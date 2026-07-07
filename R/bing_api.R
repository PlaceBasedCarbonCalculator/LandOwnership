# Functions for geocoding with the Bing Maps REST services.
# Two routes are supported:
#  * The Spatial Data Services batch dataflow (bing_geocode, ..._status,
#    ..._download) which geocodes an uploaded CSV, and
#  * The Locations API (bing_geocode_single etc.) which geocodes one
#    address per request; bing_geocode_batch() loops this over a data frame.
# API keys are read from the environment variables Bing_master_key (data
# source management) and Bing_query_key (geocoding queries).

# Delete a data source previously uploaded to the Bing Spatial Data Services
bing_delete_data <- function(dataSourceName, accessId, key = Sys.getenv("Bing_master_key")){

  url <- paste0("http://spatial.virtualearth.net/REST/v1/data/",
               accessId,
               "/",
               dataSourceName,
               "?",
               "key=",
               key)
  # Deletion requires an HTTP DELETE request (a GET just queries the source)
  res <- httr::DELETE(url)
  content <- httr::content(res, type = "text")
  return(content)
}

# List the names of data sources uploaded to the Bing Spatial Data Services
bing_get_data_sources <- function(key = Sys.getenv("Bing_master_key")){
  url <- paste0("http://spatial.virtualearth.net/REST/v1/data?key=",
                key)
  res <- httr::GET(url)
  content <- httr::content(res, type = "text")
  content <- xml2::read_xml(content)
  content <- xml2::as_list(content)
  return(content$service$workspace$title)
}

# Upload a CSV to the Bing Spatial Data Services as a new/updated data
# source. If dataSourceName is NULL it is derived from the file name.
bing_upload_data <- function(path,
                             dataSourceName = NULL, 
                             input = "csv",
                             loadOperation = "complete", 
                             master_key = Sys.getenv("Bing_master_key")){
  # if(is.null(accessId)){
  #   accessId <- paste(sample(c(letters, LETTERS, 0:9), 20, TRUE), collapse = "")
  #   message("No accessId provided, So I created one: ",accessId)
  # }
  
  if(is.null(dataSourceName)){
    dataSourceName <- strsplit(path,"/", fixed = TRUE)
    dataSourceName <- dataSourceName[[1]]
    dataSourceName <- dataSourceName[length(dataSourceName)]
    dataSourceName <- strsplit(dataSourceName,".", fixed = TRUE) 
    dataSourceName <- dataSourceName[[1]]
    dataSourceName <- dataSourceName[1]
    message("No dataSourceName provided, So I created one: ",dataSourceName)
  }
  
  url <- paste0("http://spatial.virtualearth.net/REST/v1/Dataflows/LoadDataSource?dataSourceName=",
                dataSourceName,
                "&loadOperation=",
                loadOperation,
                "&input=",
                input,
                "&key=",
                master_key)
  res <- httr::POST(url, body = httr::upload_file(path))
  content <- httr::content(res, type = "text")
  content <- jsonlite::fromJSON(content)
  if(content$statusCode != 201){
    warning("Failed")
  } else {
    message("Success")
  }
  return(content)
}

# Submit a CSV of addresses as a batch geocode dataflow job.
# Returns the job info (including $id) on success, used by
# bing_geocode_status() and bing_geocode_download().
bing_geocode <- function(path, query_key = Sys.getenv("Bing_query_key")){
  
  checkmate::assert_file_exists(path)
  
  url <- paste0("http://spatial.virtualearth.net/REST/v1/Dataflows/Geocode?input=csv&output=json&key=",
  query_key)
  
  res <- httr::POST(url, body = httr::upload_file(path))
  content <- httr::content(res, type = "text", encoding = "UTF-8")
  content <- jsonlite::fromJSON(content)
  if(content$statusCode != 201){
    warning("Failed")
  } else {
    message("Success")
    jobID <- content$resourceSets$resources[[1]]
    return(jobID)
  }
  return(content)
  
}


# Check the status of a batch geocode job submitted with bing_geocode()
bing_geocode_status <- function(jobID, query_key = Sys.getenv("Bing_query_key")){
  
  url <- paste0("http://spatial.virtualearth.net/REST/v1/Dataflows/Geocode/",
                jobID,
                "?output=json&key=",
                query_key)
  
  res <- httr::GET(url)
  content <- httr::content(res, type = "text", encoding = "UTF-8")
  content <- jsonlite::fromJSON(content)
  if(content$statusCode != 200){
    warning("Failed")
  } else {
    message("Success")
    return(content$resourceSets)
  }
  
  return(content)
}



# Download the successful results of a completed batch geocode job as a
# data frame
bing_geocode_download <- function(jobID, query_key = Sys.getenv("Bing_query_key")){
  
  url <- paste0("https://spatial.virtualearth.net/REST/v1/dataflows/Geocode/",
                jobID,
                "/output/succeeded?key=",
                query_key)
  
  res <- httr::GET(url)
  content <- httr::content(res, type = "text", encoding = "UTF-8")
  # content <- substr(content, 34, nchar(content))
  # foo = read.table(text = content, sep = ",")
  
  content <- strsplit(content,"\n", fixed = TRUE)
  content <- content[[1]]
  content <- content[seq(2,length(content))]
  #foo <- substr(content, nchar(content), nchar(content))
  #content <- paste0(content,"\n")
  
  dir.create(file.path(tempdir(),"bing"))
  writeLines(content, file.path(tempdir(),"bing/geocodes.csv"))
  tab <- read.csv(file.path(tempdir(),"bing/geocodes.csv"))
  unlink(file.path(tempdir(),"bing"), recursive = TRUE)
  
  # tab <- read.table(textConnection(content), sep = ",", fileEncoding = "UTF-8")
  # tab <- read.table(textConnection(content[19]), sep = ",", )
  
  
  names(tab) <-  c("Id","AddressLine","AdminDistrict","PostalCode",
  "CountryRegion","Latitude","Longitude","EntityType",
  "MatchCodes","Confidence","SouthLatitude", "WestLongitude",
  "NorthLatitude", "EastLongitude", "StatusCode","FaultReason",
  "TraceId")
  
  return(tab)
}


# Convert a geocode results data frame to an sf points object (WGS84)
bing_to_sf <- function(tab){
  tab <- sf::st_as_sf(tab, coords = c("Longitude","Latitude"), crs = 4326)
  tab$TraceId <- NULL
  return(tab)
}


# Build a URL from a base and a named list of query parameters (NULL
# entries are dropped; values are URL-encoded)
build_url <- function(routerUrl, query) {
  secs <- unlist(query, use.names = TRUE)
  secs <- sapply(secs, utils::URLencode, reserved = TRUE)
  secs <- paste0(names(secs), "=", secs)
  secs <- paste(secs, collapse = "&")
  secs <- paste0(routerUrl, "?", secs)
  secs
}


# Geocode a single address with the Bing Maps Locations API.
# Returns a data frame of candidate matches (one row per result) with
# coordinates, confidence, entityType and matchCodes, or a single row of
# NAs if the request failed.
bing_geocode_single <- function(countryRegion = "GB",
                               adminDistrict = NULL,
                               postalCode = NULL, 
                               locality = NULL, 
                               addressLine = NULL,
                               includeNeighborhood = 0, 
                               include = "ciso2", 
                               maxResults = 5,
                               cultureCode = "en-GB",
                               key = Sys.getenv("Bing_query_key")){
  

  url <- "http://dev.virtualearth.net/REST/v1/Locations"

  query <- list(
    countryRegion = countryRegion,
    adminDistrict = adminDistrict,
    postalCode = postalCode,
    locality = locality,
    addressLine = addressLine,
    includeNeighborhood = includeNeighborhood,
    include = include,
    maxResults = maxResults,
    key = key,
    c = cultureCode
  )
  
  
  
  url <- build_url(url, query)
  text <- curl::curl_fetch_memory(url)
  text <- rawToChar(text$content)
  
  asjson <- try(RcppSimdJson::fparse(text),
                silent = TRUE
  )
  
  if("try-error" %in% class(asjson)){
    message("Geocode failed: json parse failed")
    return(data.frame(addressLine = NA,
                      adminDistrict = NA,
                      adminDistrict2 = NA,
                      countryRegion = NA,
                      formattedAddress = NA,
                      locality = NA,
                      postalCode = NA,
                      countryRegionIso2 = NA ,
                      latitude = NA,
                      longitude = NA,
                      confidence = NA,
                      entityType = NA,
                      matchCodes = NA))
  }
  
  
  if(asjson$statusCode != 200){
    message("Geocode failed: ", asjson$statusDescription)
    return(data.frame(addressLine = NA,
                      adminDistrict = NA,
                      adminDistrict2 = NA,
                      countryRegion = NA,
                      formattedAddress = NA,
                      locality = NA,
                      postalCode = NA,
                      countryRegionIso2 = NA ,
                      latitude = NA,
                      longitude = NA,
                      confidence = NA,
                      entityType = NA,
                      matchCodes = NA))
    
  }
  
  asjson <- asjson$resourceSets
  
  if(length(asjson$resources) > 1){
    stop("Muliple resultss for ",addressLine)
  }
  
  res <- asjson$resources[[1]]
  
  if(is.null(res)){
    message("Geocode failed: no result returned")
    return(data.frame(addressLine = NA,
                      adminDistrict = NA,
                      adminDistrict2 = NA,
                      countryRegion = NA,
                      formattedAddress = NA,
                      locality = NA,
                      postalCode = NA,
                      countryRegionIso2 = NA ,
                      latitude = NA,
                      longitude = NA,
                      confidence = NA,
                      entityType = NA,
                      matchCodes = NA))
  }
  
  if(length(res$address) > 1){
    res_address <- lapply(res$address, as.data.frame)
    res_address <- dplyr::bind_rows(res_address)
  } else {
    res_address <- res$address[[1]]
    res_address <- as.data.frame(res_address)
  }
  
  if(length(res$point) > 1){
    res_point <- lapply(res$point, as.data.frame)
    res_point <- dplyr::bind_rows(res_point)
  } else {
    res_point <- res$point[[1]]
    res_point <- as.data.frame(res_point)
  }
  
  if(length(res_point$coordinates) > 0){
    res_address$latitude <- res_point$coordinates[seq(1,length(res_point$coordinates), 2)]
    res_address$longitude <- res_point$coordinates[seq(2,length(res_point$coordinates), 2)]
  } else {
    res_address$latitude <- NA
    res_address$longitude <- NA
  }
  
  res_address$confidence <- res$confidence
  res_address$entityType <- res$entityType
  res_address$matchCodes <- res$matchCodes
  
  return(res_address)
  
  
}

# Error-tolerant wrapper around bing_geocode_single(); returns NULL on
# error instead of stopping, so a batch run is not aborted by one failure
bing_geocode_single_try <- function(countryRegion = "GB",
                                    adminDistrict = NULL,
                                    postalCode = NULL, 
                                    locality = NULL, 
                                    addressLine = NULL,
                                    includeNeighborhood = 0, 
                                    include = "ciso2", 
                                    maxResults = 5,
                                    cultureCode = "en-GB",
                                    key = Sys.getenv("Bing_query_key")){
  
  r <- try(bing_geocode_single(countryRegion = countryRegion,
                                 adminDistrict = adminDistrict,
                                 postalCode = postalCode, 
                                 locality = locality, 
                                 addressLine = addressLine,
                                 includeNeighborhood = includeNeighborhood, 
                                 include = include, 
                                 maxResults = maxResults,
                                 cultureCode = cultureCode,
                                 key = key), silent = TRUE)
  
  if("try-error" %in% class(r)){
    message("\nFailed on ",addressLine)
    return(NULL)
  } else {
    return(r)
  }
  
  
}


# Geocode a data frame of addresses one row at a time with a progress bar.
# `dat` must contain at least 4 of the columns countryRegion, adminDistrict,
# postalCode, locality, addressLine, includeNeighborhood, include,
# maxResults, key. Returns dat joined to the geocode results.
bing_geocode_batch <- function(dat){
  
  vars <- c("countryRegion","adminDistrict","postalCode","locality",
            "addressLine","includeNeighborhood","include","maxResults","key")
  vars <- names(dat)[names(dat) %in% vars]
  
  if(length(vars) < 4){
    stop(" not enough valid columns")
  }
  dat$matchID <- as.character(seq_len(nrow(dat)))
  
  # r <- purrr::pmap_dfr(dat[,vars], bing_geocode_single)
  r <- map_df_progress(dat[,vars], bing_geocode_single_try, .id = "matchID")
  
  r2 <- dplyr::left_join(dat, r, by = "matchID")
  
  return(r2)
  
}

# purrr::pmap_dfr() with a progress bar
map_df_progress <- function(.x, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = nrow(.x), force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::pmap_dfr(.x, f, ..., .id = .id)
}


# foo = bing_geocode_batch(dat[1:15,])
# 
# 
# dat = read.csv("data/for_geocoding/UK_freehold_pc_single_short_batch_1.csv", skip = 1)
# names(dat) = c(c("id","addressLine",
#                  "adminDistrict","postalCode",
#                  "countryRegion","Latitude",
#                  "Longitude","EntityType",
#                  "MatchCodes","Confidence",
#                  "BoundingBox.SouthLatitude","BoundingBox.WestLongitude",
#                  "BoundingBox.NorthLatitude","BoundingBox.EastLongitude",
#                  "StatusCode","FaultReason",                           
#                  "TraceId"))
# dat <- dat[,1:5]
# dat$countryRegion = "GB"
# dat$id <- as.character(dat$id)
