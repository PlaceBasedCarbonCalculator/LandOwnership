# Geocoding via the Azure Maps Geocode API (the replacement for the retired
# Bing Maps Locations API).
# Docs: https://learn.microsoft.com/en-us/rest/api/maps/search/get-geocoding
#
# Requires build_url() from R/bing_api.R, so source that file first:
#   source("R/bing_api.R")
#
# Query string template for reference:
# ?&top={top}&query={query}&addressLine={addressLine}&countryRegion={countryRegion}
# &bbox={bbox}&view={view}&coordinates={coordinates}&adminDistrict={adminDistrict}
# &adminDistrict2={adminDistrict2}&adminDistrict3={adminDistrict3}
# &locality={locality}&postalCode={postalCode}

# Geocode a single address with Azure Maps.
# Either pass a free-text `query`, or structured address parts
# (addressLine, locality, adminDistrict, postalCode, etc.).
# Returns a data frame of candidate matches (one row per result), or a
# single row of NAs if the request or JSON parsing failed.
# (Renamed from bing_geocode_single so it no longer clashes with the
# function of the same name in R/bing_api.R.)
azure_geocode_single <- function(addressLine = NULL,
                                adminDistrict = NULL,
                                adminDistrict2 = NULL,
                                adminDistrict3 = NULL,
                                postalCode = NULL,
                                countryRegion = "GB",
                                locality = NULL,
                                query = NULL,
                                api_version = "2026-01-01",
                                top = 5,
                                bbox = NULL,
                                view = NULL,
                                coordinates = NULL,
                                key = Sys.getenv("AZURE_MAPS_PRIMARY_KEY")){

  # Empty result returned on any failure
  failed_result <- data.frame(addressLine = NA,
                              adminDistrict = NA,
                              adminDistrict2 = NA,
                              countryRegion = NA,
                              formattedAddress = NA,
                              locality = NA,
                              postalCode = NA,
                              countryRegionIso2 = NA,
                              latitude = NA,
                              longitude = NA,
                              confidence = NA,
                              entityType = NA,
                              matchCodes = NA)

  url <- "https://atlas.microsoft.com/geocode"

  query <- list(
    countryRegion = countryRegion,
    addressLine = addressLine,
    adminDistrict = adminDistrict,
    adminDistrict2 = adminDistrict2,
    adminDistrict3 = adminDistrict3,
    postalCode = postalCode,
    locality = locality,
    query = query,
    `api-version` = api_version,
    top = top,
    bbox = bbox,
    view = view,
    coordinates = coordinates,
    `subscription-key` = key
  )

  url <- build_url(url, query)
  text <- curl::curl_fetch_memory(url)
  text <- rawToChar(text$content)

  asjson <- try(RcppSimdJson::fparse(text),
                silent = TRUE
  )

  if("try-error" %in% class(asjson)){
    message("Geocode failed: json parse failed")
    return(failed_result)
  }

  # The Azure Maps Geocode API returns a GeoJSON FeatureCollection,
  # not the Bing-style resourceSets structure.
  if(!is.null(asjson$error)){
    message("Geocode failed: ", asjson$error$message)
    return(failed_result)
  }

  feats <- asjson$features

  if(is.null(feats) || length(feats) == 0){
    message("Geocode failed: no result returned")
    return(failed_result)
  }

  

  if(is.data.frame(feats)){
    # fparse simplified the features into a data frame whose geometry/bbox/
    # properties columns are still list-columns (one list per row) - convert
    # each row back into a feature list so parse_feature() can walk it
    feats <- lapply(seq_len(nrow(feats)), function(i){
      list(
        type = feats$type[i],
        geometry = feats$geometry[[i]],
        bbox = feats$bbox[[i]],
        properties = feats$properties[[i]]
      )
    })
  }

  res_address <- lapply(feats, parse_feature)
  res_address <- dplyr::bind_rows(res_address)

  return(res_address)
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


parse_feature <- function(ft){
    addr <- ft$properties$address
    # adminDistricts is a data frame when every element only has a
    # shortName (fparse simplifies uniform arrays), otherwise a list
    ad <- addr$adminDistricts
    adminDistricts <- if(is.null(ad)){
      character(0)
    } else if(is.data.frame(ad)){
      ad$shortName
    } else {
      vapply(ad, function(x) x$shortName, character(1))
    }
    res <- data.frame(
      addressLine = if(is.null(addr$addressLine)) NA else addr$addressLine,
      adminDistrict = if(length(adminDistricts) < 1) NA else adminDistricts[1],
      adminDistrict2 = if(length(adminDistricts) < 2) NA else adminDistricts[2],
      countryRegion = if(is.null(addr$countryRegion$name)) NA else addr$countryRegion$name,
      formattedAddress = if(is.null(addr$formattedAddress)) NA else addr$formattedAddress,
      locality = if(is.null(addr$locality)) NA else addr$locality,
      postalCode = if(is.null(addr$postalCode)) NA else addr$postalCode,
      countryRegionIso2 = if(is.null(addr$countryRegion$ISO)) NA else addr$countryRegion$ISO,
      # GeoJSON coordinates are [longitude, latitude]
      latitude = ft$geometry$coordinates[2],
      longitude = ft$geometry$coordinates[1],
      confidence = if(is.null(ft$properties$confidence)) NA else ft$properties$confidence,
      entityType = if(is.null(ft$properties$type)) NA else ft$properties$type,
      matchCodes = if(is.null(ft$properties$matchCodes)) NA else paste(unlist(ft$properties$matchCodes), collapse = ",")
    )
    res
  }
