# Experimental functions for the Bing Maps Routes API, including parsing
# the traffic warnings on a route. Not part of the geocoding pipeline.
# Requires build_url() from R/bing_api.R, so source that file first.

# Route between two "lat,lng" waypoints and return the route path split
# into segments with traffic severity attached
bing_route <- function(from = "53.736968,-1.728393",
                       to = "53.749414,-1.635435", 
                       travelMode = "Driving",
                       dateTime = NULL,
                       avoid = NULL,
                       distanceBeforeFirstTurn = NULL,
                       heading = NULL,
                       optimize = "time",
                       optimizeWaypoints = FALSE,
                       
                       key = Sys.getenv("Bing_query_key")) {
  
  url <- "http://dev.virtualearth.net/REST/v1/Routes"
  
  query <- list(
    wayPoint.1 = from,
    wayPoint.2 = to,
    avoid = avoid,
    heading = heading,
    distanceBeforeFirstTurn = distanceBeforeFirstTurn,
    optimize = optimize,
    optimizeWaypoints = optimizeWaypoints,
    routeAttributes = "routePath,routepathannotations",
    dateTime = dateTime, # Required for transit
    key = key
  )
  
  # Example of the full query the Bing Maps website itself makes:
  # https://dev.virtualearth.net/REST/v1/Routes/alternate?key=<KEY>&o=json
  #   &ur=gb&c=en-GB&wp.0=<lat,lng>&wp.1=<lat,lng>
  #   &ra=routepath,routepathannotations,routeproperties,transitStops
  #   &optmz=timeWithTraffic&maxSolns=4&rpo=Points

  url2 <- build_url(url, query)
  text <- curl::curl_fetch_memory(url2)
  text <- rawToChar(text$content)
  asjson <- try(RcppSimdJson::fparse(text),
                silent = TRUE
  )
  
  res <- asjson$resourceSets$resources[[1]]
  res_legs <- res$routeLegs
  res_path <- res$routePath
  res$routeLegs <- NULL
  res$routePath <- NULL
  res_legs <- res_legs[[1]]
  res_legs$actualEnd <- bing_parse_point(res_legs$actualEnd)
  res_legs$actualStart <- bing_parse_point(res_legs$actualStart)
  
  res_legs_itinerary <- res_legs$itineraryItems[[1]]
  res_legs_SubLegs <- res_legs$routeSubLegs
  
  res_path <- res_path[[1]]
  res_path <- res_path$line$coordinates
  res_path <- bing_parse_coordinates(res_path)
  
  res_legs_itinerary_details <- res_legs_itinerary$details
  res_legs_itinerary_details <- dplyr::bind_rows(res_legs_itinerary_details, .id = "row_id")
  
  # Warnings contain the start/end of congestion
  warn <- bing_parse_warnings(res_legs_itinerary$warnings)
  path_warn <- bing_match_traffic_to_legs(res_path, warn)
  # To view: tmap::qtm(path_warn, lines.col = "severity")
  return(path_warn)
}


gather_traffic <- function(){
  
  q <- osmdata::opq(bbox = 'Leeds UK')
  q <- osmdata::add_osm_feature(q ,key = 'highway')
  osm <- osmdata::osmdata_sf(q)
  
  
  
  value = c("motorway","trunk","primary",
            "secondary","tertiary",
            "motorway_link","trunk_link",
            "primary_link","secondary_link",
            "tertiary_link")
  
}



bing_annotate_path <- function(x){
  coords = x$line$coordinates
  annotations = x$annotations
  annotations$index2 <- c(annotations$index[seq(2,nrow(annotations))],nrow(coords) - 1)
  annotations$index = annotations$index + 1
  annotations$index2 = annotations$index2 + 1
  
  ls <- list()
  for(i in seq_len(nrow(annotations))){
    sub <- coords[seq(annotations$index[i], annotations$index2[i]),2:1]
    sub <- sf::st_sfc(list(sf::st_linestring(sub)), crs = 4326)
    df <- sf::st_as_sf(data.frame(traffic = annotations$traffic[i], geometry = sub))
    ls[[i]] <- df
  }
  
  ls <- dplyr::bind_rows(ls)
  return(ls)
}


# Convert a Bing [lat,lng] coordinate matrix into an sfc LINESTRING
bing_parse_coordinates <- function(x){
  coords = matrix(c(x[,2], x[,1]), ncol = 2)
  coords = sf::st_linestring(coords)
  coords = sf::st_sfc(list(coords), crs = 4326)
  return(coords)
}

# Match TrafficFlow warnings to the segments of the route path `l`,
# returning the path split into lines with a severity attribute
bing_match_traffic_to_legs <- function(l, warn){
  
  warn = warn[warn$warningType == "TrafficFlow",]
  
  p = sf::st_cast(l, "POINT")
  wf = sf::st_as_sf(warn, coords = c("from_lng","from_lat"), crs = 4326)
  wt = sf::st_as_sf(warn, coords = c("to_lng","to_lat"), crs = 4326)
 
  wf_near <- unlist(nngeo::st_nn(wf,p, progress = FALSE), use.names = FALSE)
  wt_near <- unlist(nngeo::st_nn(wt,p, progress = FALSE), use.names = FALSE)
  
  seqs <- list()
  for(i in seq_along(wf_near)){
    seqs[[i]] <- seq(wf_near[i],wt_near[i] )
  }
  names(seqs) <- paste0(seq(1, nrow(warn)),"_")
  seqs <- unlist(seqs)
  names(seqs) <- sapply(strsplit(names(seqs),"_"),`[[`,1)
  
  seqs <- data.frame(warn_id = names(seqs),
                     segment_id = unname(seqs))
  
  pdf <- sf::st_as_sf(p)
  pdf$segment_id <- seq(1, nrow(pdf))
  
  pdf <- dplyr::left_join(pdf, seqs, by = "segment_id")
  
  pdf$warn_id[is.na(pdf$warn_id)] <- 0
  
  pdf$grp <- with(rle(pdf$warn_id), rep(seq_along(values), lengths))
  
  # Need to duplicate start/end points
  chng <- pdf$grp[seq(1,nrow(pdf)-1)] != pdf$grp[seq(2,nrow(pdf))]
  pdf$chng_before <- c(chng, FALSE)
  pdf$chng_after <- c(FALSE, chng)
  
  pdf_before = pdf[pdf$chng_before & pdf$warn_id != 0,]
  pdf_after = pdf[pdf$chng_after & pdf$warn_id != 0,]
  
  pdf_before$warn_id = 0
  pdf_after$warn_id = 0
  
  pdf_before$grp = pdf_before$grp + 1
  pdf_after$grp = pdf_after$grp - 1
  
  pdf = rbind(pdf, pdf_after, pdf_before)
  pdf = pdf[order(pdf$segment_id),]
  
  plst <- dplyr::group_split(pdf, pdf$grp)
  l_warn <- lapply(plst, points_to_lines)
  l_warn <- dplyr::bind_rows(l_warn)
  
  warn$warn_id <- as.character(seq(1, nrow(warn)))
  warn = warn[,c("warn_id","severity")]
  l_warn = dplyr::left_join(l_warn, warn, by = "warn_id")
  l_warn
  
}

# qtm(l_warn, lines.col = "severity", lines.lwd = 3)
# p1 = st_as_sf(warn, coords = c("from_lng","from_lat"), crs = 4326)
# p2 = st_as_sf(warn, coords = c("to_lng","to_lat"), crs = 4326)
# qtm(l) + qtm(p1) + qtm(p2, dots.col = "red")


points_to_lines <- function(x){
  warn_id = x$warn_id[1]
  x = sf::st_coordinates(x)
  x = sf::st_sfc(sf::st_linestring(x), crs = 4326)
  x = sf::st_as_sf(x)
  x$warn_id = warn_id
  x
}

bing_parse_point <- function(x){
  coords = lapply(x, `[[`, "coordinates")
  coords = lapply(coords, function(y){sf::st_point(y[2:1])})
  coords = sf::st_as_sfc(coords, crs = 4326)
  return(coords)
}

bing_parse_warnings <- function(x){
  y = x[lengths(x)>1]
  y = dplyr::bind_rows(y)
  orign <- strsplit(y$origin,",")
  dest <- strsplit(y$to,",")
  y$from_lat <- as.numeric(sapply(orign, `[`, 1))
  y$from_lng <- as.numeric(sapply(orign, `[`, 2))
  y$to_lat <- as.numeric(sapply(dest, `[`, 1))
  y$to_lng <- as.numeric(sapply(dest, `[`, 2))
  y = y[names(y) %in% c("severity","text","warningType","startTime","endTime",  
                        "from_lat","from_lng","to_lat","to_lng")]
  if(!is.null(y$endTime)){
    y$endTime <- bing_parse_time(y$endTime)
    y$startTime <- bing_parse_time(y$startTime)
  }
  return(y)
}

bing_parse_time <- function(x){
  x[sapply(x, is.null)] <- NA
  x <- unlist(lapply(x, `[[`, 1),use.names = FALSE )
  x <- gsub("/Date(","",x, fixed = TRUE)
  x <- gsub(")/","",x, fixed = TRUE)
  x <- as.numeric(x)/1000
  x <- as.POSIXct(x, origin = "1970-01-01")
  x
}

