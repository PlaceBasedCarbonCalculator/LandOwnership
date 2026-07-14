# Stage 6b: infill addresses for UPRNs that have no EPC / Price Paid
# address, using three newly-added open datasets:
#
#   1. OS Open Linked Identifiers BLPU-UPRN <-> Street-USRN (lids-*.zip):
#      42M UPRN->USRN pairs. The USRN is a unique street id, so if we know
#      the street name for one UPRN on a street we can infer it for every
#      other UPRN on the same street.
#   2. OS Open USRN (osopenusrn_*.gpkg): street centreline geometry per
#      USRN. No street names in this product - names come from the EPC /
#      Price Paid addresses of UPRNs linked to the USRN, or from OSM.
#      The geometry is used (a) as a sanity check on house-number guesses
#      and (b) to provide street-centroid fallback locations for
#      "land at X Road"-style titles with no house number.
#   3. OSM (united-kingdom-latest.gpkg, multipolygons layer only): ~3.1M
#      buildings carry addr:housenumber (+street/postcode/city) in the
#      other_tags hstore. A UPRN point inside such a building inherits its
#      address.
#
# Everything inferred here is FLAGGED: address_source says where the street
# came from, number_source says where the house number came from, and
# number_guessed marks gap-interpolated numbers ("we have 22 and 26, this
# unknown UPRN sits between them on the same side, call it 24"). UK
# numbering is erratic - corner plots, infill developments, skipped
# numbers - so guesses are conservative (single-gap, same parity, same
# street side, both neighbours nearby and agreeing on postcode) and are
# never treated as better than "guess" quality by the matcher.

# ---------------------------------------------------------------------------
# Loaders
# ---------------------------------------------------------------------------

# OS Open Linked Identifiers: UPRN <-> USRN pairs. 5GB CSV inside the zip;
# only the two id columns are read (~700MB in memory).
load_uprn_usrn_lookup <- function(zip_path) {
  tmp <- tempfile("lids")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  fls <- utils::unzip(zip_path, list = TRUE)$Name
  csvf <- fls[grepl("\\.csv$", fls)][1]
  utils::unzip(zip_path, files = csvf, exdir = tmp)

  n <- pipeline_sample_n()
  dt <- data.table::fread(
    file.path(tmp, csvf),
    select = c("IDENTIFIER_1", "IDENTIFIER_2"),
    integer64 = "numeric",
    nrows = if (is.na(n)) Inf else n,
    showProgress = FALSE
  )
  data.table::setnames(dt, c("UPRN", "USRN"))
  dt <- unique(dt)
  as.data.frame(dt)
}

# OS Open USRN street centrelines (BNG). Mixed XY/XYZ geometry - Z dropped.
load_usrn_geometry <- function(zip_path) {
  fls <- utils::unzip(zip_path, list = TRUE)$Name
  gpkg <- fls[grepl("\\.gpkg$", fls)][1]
  vsi <- paste0("/vsizip/", gsub("\\\\", "/", zip_path), "/", gpkg)

  n <- pipeline_sample_n()
  if (is.na(n)) {
    x <- sf::st_read(vsi, layer = "openUSRN", quiet = TRUE)
  } else {
    x <- sf::st_read(vsi, query = paste0('SELECT * FROM "openUSRN" LIMIT ', n), quiet = TRUE)
  }
  x <- sf::st_zm(x)
  x$usrn <- as.numeric(x$usrn)
  x[, c("usrn", attr(x, "sf_column"))]
}

# Pull one "key"=>"value" out of an OSM other_tags hstore string.
extract_osm_tag <- function(tags, key) {
  stringr::str_match(tags, paste0('"', key, '"=>"([^"]*)"'))[, 2]
}

# Named OSM roads, read from the .osm.pbf (NOT the gpkg - the gpkg was
# produced by the sibling repo's read_osm_pbf_buildings() via
# osmextract::oe_read(layer = "multipolygons"), which only translates the
# requested layer; asking oe_read for "lines" here translates that layer
# once and appends it to the same cached gpkg). Used to name USRNs that
# have no EPC/Price-Paid address anywhere along them.
load_osm_road_names <- function(pbf_path) {
  street_types <- c(
    "motorway", "trunk", "primary", "secondary", "tertiary",
    "unclassified", "residential", "living_street", "service", "pedestrian"
  )
  # the geometry column must be selected explicitly - without it st_read
  # returns a plain data.frame, not sf
  q <- paste0(
    "SELECT osm_id, name, highway, geometry FROM lines WHERE name IS NOT NULL ",
    "AND highway IN (", paste0("'", street_types, "'", collapse = ","), ")"
  )
  n <- pipeline_sample_n()
  if (!is.na(n)) {
    q <- paste0(q, " LIMIT ", n)
  }
  x <- osmextract::oe_read(pbf_path, layer = "lines", query = q, quiet = TRUE)
  sf::st_transform(x, 27700)
}

# Name USRNs from the nearest named OSM road. Probe point is a point ON the
# USRN line; a name is only accepted when that point lies within `max_dist`
# metres of the road (a USRN and its OSM twin should practically overlap -
# 15m tolerates digitisation offset without grabbing the next street over).
# Single-probe-point is a known simplification: on long curved USRNs the
# midpoint can sit nearer a side road; acceptable because these names only
# ever produce "medium"/"street"-quality matches downstream.
name_usrns_from_osm <- function(usrn_geom, osm_roads, exclude_usrns = numeric(0),
                                max_dist = 15) {
  todo <- usrn_geom[!usrn_geom$usrn %in% exclude_usrns, ]
  if (nrow(todo) == 0 || is.null(osm_roads) || nrow(osm_roads) == 0) {
    return(data.frame(USRN = numeric(0), street = character(0)))
  }
  suppressWarnings(probe <- sf::st_point_on_surface(sf::st_geometry(todo)))
  probe <- sf::st_as_sf(data.frame(usrn = todo$usrn), geometry = probe)

  chunk_size <- 200000L
  chunks <- split(seq_len(nrow(probe)), ceiling(seq_len(nrow(probe)) / chunk_size))
  out <- lapply(seq_along(chunks), function(i) {
    p <- probe[chunks[[i]], ]
    nearest <- sf::st_nearest_feature(p, osm_roads)
    d <- as.numeric(sf::st_distance(p, osm_roads[nearest, ], by_element = TRUE))
    hit <- d <= max_dist
    message("  OSM road-name chunk ", i, "/", length(chunks), ": ", sum(hit), " named")
    data.frame(
      USRN = p$usrn[hit],
      street = normalise_name(osm_roads$name[nearest][hit]),
      stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(out)
  out[!is.na(out$street), ]
}

# District for arbitrary USRNs by point-in-polygon against
# data/la_bounds.geojson (331 LA polygons whose `name` field is already in
# the Land Registry's uppercase District style, e.g. "CHESHIRE EAST").
assign_usrn_districts <- function(usrn_geom, la_bounds_path) {
  if (nrow(usrn_geom) == 0) {
    return(data.frame(USRN = numeric(0), district = character(0)))
  }
  la <- sf::st_read(la_bounds_path, quiet = TRUE)
  la <- sf::st_transform(la[, "name"], 27700)
  la <- sf::st_make_valid(la)
  suppressWarnings(probe <- sf::st_point_on_surface(sf::st_geometry(usrn_geom)))
  probe <- sf::st_as_sf(data.frame(usrn = usrn_geom$usrn), geometry = probe)
  joined <- sf::st_join(probe, la, join = sf::st_within)
  joined <- sf::st_drop_geometry(joined)
  joined <- joined[!is.na(joined$name), ]
  joined <- joined[!duplicated(joined$usrn), ]
  data.frame(USRN = joined$usrn, district = normalise_name(joined$name))
}

# OSM buildings (multipolygons is the only layer in this extract) that have
# a tagged house number. Attribute filter is pushed down to SQLite, so only
# the ~3.1M matching polygons are read, not all 17.5M.
load_osm_building_addresses <- function(gpkg_path) {
  q <- paste0(
    "SELECT osm_id, osm_way_id, name, building, other_tags FROM multipolygons ",
    "WHERE other_tags LIKE '%addr:housenumber%'"
  )
  n <- pipeline_sample_n()
  if (!is.na(n)) {
    q <- paste0(q, " LIMIT ", n)
  }
  x <- sf::st_read(gpkg_path, query = q, quiet = TRUE)

  x$osm_housenumber <- extract_osm_tag(x$other_tags, "addr:housenumber")
  x$osm_street <- extract_osm_tag(x$other_tags, "addr:street")
  x$osm_postcode <- normalise_postcode(extract_osm_tag(x$other_tags, "addr:postcode"))
  x$osm_city <- extract_osm_tag(x$other_tags, "addr:city")
  x$osm_building_name <- ifelse(!is.na(x$name), x$name,
    extract_osm_tag(x$other_tags, "addr:housename")
  )
  x$other_tags <- NULL
  x
}

# National Statistics UPRN Lookup (NSUL): the authoritative UPRN ->
# (postcode, local authority) mapping, one regional CSV per England/Wales
# region inside the zip. Scotland's file is skipped - CCOD/OCOD only covers
# England & Wales, so Scottish rows would just cost ~1GB of memory. Only
# UPRN, PCDS and the LAD*CD column are read; the LAD column is matched by
# pattern because its name carries the boundary year (LAD25CD today).
load_nsul <- function(zip_path) {
  fls <- utils::unzip(zip_path, list = TRUE)$Name
  csvs <- fls[grepl("^Data/.*\\.csv$", fls)]
  csvs <- csvs[!grepl("_SC\\.csv$", csvs)]

  n <- pipeline_sample_n()
  if (!is.na(n)) {
    csvs <- csvs[1]
  }

  tmp <- tempfile("nsul")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  out <- vector("list", length(csvs))
  for (i in seq_along(csvs)) {
    utils::unzip(zip_path, files = csvs[i], exdir = tmp)
    f <- file.path(tmp, csvs[i])
    hdr <- names(data.table::fread(f, nrows = 0))
    lad_col <- grep("^LAD[0-9]+CD$", hdr, value = TRUE)[1]
    if (is.na(lad_col) || !all(c("UPRN", "PCDS") %in% hdr)) {
      stop("NSUL schema has changed in ", csvs[i], " - expected UPRN, PCDS and a LAD*CD column.")
    }
    dt <- data.table::fread(
      f,
      select = c("UPRN", "PCDS", lad_col),
      integer64 = "numeric",
      nrows = if (is.na(n)) Inf else n,
      showProgress = FALSE
    )
    data.table::setnames(dt, c("UPRN", "postcode", "lad_code"))
    file.remove(f) # keep peak temp-disk usage to one region at a time
    message("  NSUL ", basename(csvs[i]), ": ", nrow(dt), " rows")
    out[[i]] <- dt
  }
  out <- data.table::rbindlist(out)
  out[, postcode := normalise_postcode(postcode)]
  out <- unique(out, by = "UPRN")
  as.data.frame(out)
}

# The official LAD code -> name lookup shipped inside the NSUL zip
# (Documents/LAD ... names and codes ... .csv; first two columns are
# code, name).
load_nsul_lad_names <- function(zip_path) {
  fls <- utils::unzip(zip_path, list = TRUE)$Name
  lad_csv <- fls[grepl("^Documents/LAD .*names and codes.*\\.csv$", fls)][1]
  if (is.na(lad_csv)) {
    stop("No LAD names-and-codes csv found in ", zip_path)
  }
  tmp <- tempfile("lad")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  utils::unzip(zip_path, files = lad_csv, exdir = tmp)
  lad <- data.table::fread(file.path(tmp, lad_csv), select = 1:2)
  data.table::setnames(lad, c("lad_code", "lad_name"))
  as.data.frame(lad)
}

# LAD code -> district name AS THE LAND REGISTRY SPELLS IT. Primary source:
# majority vote of the LR's own District values across the LAD's postcodes
# (via NSUL postcode->LAD + the LR-derived postcode->district lookup) -
# that guarantees agreement with the CCOD/OCOD District column. LADs never
# seen in LR data fall back to the official ONS name (normalised), which
# matches LR's spelling for most districts.
build_lad_district_lookup <- function(nsul, postcode_district, lad_names) {
  pc_lad <- data.table::as.data.table(nsul)[, .(postcode, lad_code)]
  pc_lad <- unique(pc_lad)
  pc_lad <- merge(pc_lad, data.table::as.data.table(postcode_district),
    by = "postcode"
  )
  lr_vote <- pc_lad[!is.na(district), .N, by = .(lad_code, district)]
  data.table::setorder(lr_vote, lad_code, -N)
  lr_vote <- lr_vote[!duplicated(lad_code)]

  lookup <- data.table::as.data.table(lad_names)
  lookup[, district_official := normalise_name(lad_name)]
  lookup <- merge(lookup, lr_vote[, .(lad_code, district)], by = "lad_code", all.x = TRUE)
  lookup[, district := ifelse(is.na(district), district_official, district)]
  as.data.frame(lookup[, .(lad_code, district)])
}

# UPRN -> (postcode, district): the per-UPRN place facts used to enrich the
# infill, name streets and validate gap guesses.
build_uprn_places <- function(nsul, lad_district) {
  places <- data.table::as.data.table(nsul)
  places <- merge(places, data.table::as.data.table(lad_district),
    by = "lad_code", all.x = TRUE
  )
  as.data.frame(places[, .(UPRN, postcode, district)])
}

# ---------------------------------------------------------------------------
# Known addresses and derived street names
# ---------------------------------------------------------------------------

# One row per UPRN that already has a usable address (EPC domestic /
# non-domestic / Price Paid), with the house number and street name parsed
# out of the first address line. This is both a matching resource in its
# own right and the "training data" for the USRN street-name inference.
build_known_uprn_addresses <- function(uprn_epc_lr, house_price_lr_uprn) {
  dom <- uprn_epc_lr$domestic
  nondom <- uprn_epc_lr$nondomestic
  hp <- house_price_lr_uprn[!is.na(house_price_lr_uprn$uprn), ]

  grab <- function(df, uprn, addr, postcode, source) {
    out <- data.frame(
      UPRN = as.numeric(df[[uprn]]),
      addr = as.character(df[[addr]]),
      postcode = normalise_postcode(df[[postcode]]),
      LATITUDE = as.numeric(df$LATITUDE),
      LONGITUDE = as.numeric(df$LONGITUDE),
      address_source = source,
      stringsAsFactors = FALSE
    )
    if (all(c("X_COORDINATE", "Y_COORDINATE") %in% names(df))) {
      out$X_COORDINATE <- as.numeric(df$X_COORDINATE)
      out$Y_COORDINATE <- as.numeric(df$Y_COORDINATE)
    } else {
      out$X_COORDINATE <- NA_real_
      out$Y_COORDINATE <- NA_real_
    }
    out
  }

  known <- dplyr::bind_rows(
    grab(dom, "UPRN", "addr", "POSTCODE", "epc_domestic"),
    grab(nondom, "UPRN", "adr1", "postcode", "epc_nondomestic"),
    grab(hp, "uprn", "address1", "postcode", "price_paid")
  )
  known <- known[!is.na(known$UPRN) & !is.na(known$addr) & known$addr != "", ]
  # priority order is the bind order above; keep the first address per UPRN
  known <- known[!duplicated(known$UPRN), ]

  known$house_number <- extract_house_number(known$addr)
  known$street <- extract_street_name(known$addr)

  # fill missing BNG coords from lon/lat so the gap-guessing geometry (all
  # done in metres) can use every known point
  need_xy <- is.na(known$X_COORDINATE) & !is.na(known$LONGITUDE) & !is.na(known$LATITUDE)
  if (any(need_xy)) {
    pts <- sf::st_as_sf(known[need_xy, c("LONGITUDE", "LATITUDE")],
      coords = c("LONGITUDE", "LATITUDE"), crs = 4326
    )
    xy <- sf::st_coordinates(sf::st_transform(pts, 27700))
    known$X_COORDINATE[need_xy] <- xy[, 1]
    known$Y_COORDINATE[need_xy] <- xy[, 2]
  }
  known
}

# postcode -> District lookup built from the Land Registry's own rows, so
# district naming is consistent with the CCOD/OCOD `District` column by
# construction (no ONS-code mapping needed).
build_postcode_district_lookup <- function(...) {
  raws <- list(...)
  pc_dist <- lapply(raws, function(df) {
    df <- df[!is.na(df$Postcode) & !is.na(df$District), c("Postcode", "District")]
    df$postcode <- normalise_postcode(df$Postcode)
    df$district <- normalise_name(df$District)
    df[, c("postcode", "district")]
  })
  pc_dist <- dplyr::bind_rows(pc_dist)
  pc_dist <- pc_dist[!is.na(pc_dist$postcode) & !is.na(pc_dist$district), ]

  counts <- pc_dist |>
    dplyr::count(postcode, district) |>
    dplyr::arrange(postcode, dplyr::desc(n))
  counts[!duplicated(counts$postcode), c("postcode", "district")]
}

# Give each USRN a street name (and district). Two sources, EPC/PP first:
#   1. Majority vote of the parsed street names of the USRN's known UPRNs
#      (`street_agreement` = share agreeing with the winner; low agreement
#      means messy LIDS links or address parses, so the name isn't trusted).
#      These names follow Land Registry address conventions, so they win.
#   2. For USRNs with no EPC/PP presence at all: the nearest named OSM road
#      (street_confidence "osm_road") - pass usrn_geom + osm_roads.
# Districts come from the majority postcode via the LR-derived lookup where
# possible, else point-in-polygon against data/la_bounds.geojson.
build_usrn_street_names <- function(uprn_usrn, known_uprn_addresses, postcode_district,
                                    usrn_geom = NULL, osm_roads = NULL,
                                    la_bounds_path = NULL, uprn_places = NULL) {
  empty <- data.frame(
    USRN = numeric(0), street = character(0), district = character(0),
    street_n = integer(0), street_agreement = numeric(0),
    street_confidence = character(0)
  )

  known <- known_uprn_addresses[!is.na(known_uprn_addresses$street), ]
  dt <- data.table::as.data.table(
    dplyr::inner_join(
      known[, c("UPRN", "street", "postcode")],
      uprn_usrn,
      by = "UPRN"
    )
  )
  if (nrow(dt) > 0) {
    dt[, street_norm := normalise_name(street)]
    per_usrn <- dt[, {
      tab <- sort(table(street_norm), decreasing = TRUE)
      list(
        street = names(tab)[1],
        street_n = as.integer(tab[1]),
        street_agreement = as.numeric(tab[1]) / .N,
        postcode = names(sort(table(postcode), decreasing = TRUE))[1]
      )
    }, by = USRN]
    per_usrn <- merge(per_usrn, postcode_district, by = "postcode", all.x = TRUE)
    per_usrn[, postcode := NULL]
    per_usrn <- as.data.frame(per_usrn)
    per_usrn <- per_usrn[!is.na(per_usrn$street) & per_usrn$street_agreement >= 0.6, ]
    per_usrn$street_confidence <- ifelse(
      per_usrn$street_n >= 3 & per_usrn$street_agreement >= 0.8, "high",
      ifelse(per_usrn$street_n >= 2, "medium", "low")
    )
  } else {
    per_usrn <- empty
  }
  message(nrow(per_usrn), " USRNs named from EPC/Price-Paid addresses.")

  # OSM road names for streets with no EPC/PP presence at all
  if (!is.null(usrn_geom) && !is.null(osm_roads)) {
    osm_named <- name_usrns_from_osm(usrn_geom, osm_roads,
      exclude_usrns = per_usrn$USRN
    )
    if (nrow(osm_named) > 0) {
      osm_named$district <- NA_character_
      osm_named$street_n <- NA_integer_
      osm_named$street_agreement <- NA_real_
      osm_named$street_confidence <- "osm_road"
      per_usrn <- dplyr::bind_rows(per_usrn, osm_named)
    }
    message(nrow(osm_named), " further USRNs named from nearby named OSM roads.")
  }

  # district from NSUL: majority district of the street's UPRNs - covers
  # OSM-named streets (no postcode evidence) and streets whose postcodes
  # never appear in the LR data
  if (!is.null(uprn_places) && any(is.na(per_usrn$district))) {
    need <- per_usrn$USRN[is.na(per_usrn$district)]
    dtp <- data.table::as.data.table(uprn_usrn)[USRN %in% need]
    dtp <- merge(dtp,
      data.table::as.data.table(uprn_places)[!is.na(district), .(UPRN, district)],
      by = "UPRN"
    )
    if (nrow(dtp) > 0) {
      dtp <- dtp[, .N, by = .(USRN, district)]
      data.table::setorder(dtp, USRN, -N)
      dtp <- dtp[!duplicated(USRN)]
      di <- match(per_usrn$USRN, dtp$USRN)
      per_usrn$district <- ifelse(is.na(per_usrn$district), dtp$district[di], per_usrn$district)
    }
  }

  # last-resort district fallback by geometry for anything still unplaced
  if (!is.null(la_bounds_path) && !is.null(usrn_geom) && any(is.na(per_usrn$district))) {
    need <- per_usrn$USRN[is.na(per_usrn$district)]
    la_d <- assign_usrn_districts(usrn_geom[usrn_geom$usrn %in% need, ], la_bounds_path)
    di <- match(per_usrn$USRN, la_d$USRN)
    per_usrn$district <- ifelse(is.na(per_usrn$district), la_d$district[di], per_usrn$district)
  }
  per_usrn
}

# ---------------------------------------------------------------------------
# The infill itself
# ---------------------------------------------------------------------------

# Gap-guess house numbers for unknown UPRNs. For each street: take the
# known, numeric-numbered UPRNs, split by odd/even (UK convention: one
# parity per side), order everything along the street's principal axis, and
# wherever two consecutive known numbers differ by exactly 4 (one missing
# house) with exactly ONE unknown UPRN projected between them on the same
# side of the street, guess the midpoint number. Requirements:
#   - neighbours < 150m apart along the axis (genuinely adjacent),
#   - unknown within 12m of the known pair's side-offset (same side of the
#     street: opposite frontages are usually 15m+ apart and carry the other
#     parity, so a loose tolerance would guess even numbers for odd-side
#     houses),
#   - unknown within 75m of the USRN centreline (LIDS link and coordinate
#     agree),
#   - both neighbours in the same postcode (inherited by the guess).
guess_gap_numbers <- function(unknown_on_usrn, known_on_usrn, usrn_geom) {
  guesses <- list()

  # pre-filter to streets that can possibly yield a guess (an unknown UPRN
  # plus two known same-parity numbers exactly 4 apart) - this cuts the
  # per-street loop from every street in Britain to a small candidate set
  kn_dt <- data.table::as.data.table(known_on_usrn)
  kn_dt[, num := suppressWarnings(as.numeric(gsub("[A-Za-z]", "", house_number)))]
  kn_dt <- kn_dt[!is.na(num)]
  gap_usrns <- kn_dt[, {
    n <- sort(unique(num))
    list(has_gap = any(diff(n[n %% 2 == 0]) == 4) | any(diff(n[n %% 2 == 1]) == 4))
  }, by = USRN]
  gap_usrns <- gap_usrns$USRN[gap_usrns$has_gap]

  usrns <- intersect(unique(unknown_on_usrn$USRN), gap_usrns)
  if (length(usrns) == 0) {
    return(data.frame())
  }

  known_split <- split(known_on_usrn[known_on_usrn$USRN %in% usrns, ],
                       known_on_usrn$USRN[known_on_usrn$USRN %in% usrns])
  unknown_split <- split(unknown_on_usrn[unknown_on_usrn$USRN %in% usrns, ],
                         unknown_on_usrn$USRN[unknown_on_usrn$USRN %in% usrns])

  for (u in names(known_split)) {
    kn <- known_split[[u]]
    un <- unknown_split[[u]]
    if (is.null(un) || nrow(un) == 0 || nrow(kn) < 2) next

    kn$num <- suppressWarnings(as.numeric(gsub("[A-Za-z]", "", kn$house_number)))
    kn <- kn[!is.na(kn$num) & !is.na(kn$X_COORDINATE), ]
    if (nrow(kn) < 2) next

    # principal axis of all the street's points: t = along-street,
    # s = side-of-street offset
    xy <- rbind(
      as.matrix(kn[, c("X_COORDINATE", "Y_COORDINATE")]),
      as.matrix(un[, c("X_COORDINATE", "Y_COORDINATE")])
    )
    ctr <- colMeans(xy)
    xy_c <- sweep(xy, 2, ctr)
    ev <- eigen(stats::cov(xy_c), symmetric = TRUE)$vectors
    t_all <- xy_c %*% ev[, 1]
    s_all <- xy_c %*% ev[, 2]
    kn$t <- t_all[seq_len(nrow(kn))]
    kn$s <- s_all[seq_len(nrow(kn))]
    un$t <- t_all[nrow(kn) + seq_len(nrow(un))]
    un$s <- s_all[nrow(kn) + seq_len(nrow(un))]

    for (par in c(0, 1)) {
      knp <- kn[kn$num %% 2 == par, ]
      if (nrow(knp) < 2) next
      knp <- knp[order(knp$num), ]
      for (i in seq_len(nrow(knp) - 1)) {
        a <- knp[i, ]
        b <- knp[i + 1, ]
        if (b$num - a$num != 4) next # exactly one missing number
        if (abs(b$t - a$t) > 150) next # neighbours must be adjacent
        if (is.na(a$postcode) || is.na(b$postcode) || a$postcode != b$postcode) next

        t_lo <- min(a$t, b$t)
        t_hi <- max(a$t, b$t)
        side_ref <- mean(c(a$s, b$s))
        cand <- un[un$t > t_lo & un$t < t_hi & abs(un$s - side_ref) < 12, ]
        if (nrow(cand) != 1) next # ambiguous unless exactly one candidate

        guesses[[length(guesses) + 1]] <- data.frame(
          UPRN = cand$UPRN,
          USRN = as.numeric(u),
          house_number = as.character(a$num + 2),
          postcode = a$postcode,
          guess_between = paste0(a$num, "-", b$num),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(guesses) == 0) {
    return(data.frame())
  }
  guesses <- dplyr::bind_rows(guesses)
  # a UPRN guessed twice (overlapping windows) is ambiguous - drop it
  guesses <- guesses[!guesses$UPRN %in% guesses$UPRN[duplicated(guesses$UPRN)], ]

  # sanity: guessed UPRN must actually sit near its USRN centreline
  if (nrow(guesses) > 0 && !is.null(usrn_geom) && nrow(usrn_geom) > 0) {
    un_idx <- match(guesses$UPRN, unknown_on_usrn$UPRN)
    pts <- sf::st_as_sf(
      unknown_on_usrn[un_idx, c("X_COORDINATE", "Y_COORDINATE")],
      coords = c("X_COORDINATE", "Y_COORDINATE"), crs = 27700
    )
    geom_idx <- match(guesses$USRN, usrn_geom$usrn)
    has_geom <- !is.na(geom_idx)
    if (any(has_geom)) {
      d <- rep(NA_real_, nrow(guesses))
      d[has_geom] <- as.numeric(sf::st_distance(
        pts[has_geom, ], usrn_geom[geom_idx[has_geom], ],
        by_element = TRUE
      ))
      guesses <- guesses[is.na(d) | d < 75, ]
    }
  }
  guesses
}

# Fold the NSUL per-UPRN facts (uprn_places: postcode + LR-style district)
# into the assembled infill table:
#   - a number guess whose neighbour-inherited postcode disagrees with the
#     UPRN's true NSUL postcode is WITHDRAWN (the "sits between" assumption
#     is evidently wrong) - the street name is kept;
#   - the NSUL postcode then wins over OSM tags / neighbour inheritance
#     (postcode_source records which one survived);
#   - the NSUL district wins over the postcode/USRN/OSM-city fallbacks.
apply_uprn_places <- function(infill, uprn_places) {
  if (is.null(uprn_places) || nrow(infill) == 0) {
    return(infill)
  }
  pi <- match(infill$UPRN, uprn_places$UPRN)
  nsul_pc <- uprn_places$postcode[pi]
  nsul_district <- uprn_places$district[pi]

  conflict <- infill$number_guessed &
    !is.na(nsul_pc) & !is.na(infill$postcode) &
    infill$postcode != nsul_pc
  if (any(conflict)) {
    message(
      sum(conflict), " gap-guessed house numbers withdrawn: the neighbours' ",
      "postcode disagrees with the UPRN's NSUL postcode."
    )
    infill$house_number[conflict] <- NA_character_
    infill$number_source[conflict] <- NA_character_
    infill$number_guessed[conflict] <- FALSE
    infill$guess_between[conflict] <- NA_character_
  }

  use_nsul <- !is.na(nsul_pc)
  infill$postcode[use_nsul] <- nsul_pc[use_nsul]
  infill$postcode_source[use_nsul] <- "nsul"
  infill$district <- ifelse(!is.na(nsul_district), nsul_district, infill$district)
  infill
}

# Main infill builder: one row per previously address-less UPRN that gained
# any information, with full provenance flags.
build_uprn_infill <- function(uprn_epc_lr, uprn_usrn, usrn_street_names,
                              known_uprn_addresses, usrn_geom, osm_addresses,
                              postcode_district, uprn_places = NULL) {
  unknown <- uprn_epc_lr$unknown
  unknown <- unknown[!is.na(unknown$UPRN), ]
  message(nrow(unknown), " UPRNs have no EPC/Price-Paid address - attempting infill.")

  # --- 1. OSM building addresses: UPRN point inside an addr-tagged polygon.
  # Chunked: the full run is ~millions of points against ~3.1M polygons, and
  # one giant st_join would hold every intermediate in memory at once.
  un_pts <- sf::st_as_sf(
    unknown[, c("UPRN", "LATITUDE", "LONGITUDE")],
    coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE
  )
  osm_cols <- c("osm_housenumber", "osm_street", "osm_postcode", "osm_city", "osm_building_name")
  chunk_size <- 500000L
  chunks <- split(seq_len(nrow(un_pts)), ceiling(seq_len(nrow(un_pts)) / chunk_size))
  osm_joined <- lapply(seq_along(chunks), function(i) {
    j <- sf::st_join(un_pts[chunks[[i]], ], osm_addresses[, osm_cols], join = sf::st_within)
    j <- sf::st_drop_geometry(j)
    j <- j[!is.na(j$osm_housenumber), ]
    message("  OSM join chunk ", i, "/", length(chunks), ": ", nrow(j), " hits")
    j
  })
  osm_joined <- dplyr::bind_rows(osm_joined)
  # a UPRN in two overlapping tagged buildings is ambiguous - drop
  osm_joined <- osm_joined[!osm_joined$UPRN %in% osm_joined$UPRN[duplicated(osm_joined$UPRN)], ]
  message(nrow(osm_joined), " unknown UPRNs matched an OSM addr-tagged building.")

  # --- 2. USRN street names for everything on a named street
  un_usrn <- dplyr::inner_join(unknown, uprn_usrn, by = "UPRN")
  un_street <- dplyr::inner_join(un_usrn, usrn_street_names, by = "USRN")
  message(nrow(un_street), " unknown UPRNs sit on a USRN with an inferred street name.")

  # --- 3. gap-guessed house numbers (subset of 2)
  known_on_usrn <- dplyr::inner_join(
    known_uprn_addresses[!is.na(known_uprn_addresses$house_number), ],
    uprn_usrn,
    by = "UPRN"
  )
  guesses <- guess_gap_numbers(
    unknown_on_usrn = un_usrn[, c("UPRN", "USRN", "X_COORDINATE", "Y_COORDINATE")],
    known_on_usrn = known_on_usrn[, c(
      "UPRN", "USRN", "house_number", "postcode", "X_COORDINATE", "Y_COORDINATE"
    )],
    usrn_geom = usrn_geom
  )
  message(nrow(guesses), " house numbers gap-guessed (flagged number_guessed = TRUE).")

  # --- assemble, OSM first (a real tagged address beats an inference)
  out_osm <- data.frame(
    UPRN = osm_joined$UPRN,
    house_number = toupper(osm_joined$osm_housenumber),
    street = normalise_name(osm_joined$osm_street),
    postcode = osm_joined$osm_postcode,
    postcode_source = ifelse(is.na(osm_joined$osm_postcode), NA_character_, "osm"),
    building_name = osm_joined$osm_building_name,
    osm_city = normalise_name(osm_joined$osm_city),
    address_source = "osm_building",
    number_source = "osm",
    number_guessed = FALSE,
    guess_between = NA_character_,
    street_confidence = "high",
    stringsAsFactors = FALSE
  )

  street_only <- un_street[!un_street$UPRN %in% out_osm$UPRN, ]
  out_street <- data.frame(
    UPRN = street_only$UPRN,
    house_number = NA_character_,
    street = street_only$street,
    postcode = NA_character_,
    postcode_source = NA_character_,
    building_name = NA_character_,
    osm_city = NA_character_,
    address_source = "usrn_street",
    number_source = NA_character_,
    number_guessed = FALSE,
    guess_between = NA_character_,
    street_confidence = street_only$street_confidence,
    stringsAsFactors = FALSE
  )
  if (nrow(guesses) > 0) {
    gi <- match(out_street$UPRN, guesses$UPRN)
    hit <- !is.na(gi)
    out_street$house_number[hit] <- guesses$house_number[gi[hit]]
    out_street$postcode[hit] <- guesses$postcode[gi[hit]]
    out_street$postcode_source[hit] <- "gap_neighbours"
    out_street$number_source[hit] <- "gap_guess"
    out_street$number_guessed[hit] <- TRUE
    out_street$guess_between[hit] <- guesses$guess_between[gi[hit]]
  }

  infill <- dplyr::bind_rows(out_osm, out_street)

  # attach district: via postcode where we have one, else via the USRN's
  # district (majority district of the street's known UPRNs), else OSM city
  infill <- dplyr::left_join(infill, postcode_district, by = "postcode")
  usrn_district <- dplyr::inner_join(
    un_usrn[, c("UPRN", "USRN")],
    usrn_street_names[, c("USRN", "district")],
    by = "USRN"
  )
  usrn_district <- usrn_district[!duplicated(usrn_district$UPRN), ]
  di <- match(infill$UPRN, usrn_district$UPRN)
  infill$district <- ifelse(!is.na(infill$district), infill$district,
    usrn_district$district[di]
  )
  infill$district <- ifelse(!is.na(infill$district), infill$district, infill$osm_city)

  # NSUL last: it validates/overrides the weaker postcode and district
  # evidence above (see apply_uprn_places)
  infill <- apply_uprn_places(infill, uprn_places)

  # coordinates from the UPRN release itself
  ci <- match(infill$UPRN, unknown$UPRN)
  infill$LATITUDE <- unknown$LATITUDE[ci]
  infill$LONGITUDE <- unknown$LONGITUDE[ci]

  infill
}

# ---------------------------------------------------------------------------
# Street centroids for numberless "land at X Road" style matching
# ---------------------------------------------------------------------------

# (district, street) -> representative point on the street. Where several
# USRNs in a district share a street name they're merged if they're
# plausibly the same street (all midpoints within 1.5km), otherwise the
# name is ambiguous in that district and dropped.
build_street_centroid_lookup <- function(usrn_street_names, usrn_geom) {
  named <- usrn_street_names[!is.na(usrn_street_names$district), ]
  geom <- usrn_geom[usrn_geom$usrn %in% named$USRN, ]
  if (nrow(geom) == 0) {
    return(data.frame(
      key = character(0), LATITUDE = numeric(0), LONGITUDE = numeric(0),
      n_usrn = integer(0)
    ))
  }
  suppressWarnings(mids <- sf::st_point_on_surface(sf::st_geometry(geom)))
  xy <- sf::st_coordinates(mids)
  pts <- data.frame(USRN = geom$usrn, x = xy[, 1], y = xy[, 2])
  pts <- dplyr::inner_join(pts, named[, c("USRN", "street", "district")], by = "USRN")

  dt <- data.table::as.data.table(pts)
  grouped <- dt[, {
    spread <- max(dist(cbind(x, y)), 0)
    list(x = mean(x), y = mean(y), n_usrn = .N, spread = spread)
  }, by = .(district, street)]
  grouped <- grouped[spread < 1500]

  lonlat <- sf::st_coordinates(sf::st_transform(
    sf::st_as_sf(grouped, coords = c("x", "y"), crs = 27700), 4326
  ))
  data.frame(
    key = paste(grouped$district, grouped$street, sep = "|"),
    LATITUDE = lonlat[, 2],
    LONGITUDE = lonlat[, 1],
    n_usrn = grouped$n_usrn,
    stringsAsFactors = FALSE
  )
}
