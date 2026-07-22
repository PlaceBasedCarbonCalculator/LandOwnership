# Historical postcode boundaries, used as a last-resort GEOGRAPHIC fallback
# in fuzzy_match.R (fuzzy_match_geographic()): when a Land Registry title's
# own postcode text doesn't match any current UPRN at that house number -
# see the 2026-07 Kirklees audit ("15 Benomley Crescent" registered under
# HD5 8LU when the true UPRN sits in HD5 8LT, because the title splits
# odd/even numbers between two postcodes the wrong way round) - the
# postcode string can still be resolved to a rough location, even if it's
# been split/retired since the title was registered, and UPRN candidates
# found by proximity instead of by matching postcode/district text.
#
# Sourced from the sibling PlaceBasedCarbonCalculator/build repo's own
# _targets store (bounds_postcodes_2015/2020/2024 - OS Postcode Polygons,
# already downloaded/built there for other projects) via a cross-store
# targets::tar_read(), the same way this repo already reads other
# already-built external data rather than duplicating the download/parse
# logic. NOT re-declared as tar_target()s of our own in the sibling repo -
# this repo only ever reads them, never rebuilds them.

# One row per postcode ever seen across the three (2015/2020/2024) polygon
# releases, with a representative point (st_point_on_surface, so it always
# lands inside the polygon even for concave postcode shapes) and the most
# recent year that postcode appeared in - `last_seen_year` < 2024 flags a
# postcode that's been split/merged/retired since. The MOST RECENT sighting
# wins the coordinates: postcode boundaries rarely move far between epochs,
# but prefer the freshest when they do.
build_postcode_history_lookup <- function(build_repo_store) {
  centroid_table <- function(bounds, year) {
    suppressWarnings(pts <- sf::st_point_on_surface(sf::st_geometry(bounds)))
    pts_sf <- sf::st_transform(sf::st_sf(POSTCODE = bounds$POSTCODE, geometry = pts), 4326)
    xy <- sf::st_coordinates(pts_sf)
    data.frame(
      postcode = normalise_postcode(bounds$POSTCODE),
      LATITUDE = xy[, 2], LONGITUDE = xy[, 1],
      year = year, stringsAsFactors = FALSE
    )
  }

  bounds_2024 <- targets::tar_read(bounds_postcodes_2024, store = build_repo_store)
  bounds_2020 <- targets::tar_read(bounds_postcodes_2020, store = build_repo_store)
  bounds_2015 <- targets::tar_read(bounds_postcodes_2015, store = build_repo_store)

  all_years <- dplyr::bind_rows(
    centroid_table(bounds_2024, 2024),
    centroid_table(bounds_2020, 2020),
    centroid_table(bounds_2015, 2015)
  )
  all_years <- all_years[!is.na(all_years$postcode), ]

  dt <- data.table::as.data.table(all_years)
  data.table::setorder(dt, postcode, -year)
  out <- dt[!duplicated(dt, by = "postcode")]
  data.table::setnames(out, "year", "last_seen_year")
  message(
    nrow(out), " distinct postcodes in the historical boundary lookup (",
    sum(out$last_seen_year < 2024), " not seen in the 2024 release - ",
    "likely split/merged/retired since)."
  )
  as.data.frame(out)
}

# ---------------------------------------------------------------------------
# (postcode, street) centroids from REAL postcode polygon boundaries
# ---------------------------------------------------------------------------
#
# build_street_centroid_postcode_lookup() (uprn_infill.R) keys a centroid to
# a USRN's single MAJORITY postcode string - a fast approximation that
# breaks down for a road that genuinely runs through more than one postcode
# (a numberless "land on X Road (postcode)" title for the minority-postcode
# end of that road would get a centroid on the WRONG stretch, or even in
# the wrong postcode entirely). link_usrn_postcode_boundaries() below fixes
# this by actually intersecting USRN geometry against postcode POLYGON
# boundaries, so a road spanning several postcodes gets one centroid per
# postcode, positioned on the real stretch of road inside that boundary.
#
# Pure/testable core: takes already-loaded boundary layers (named list,
# one sf polygon layer per vintage year) rather than reading the sibling
# store itself - build_street_postcode_boundary_lookup() below is the thin
# wrapper that does the cross-store tar_read() and calls this.
#
# Performance note: for the overwhelming majority of USRNs that sit wholly
# inside ONE postcode polygon, the representative point is just
# st_point_on_surface() of the whole line (cheap, no st_intersection()
# needed, and always lands ON the line by construction). Only USRNs whose
# st_intersects() hits MORE than one postcode polygon pay the cost of an
# actual st_intersection() to work out which stretch belongs to which
# postcode - line/polygon intersection at national scale is expensive
# enough (per the fuzzy_match_geographic() header note on a similar
# national-scale join) that it's worth skipping wherever it isn't needed.
link_usrn_postcode_boundaries <- function(usrn_geom, usrn_street_names, bounds_years,
                                          chunk_size = 200000L, chunk_margin_m = 2000) {
  named <- usrn_street_names[!is.na(usrn_street_names$street), c("USRN", "street")]
  geom <- usrn_geom[usrn_geom$usrn %in% named$USRN, ]
  empty <- data.frame(
    key = character(0), LATITUDE = numeric(0), LONGITUDE = numeric(0),
    n_usrn = integer(0), last_seen_year = integer(0)
  )
  if (nrow(geom) == 0 || length(bounds_years) == 0) {
    return(empty)
  }

  # --- spatially sort the USRNs before chunking -----------------------------
  # Chunks were previously cut in whatever row order the USRN product happens
  # to arrive in, which is not geographic: every chunk therefore sprawled
  # across the whole country, and each one had to be joined against the FULL
  # national postcode layer (~1.7M polygons per vintage, three vintages).
  # Measured on a 55x40km West Yorkshire smoke run (2026-07-21): 58 minutes
  # for 51,857 USRNs against a 140k-polygon regional subset - and the
  # national target set is ~12x larger again, on top of ~27x more USRNs, so
  # the cost grows faster than linearly.
  #
  # Ordering by the Morton (Z-order) code of each USRN's representative point
  # makes consecutive rows geographic neighbours, so a chunk covers a compact
  # box and the postcode layer can be pre-filtered to that box (plus a
  # margin) before the join. Morton rather than a plain (x then y) sort
  # because the latter yields full-height vertical strips, whose bounding box
  # still spans the length of the country; Z-order keeps chunks square-ish.
  #
  # Purely a performance change: the final result is grouped by
  # (postcode, street) regardless of the order rows were produced in.
  suppressWarnings(rep_pt <- sf::st_point_on_surface(sf::st_geometry(geom)))
  rep_xy <- sf::st_coordinates(rep_pt)
  # 1km cells, clamped to a 2^16 grid - ample for GB (~700 x 1300km)
  cell_x <- pmin(pmax(as.integer(rep_xy[, 1] %/% 1000), 0L), 65535L)
  cell_y <- pmin(pmax(as.integer(rep_xy[, 2] %/% 1000), 0L), 65535L)
  morton <- rep(0, length(cell_x))
  for (b in 0:15) {
    morton <- morton +
      bitwAnd(bitwShiftR(cell_x, b), 1L) * 2^(2 * b) +
      bitwAnd(bitwShiftR(cell_y, b), 1L) * 2^(2 * b + 1)
  }
  geom <- geom[order(morton), ]

  one_year <- function(pc_bounds, year) {
    pc_bounds <- pc_bounds[, "POSTCODE"]
    if (is.na(sf::st_crs(pc_bounds))) {
      sf::st_crs(pc_bounds) <- 27700
    } else if (sf::st_crs(pc_bounds) != sf::st_crs(27700)) {
      pc_bounds <- sf::st_transform(pc_bounds, 27700)
    }
    pc_bounds <- sf::st_make_valid(pc_bounds)

    chunks <- split(seq_len(nrow(geom)), ceiling(seq_len(nrow(geom)) / chunk_size))
    out <- lapply(seq_along(chunks), function(i) {
      g <- geom[chunks[[i]], "usrn"]
      # Because `geom` is Morton-ordered above, this chunk occupies a compact
      # box - so only the postcode polygons near it can possibly intersect
      # it. Pre-filtering to that box means the join below builds its spatial
      # index over a few thousand local polygons instead of ~1.7M national
      # ones. `chunk_margin_m` guards the edges (a polygon straddling the box
      # boundary must still be considered); postcode polygons are a few
      # hundred metres across at most, so the 2km default is generous.
      bb <- sf::st_bbox(g)
      bb[c("xmin", "ymin")] <- bb[c("xmin", "ymin")] - chunk_margin_m
      bb[c("xmax", "ymax")] <- bb[c("xmax", "ymax")] + chunk_margin_m
      pc_local <- pc_bounds[
        suppressMessages(
          lengths(sf::st_intersects(pc_bounds, sf::st_as_sfc(bb))) > 0
        ),
      ]
      if (nrow(pc_local) == 0) {
        return(NULL)
      }
      touch <- suppressWarnings(sf::st_join(g, pc_local, join = sf::st_intersects))
      touch <- sf::st_drop_geometry(touch)
      touch <- touch[!is.na(touch$POSTCODE), ]
      if (nrow(touch) == 0) {
        return(NULL)
      }
      n_pc <- table(touch$usrn)
      single_usrns <- as.numeric(names(n_pc)[n_pc == 1])
      multi_usrns <- as.numeric(names(n_pc)[n_pc > 1])

      res <- list()
      # Fast path for USRNs touching exactly ONE postcode polygon: a
      # point-on-surface of the whole line is on the road and needs no
      # st_intersection. It is NOT automatically inside the polygon though -
      # postcode polygons don't tile the country, so a long rural road that
      # merely clips one polygon at its end has its midpoint out in open
      # country, far from the postcode it would be keyed to. Verify each
      # candidate actually falls within its assigned polygon and demote the
      # ones that don't to the exact intersection path below, so the
      # "on the road AND inside the postcode" guarantee holds for every row
      # this function emits, not just the multi-postcode ones.
      demoted <- numeric(0)
      if (length(single_usrns) > 0) {
        gs <- g[g$usrn %in% single_usrns, ]
        gs_pc <- normalise_postcode(touch$POSTCODE[match(gs$usrn, touch$usrn)])
        suppressWarnings(pts <- sf::st_point_on_surface(sf::st_geometry(gs)))
        pts_sf <- sf::st_sf(usrn = gs$usrn, postcode = gs_pc, geometry = pts)
        inside <- suppressWarnings(
          sf::st_join(pts_sf, pc_local, join = sf::st_within)
        )
        inside <- sf::st_drop_geometry(inside)
        # st_join can return several rows per point where polygons overlap;
        # a point counts as verified if ANY joined polygon is its own
        ok_usrn <- unique(inside$usrn[
          !is.na(inside$POSTCODE) &
            normalise_postcode(inside$POSTCODE) == inside$postcode
        ])
        keep <- gs$usrn %in% ok_usrn
        demoted <- gs$usrn[!keep]
        if (any(keep)) {
          xy <- sf::st_coordinates(pts[keep])
          res$single <- data.frame(
            usrn = gs$usrn[keep], postcode = gs_pc[keep],
            x = xy[, 1], y = xy[, 2]
          )
        }
      }
      exact_usrns <- c(multi_usrns, demoted)
      if (length(exact_usrns) > 0) {
        gm <- g[g$usrn %in% exact_usrns, ]
        pcs_touching <- pc_local[
          pc_local$POSTCODE %in% touch$POSTCODE[touch$usrn %in% exact_usrns],
        ]
        inter <- suppressWarnings(sf::st_intersection(gm, pcs_touching))
        if (nrow(inter) > 0) {
          suppressWarnings(pts <- sf::st_point_on_surface(sf::st_geometry(inter)))
          xy <- sf::st_coordinates(pts)
          res$multi <- data.frame(
            usrn = inter$usrn, postcode = normalise_postcode(inter$POSTCODE),
            x = xy[, 1], y = xy[, 2]
          )
        }
      }
      message(
        "  postcode-boundary link ", year, " chunk ", i, "/", length(chunks), ": ",
        nrow(touch), " (USRN, postcode) hits (", length(multi_usrns),
        " multi-postcode USRNs, ", length(demoted),
        " single-postcode USRNs demoted to exact intersection)"
      )
      dplyr::bind_rows(res)
    })
    out <- dplyr::bind_rows(out)
    if (nrow(out) > 0) {
      out$year <- year
    }
    out
  }

  # One vintage at a time: `bounds_years` values are FUNCTIONS returning the
  # boundary layer (see build_street_postcode_boundary_lookup()), not the
  # layers themselves, so only one national postcode-polygon layer is ever
  # resident. The three vintages are 0.7GB/1.4GB/2.3GB serialised in the
  # sibling store and expand several-fold as sf polygons; holding all three
  # alongside `geom` (1.77M USRN lines) and the per-chunk join results was a
  # real out-of-memory risk on a full national run. rm()/gc() between years
  # so the previous layer is actually released before the next is read.
  # A plain (non-function) value is still accepted so callers - including
  # the tests - can pass ready-made layers.
  all_years <- dplyr::bind_rows(lapply(names(bounds_years), function(y) {
    src <- bounds_years[[y]]
    pc_bounds <- if (is.function(src)) src() else src
    res <- one_year(pc_bounds, as.integer(y))
    rm(pc_bounds)
    gc(verbose = FALSE)
    res
  }))
  if (nrow(all_years) == 0) {
    return(empty)
  }

  all_years <- dplyr::inner_join(all_years, named, by = c("usrn" = "USRN"))
  all_years$street <- normalise_name(all_years$street)
  all_years <- all_years[!is.na(all_years$postcode) & !is.na(all_years$street), ]

  # the most recent sighting of a (postcode, street) pair wins the
  # coordinates (same "freshest wins" rule as build_postcode_history_lookup()
  # above) - a road's shape rarely changes between boundary vintages, but
  # prefer the newest evidence when several years agree
  #
  # Within that winning year the representative point is the MEDOID - the
  # candidate point closest to the group's mean - never the mean itself.
  # Every candidate is by construction a point ON the road and INSIDE the
  # postcode polygon (st_point_on_surface of a line, or of a line/polygon
  # intersection), and that is the whole guarantee this lookup exists to
  # provide. Averaging destroys it as soon as a (postcode, street) pair
  # covers more than one point - several USRNs sharing a street name in the
  # same postcode, or one USRN that leaves and re-enters a polygon and so
  # yields several intersection segments. The mean of two points on a
  # crescent, or on two disconnected stretches of the same street, sits in
  # the middle of the block: off the road, and potentially outside the
  # postcode entirely.
  dt <- data.table::as.data.table(all_years)
  # x/y are in the sort key purely to make the medoid's which.min tie-break
  # deterministic: candidates are then in a canonical order within each
  # group, so the chosen point does not depend on the order rows happened to
  # be produced in (which the Morton chunk ordering above changes).
  data.table::setorder(dt, postcode, street, -year, x, y)
  grouped <- dt[, {
    top_year <- year[1]
    keep <- which(year == top_year)
    xk <- x[keep]
    yk <- y[keep]
    if (length(keep) == 1L) {
      mx <- xk[1]
      my <- yk[1]
    } else {
      cx <- mean(xk)
      cy <- mean(yk)
      best <- which.min((xk - cx)^2 + (yk - cy)^2)
      mx <- xk[best]
      my <- yk[best]
    }
    list(
      x = mx, y = my,
      n_usrn = length(unique(usrn[keep])), last_seen_year = top_year
    )
  }, by = .(postcode, street)]

  lonlat <- sf::st_coordinates(sf::st_transform(
    sf::st_as_sf(grouped, coords = c("x", "y"), crs = 27700), 4326
  ))
  out <- data.frame(
    key = paste(grouped$postcode, grouped$street, sep = "|"),
    LATITUDE = lonlat[, 2], LONGITUDE = lonlat[, 1],
    n_usrn = grouped$n_usrn, last_seen_year = grouped$last_seen_year,
    stringsAsFactors = FALSE
  )
  message(
    nrow(out), " (postcode, street) centroids built from real postcode-boundary ",
    "intersections (", sum(out$last_seen_year < 2024), " only found in a pre-2024 boundary - ",
    "stale postcode/street combinations)."
  )
  out
}

# Thin wrapper: reads the three boundary vintages from the sibling store
# (same cross-store convention as build_postcode_history_lookup() above)
# and delegates to link_usrn_postcode_boundaries().
#
# Each vintage is passed as a THUNK (a zero-argument function doing the
# tar_read()) rather than an already-read layer, so the layers are read one
# at a time inside link_usrn_postcode_boundaries() and released before the
# next - see the memory note there. Building this list is therefore cheap
# and reads nothing; nothing is loaded until the loop asks for it.
build_street_postcode_boundary_lookup <- function(usrn_geom, usrn_street_names, build_repo_store) {
  bounds_years <- list(
    "2024" = function() targets::tar_read(bounds_postcodes_2024, store = build_repo_store),
    "2020" = function() targets::tar_read(bounds_postcodes_2020, store = build_repo_store),
    "2015" = function() targets::tar_read(bounds_postcodes_2015, store = build_repo_store)
  )
  link_usrn_postcode_boundaries(usrn_geom, usrn_street_names, bounds_years)
}
