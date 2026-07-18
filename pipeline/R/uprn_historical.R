# The historical UPRN dataset (first/last-seen dates + coordinates for every
# UPRN across the 2020-2025 OS Open UPRN releases) and its combination with
# the EPC registers and geocoded Land Registry data.
#
# Ported from the PlaceBasedCarbonCalculator/build repo (R/uprn.R
# load_uprn_historical and R/uprn_epc_lr_combine.R) as part of moving all
# UPRN / address handling into this repo. The build repo copies are left
# untouched for now; target names are kept identical (uprn_historical,
# uprn_historical_epc_lr) so the build repo can later switch to reading them
# from this repo's store.

#' Build first/last-seen dates for UPRNs from historical OS releases
#'
#' Reads every monthly OS Open UPRN CSV in the archive (2020-2025), stamps
#' each with its release date (parsed from the yyyymm in the file name), and
#' summarises per UPRN: the first and last release it appears in and its most
#' recent coordinates. Used by the `uprn_historical` target, which supports
#' matching EPC/Land Registry records to addresses that have been created or
#' retired over time.
load_uprn_historical = function(path){
  dir.create(file.path(tempdir(),"uprn"))
  unzip(path, exdir = file.path(tempdir(),"uprn"))
  fls = list.files(file.path(tempdir(),"uprn"), pattern = ".csv", recursive = TRUE)
  dts = as.numeric(substr(fls,nchar(fls) - 9, nchar(fls) - 4))
  fls = fls[order(dts)]

  uprn = list()
  for(i in 1:length(fls)){
    sub = readr::read_csv(file.path(tempdir(),"uprn",fls[i]))
    sub$date = lubridate::ym(substr(fls[i],nchar(fls[i]) - 9, nchar(fls[i]) - 4))
    uprn[[i]] = sub
  }
  uprn = dplyr::bind_rows(uprn)

  unlink(file.path(tempdir(),"uprn"), recursive = TRUE)

  uprn = dplyr::group_by(uprn, UPRN) |>
    dplyr::summarise(
                     date_first = min(date),
                     date_last = max(date),
                     X_COORDINATE = last(X_COORDINATE),
                     Y_COORDINATE = last(Y_COORDINATE),
                     LATITUDE = last(LATITUDE),
                     LONGITUDE = last(LONGITUDE)
                     )




  uprn

}

#' Classify every UPRN as domestic/non-domestic and attach EPC and price data
#'
#' Combines the UPRN history with the domestic and non-domestic EPC registers
#' and the geocoded Land Registry data. Each UPRN is classified as domestic,
#' non-domestic, unknown or ambiguous depending on which registers it appears
#' in; `exists` (present in the latest UPRN release) and `newbuild` (first
#' seen after June 2020) flags are added; the latest sale and 2025 nowcast
#' price are joined; and the most recent EPC record is attached to the
#' matching class. Used by the `uprn_historical_epc_lr` target.
combine_uprn_epc_lr = function(uprn_historical,
                               house_prices_nowcast,
                               path_epc_dom,
                               path_epc_nondom

                               ){

  epc_dom = readRDS(path_epc_dom)
  epc_non = readRDS(path_epc_nondom)

  epc_dom = sf::st_drop_geometry(epc_dom)
  epc_non = sf::st_drop_geometry(epc_non)

  epc_dom$uprn_date_first = NULL
  epc_dom$uprn_date_last = NULL
  epc_non$uprn_date_first = NULL
  epc_non$uprn_date_last = NULL

  uprn_historical$epc_dom = uprn_historical$UPRN %in% epc_dom$UPRN
  uprn_historical$epc_nondom = uprn_historical$UPRN %in% epc_non$UPRN

  uprn_historical$lr_dom = uprn_historical$UPRN %in% house_prices_nowcast$uprn[house_prices_nowcast$property_type != "O"]
  uprn_historical$lr_nondom = uprn_historical$UPRN %in% house_prices_nowcast$uprn[house_prices_nowcast$property_type == "O"]

  uprn_historical <- uprn_historical %>%
    mutate(
      domestic = case_when(
        !epc_dom & !epc_nondom & !lr_dom & !lr_nondom ~ "unknown",
        (epc_dom | lr_dom) & !(epc_nondom | lr_nondom) ~ "domestic",
        (epc_nondom | lr_nondom) & !(epc_dom | lr_dom) ~ "non-domestic",
        (epc_dom & epc_nondom) & (!lr_nondom & !lr_dom)  ~ "ambiguous epc",
        (!epc_dom & !epc_nondom) & (lr_nondom & lr_dom)  ~ "ambiguous lr",
        TRUE ~ "ambiguous other"
      )
    )

  # Is it a old or new UPRN. "exists" = seen in the latest release in the
  # archive (max(date_last)), rather than the build repo's hard-coded
  # 2025-11-01 - same result for the current osopenuprn_2020_2025_all.zip,
  # but it survives a re-download with newer months added.
  uprn_historical <- uprn_historical %>%
    mutate(
      exists = date_last == max(date_last),
      newbuild = date_first > lubridate::ymd("2020-06-01")
    )

  # Join on Price data
  house_prices_nowcast$date = as.Date(house_prices_nowcast$date)

  uprn_historical = left_join(uprn_historical,
                              house_prices_nowcast[,c("uprn","price","price_2025","date","property_type","freehold","address1","address2")],
                              by = c("UPRN" = "uprn"))
  # Non existent properties can't have a price
  uprn_historical$price_2025[!uprn_historical$exists] = NA

  epc_dom = epc_dom[order(epc_dom$year, decreasing = TRUE),]
  epc_dom = epc_dom[!duplicated(epc_dom$UPRN),]

  epc_non = epc_non[order(epc_non$year, decreasing = TRUE),]
  epc_non = epc_non[!duplicated(epc_non$UPRN),]

  # Make three datasets, domestic, non-domestic, and unknown
  uprn_historical_dom = left_join(uprn_historical, epc_dom,
                                  by = c("UPRN" = "UPRN"))
  uprn_historical_dom = uprn_historical_dom[!uprn_historical_dom$domestic %in% c("non-domestic","unknown"),]


  uprn_historical_nondom = left_join(uprn_historical, epc_non,
                                     by = c("UPRN" = "UPRN"))
  uprn_historical_nondom = uprn_historical_nondom[!uprn_historical_nondom$domestic %in% c("domestic","unknown"),]

  uprn_historical_unknown = uprn_historical[uprn_historical$domestic == "unknown",]

  uprn_historical_dom = uprn_historical_dom[,!names(uprn_historical_dom) %in% c("epc_dom","epc_nondom","lr_dom","lr_nondom")]
  uprn_historical_nondom = uprn_historical_nondom[,!names(uprn_historical_nondom) %in% c("epc_dom","epc_nondom","lr_dom","lr_nondom")]
  uprn_historical_unknown = uprn_historical_unknown[,!names(uprn_historical_unknown) %in% c("epc_dom","epc_nondom","lr_dom","lr_nondom")]

  uprn_historical_unknown = uprn_historical_unknown[,c("UPRN","date_first","date_last","X_COORDINATE","Y_COORDINATE","LATITUDE","LONGITUDE","domestic","exists","newbuild")]

  # Output Result
  list(domestic = uprn_historical_dom,
       nondomestic = uprn_historical_nondom,
      unknown = uprn_historical_unknown)



}
