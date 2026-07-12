# Stage 5 (tasks 5/6): read the free, already-geolocated resources built by
# the sibling PlaceBasedCarbonCalculator/build `targets` pipeline, directly
# from its store - no copying multi-GB objects into this repo.

sibling_store <- "F:/GitHub/PlaceBasedCarbonCalculator/build/_targets"

load_sibling_target <- function(name, store = sibling_store) {
  if (!dir.exists(store)) {
    stop(
      "Sibling targets store not found at ", store, ". ",
      "This target reads uprn_historical / uprn_historical_epc_lr / house_price_lr_uprn ",
      "from the PlaceBasedCarbonCalculator/build repo - check it's still at that path."
    )
  }
  targets::tar_read_raw(name, store = store)
}

load_uprn_historical <- function() load_sibling_target("uprn_historical")
load_uprn_historical_epc_lr <- function() load_sibling_target("uprn_historical_epc_lr")
load_house_price_lr_uprn <- function() load_sibling_target("house_price_lr_uprn")
