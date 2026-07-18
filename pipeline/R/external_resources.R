# Read shared GEOGRAPHY objects from the sibling PlaceBasedCarbonCalculator/
# build `targets` store - no copying multi-GB objects into this repo.
#
# Division of responsibility (July 2026): this repo now owns everything
# UPRN / address related - uprn_historical, house_price_lr_uprn,
# uprn_historical_epc_lr and friends are built HERE (see price_paid.R and
# uprn_historical.R) from the raw inputs and the EPC repo's cleaned outputs.
# The build repo remains the source of the generic statistical geographies
# read below (postcode->LSOA lookup, LSOA boundaries, LSOA->LA admin
# lookup), which its boundary-download pipeline maintains.

sibling_store <- "F:/GitHub/PlaceBasedCarbonCalculator/build/_targets"

load_sibling_target <- function(name, store = sibling_store) {
  if (!dir.exists(store)) {
    stop(
      "Sibling targets store not found at ", store, ". ",
      "This target reads the geography lookups (lookup_postcode_OA_LSOA_MSOA_2021 / ",
      "bounds_lsoa_GB_full / lsoa_admin) from the PlaceBasedCarbonCalculator/build ",
      "repo - check it's still at that path."
    )
  }
  targets::tar_read_raw(name, store = store)
}

# ONS postcode -> OA/LSOA/MSOA (2021) lookup; land_registry_add_uprn() uses
# pcds -> lsoa21cd for transactions that never match a UPRN.
load_lookup_postcode_lsoa <- function() load_sibling_target("lookup_postcode_OA_LSOA_MSOA_2021")

# Full-resolution GB LSOA/DataZone boundaries; used to spatially assign an
# LSOA to every UPRN-located transaction.
load_bounds_lsoa_gb_full <- function() load_sibling_target("bounds_lsoa_GB_full")

# LSOA -> local authority (LAD25CD) lookup; house_price_extrapolate() needs
# it for the per-LA growth multiples.
load_lsoa_admin <- function() load_sibling_target("lsoa_admin")
