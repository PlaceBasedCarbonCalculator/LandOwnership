# UPRN address infill: design and provenance flags (July 2026)

New pipeline stage (`pipeline/R/uprn_infill.R`, wired in `_targets.R` as
"Stage 6b") that infers address information for UPRNs that have no EPC or
Price-Paid record, and feeds the results into the staged free-matching in
`pipeline/R/match_free_sources.R`. Written alongside the July 2026 text-cleaning
audit implementation - see `docs/audit_text_cleaning_2026-07.md`.

## Input datasets

| Dataset | Path | Contents |
|---|---|---|
| OS Open Linked Identifiers (BLPU-UPRN ↔ Street-USRN) | `PlaceBasedCarbonCalculator/inputdata/os_uprn/lids-2026-06_csv_BLPU-UPRN-Street-USRN-11.zip` | 42.3M UPRN→USRN pairs (5GB CSV; only the two id columns are read) |
| OS Open USRN | `.../os_usrn/osopenusrn_202607_gpkg.zip` | 1.77M street centrelines, BNG, mixed XY/XYZ (Z dropped on load). **No street names in this product** - only `usrn` + street type |
| OSM Great Britain extract | `.../osm/united-kingdom-latest.gpkg` + `united-kingdom-latest.osm.pbf` | The gpkg initially held only `multipolygons` (17.5M features; it was produced by the sibling repo's `read_osm_pbf_buildings()` via `osmextract::oe_read()`, which only translates the requested layer). ~3.15M buildings carry `addr:housenumber` (plus street/postcode/city) in the `other_tags` hstore. Road names come from the **pbf**: `load_osm_road_names()` asks `oe_read()` for the `lines` layer, which is translated once and appended to the same cached gpkg |
| National Statistics UPRN Lookup (NSUL, epoch 126) | `.../os_uprn/NSUL_E126_MAY_2026.zip` | Authoritative UPRN → postcode (`PCDS`) + local authority (`LAD25CD`) for every UPRN, one CSV per region (Scotland's file skipped - CCOD/OCOD is E&W only). Also ships the official LAD code→name lookup in `Documents/` |

These are plain path constants in `_targets.R` (like `inspire_path`), not
`format = "file"` targets - hashing a 30GB gpkg every `tar_make()` would cost
minutes for files that only change on manual re-download. Bump the version in
the path when a new release lands.

## How the inference works

1. **Known addresses** (`known_uprn_addresses`): one row per UPRN that already
   has an address from EPC domestic / EPC non-domestic / Price Paid (that
   priority order), with the house number and street name parsed from the first
   address line. Street names are only trusted when the line starts with a
   house number ("22 Acacia Avenue" → street "Acacia Avenue"; "Ivy Cottage" is
   a building name, not a street).
2. **USRN street names** (`usrn_street_names`): each USRN is named by majority
   vote of the parsed street names of its known UPRNs. Kept only when ≥60%
   agree; `street_confidence` is "high" (≥3 supporters, ≥80% agreement),
   "medium" (2), or "low" (1). A district is attached via the majority postcode
   and the LR-derived postcode→district lookup (`postcode_district`, built from
   CCOD/OCOD's own rows so district naming matches the Land Registry `District`
   column by construction).
   USRNs with **no EPC/PP presence at all** are named from the nearest named
   OSM road (highway classes motorway…pedestrian) instead: the probe point on
   the USRN line must lie within 15m of the road (`street_confidence =
   "osm_road"`). EPC-derived names always win where both exist, because they
   follow Land Registry address conventions. Districts for OSM-named USRNs
   (and any others the postcode lookup couldn't place) come from
   point-in-polygon against `data/la_bounds.geojson`, whose `name` field is
   already in LR's uppercase district style.
3. **OSM building addresses**: unknown-UPRN points falling inside an
   `addr:housenumber`-tagged OSM building inherit its full address
   (number/street/postcode/city). A point inside two overlapping tagged
   buildings is ambiguous and dropped. Joined in 500k-point chunks.
4. **Street inheritance**: every remaining unknown UPRN on a named USRN gets
   that street name (`address_source = "usrn_street"`).
5. **Gap-guessed house numbers** (`guess_gap_numbers`): per street, known
   numbered UPRNs are split by odd/even parity (UK convention: one parity per
   side) and everything is ordered along the street's principal axis. Where two
   consecutive known numbers differ by **exactly 4** (one missing house) and
   **exactly one** unknown UPRN projects between them, the midpoint number is
   guessed. Guards, because UK numbering is erratic:
   - neighbours < 150m apart along the axis (genuinely adjacent);
   - the unknown within 12m of the neighbours' side-of-street offset (opposite
     frontages are usually further apart and carry the other parity);
   - both neighbours in the same postcode (which the guess inherits);
   - the unknown < 75m from the USRN centreline (OS Open USRN geometry - the
     sanity check that the LIDS link and the coordinate agree);
   - a UPRN guessed by two overlapping windows, or two candidates in one gap,
     is ambiguous → no guess;
   - gaps of 6+ (two or more missing numbers) are never interpolated.

### NSUL enrichment (applied last, `apply_uprn_places()`)

`uprn_places` (UPRN → postcode + district) is built from NSUL, with LAD codes
translated to **the Land Registry's own district spelling** by majority vote
of LR `District` values over each LAD's postcodes (`build_lad_district_lookup`;
official ONS names as fallback for LADs absent from LR data). Then:

- a **gap guess is withdrawn** when the neighbours' inherited postcode
  disagrees with the UPRN's true NSUL postcode - the "sits between them"
  assumption is evidently wrong (street name is kept);
- the NSUL postcode **overrides** OSM tags / neighbour inheritance
  (`postcode_source` records the survivor: `nsul` / `osm` / `gap_neighbours`);
- the NSUL district overrides the postcode/USRN/OSM-city district fallbacks.

NSUL also upgraded the **postcode-singleton** matching stage: it now uses the
true count of UPRNs per postcode (not just EPC/PP coverage), so a postcode
that genuinely contains one UPRN resolves any LR row carrying it - quality
raised from "low" to "medium". And USRN districts fall back to the majority
NSUL district of the street's UPRNs before resorting to LA-polygon
point-in-polygon.

## Provenance flags

Every `uprn_infill` row carries:

- `address_source`: `osm_building` | `usrn_street`
- `number_source`: `osm` | `gap_guess` | NA
- `number_guessed`: TRUE only for gap-interpolated numbers, with
  `guess_between` recording the anchoring pair (e.g. `"22-26"`)
- `postcode_source`: `nsul` | `osm` | `gap_neighbours`
- `street_confidence`: high / medium / low (USRN name agreement) | `osm_road`

## How matches use it (match_free_sources.R)

Stages run cheapest/most-trustworthy first; each match is tagged
`match_quality` + `source`:

| Stage | Key | Quality |
|---|---|---|
| 1-2 | (postcode, house number) vs EPC, then Price Paid | high |
| 3 | (postcode, building name) vs EPC/PP | high |
| 4 | (district, street, number) vs known addresses | medium |
| 5 | either key vs infilled UPRNs | medium (OSM), **guess** (gap-guessed) |
| 6 | postcode-singleton (NSUL: the postcode contains exactly one UPRN) | medium |
| 7 | (district, street) → USRN street centroid, numberless rows only | street |

Stage 7 returns **no UPRN** - just a representative point on the named street,
for "land at X Road"-style titles; where one street name maps to several USRNs
in a district they're merged only if all midpoints sit within 1.5km, otherwise
the name is ambiguous there and dropped. Consumers should filter by
`match_quality`: treat `guess`, `low` and `street` as indicative, not resolved.

## Known limitations

- OSM road naming probes a single point on each USRN: on long curved USRNs
  the probe can sit nearer a side road than the street itself, and unnamed
  service/estate roads adjacent to a named road can pick up its name. The 15m
  gate keeps this rare, and these names only ever feed "medium"/"street"
  quality matches - but treat `street_confidence = "osm_road"` accordingly.
- Principal-axis ordering is weakest on strongly curved or L-shaped streets;
  the between-neighbours + same-side + distance guards mean misordering
  normally suppresses a guess rather than producing a wrong one, but it will
  also miss some genuine gaps. Conservative by design.
- The postcode-singleton stage is only as good as EPC/PP coverage of the
  postcode - a postcode with one EPC record may still contain thirty
  properties. Hence quality "low".
- Welsh dual-named streets and streets renamed between EPC lodgement and now
  can produce sub-60% agreement and be dropped.
- `data/la_bounds.geojson` covers England & Wales only, so Scottish USRNs
  (OS Open USRN is GB-wide) never receive a district. Harmless: CCOD/OCOD is
  England & Wales only, so those streets can never be matched anyway.
