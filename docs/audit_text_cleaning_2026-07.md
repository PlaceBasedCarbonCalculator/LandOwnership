# Audit: text cleaning logic and data files (July 2026)

> **Implementation status (12 Jul 2026):** all recommendations below are now
> implemented except the one that needs human review - refreshing the removal
> lists (rec 5): run `analyise_text()` (R/text_cleaning.R) over the rebuilt
> queue's AddressLines and review the candidate n-grams before appending to
> `data/clean_strings.xlsx`. Notes on the judgement calls taken:
> - **The `flag` column** (F6): terms flagged `"f"` are now *excluded* from
>   removal (read as "false positive - do not remove"; they are directional
>   fragments whose removal destroys positional meaning). The residual
>   "east of"-style glue is trimmed by the new `final_address_tidy()` instead,
>   so output converges either way. If `"f"` meant something else, flip the
>   filter in `clean_phrases()`.
> - **Rebuild the queue once** after the next `tar_make()` (F10): AddressLines
>   change, so `build_geocode_queue()` now reconciles - rows already sent to
>   Azure are kept, unsent rows with stale keys are dropped and re-added under
>   their new keys.
> - Regression tests: `Rscript pipeline/tests/test_text_cleaning.R`.
> - The new UPRN/USRN/OSM address-infill stage is documented in
>   `docs/uprn_infill_design_2026-07.md`.

**Scope.** The clean/split stage of the 2026 pipeline and the data files it depends
on: `pipeline/R/split_addresses.R`, `R/text_cleaning.R`, `R/address_functions.R`,
`pipeline/R/utils.R`, `data/clean_strings.xlsx`, `data/long_strings.xlsx`, plus the
downstream consumers of the cleaned text (`match_free_sources.R`,
`geocode_queue.R`, `geocode_batch_runner.R`, `diff_ownership.R`).

**Method.** Code review plus empirical inspection of the live geocode queue
(`data/geocoding/queue.rds`, 64,125 pending rows as of 12 Jul 2026 — note this
snapshot may reflect a smoke-test run, so absolute counts will shift on a full run;
the structural conclusions hold either way). No code was changed.

---

## 1. How the cleaning currently works (map)

1. **Categorise** (`prep_ccod.R` / `prep_ocod.R`): freehold split into
   `simple_short` / `simple_long` / `multi_postcode` / `land_pc` / `nopc` /
   `nopc_land`; leasehold and overseas each a single category.
2. **Clean** (`split_addresses.R`): the four "boilerplate" categories
   (`land_pc`, `nopc_land`, `leasehold`, `overseas`) go through
   `clean_mines → clean_spelling → clean_compass → clean_land → clean_flats →
   clean_airspace → clean_phrases`, which replace legal boilerplate with tags
   (`@MNS`, `@COMPASS`, `@LND`, `@POS`, `@EXTRA`, `@FTS`, `@ASP`) and then remove
   ~2,087 known phrases from `clean_strings.xlsx` (plus 459 very long ones from
   `long_strings.xlsx` for `nopc_land`). The **simple categories and `nopc` get no
   phrase cleaning at all.**
3. **Split** (`split_numbers` / `split_numbers_try`): number ranges expanded to one
   row per property.
4. **Consume**: cleaned `AddressLine` is used (a) as the exact-match join key for
   carrying forward 2022 Bing results, (b) with `PostalCode` to build the
   free-source match key, and (c) verbatim as the `addressLine` sent to Azure Maps.

---

## 2. Findings

Ordered by impact. F1 and F2 directly waste paid Azure quota; F3–F5 are
meaning/correctness issues; the rest are quality and hygiene.

### F1 (high): the postcode is dropped for every `simple_short` / `simple_long` row

`categorise_ccod_freehold()` builds `AddressLine` (postcode stripped from the text)
but never sets `PostalCode`; only `finish_boilerplate()` and
`split_multi_postcode()` do (`PostalCode <- Postcode`). The simple categories skip
both, so their rows reach downstream stages with `PostalCode = NA` even though the
registry `Postcode` column is populated.

Evidence from the queue: **10,496 rows (all 1,751 `simple_long` + all 8,745
`simple_short`) have `PostalCode = NA` while `Postcode` is present**, e.g.
`"34 Autumn Terrace, Leeds"` with `Postcode = "LS6 1RN"` and `PostalCode = NA`.

Consequences compound:

- `normalise_match_key(AddressLine, PostalCode)` returns `NA` without a postcode,
  so **none of these rows can ever match the free EPC / Price-Paid lookups** —
  they fall straight through to the paid queue. These are precisely the
  easiest-to-match addresses; a large fraction would resolve for free.
- `run_geocode_batch()` sends `postalCode = row$PostalCode`, so Azure geocodes
  them **without the postcode**, materially reducing accuracy for the addresses
  that should have been the most reliable.

At the ~5,000/month Azure cap, 10,496 rows is more than two months of quota that
mostly shouldn't be spent at all. This is the single highest-value fix in the
audit: one assignment (`PostalCode <- Postcode` for the simple/nopc paths) before
the free-match stage.

### F2 (high): no quality gate before the paid queue — un-geocodable strings get queued

`build_geocode_queue()` accepts anything `match_free_sources()` didn't match.
Found in the current queue:

- **10 rows whose entire `AddressLine` is `"@MNS"`** (or `@MNS` plus a fragment) —
  the mines regex consumed the whole description and the leftover tag was queued.
- **Bare house numbers** (`"1"`, `"16"`, `"47"`, `"84A"` …) — 10 rows, 4 of them
  with no postcode either. `split_numbers()` returns bare numbers when it finds no
  road token (`no_road_flag`), and nothing downstream checks the result is still an
  address.
- **53 `"Properties at …"` multi-road strings** (e.g. *"Properties at Aire Walk,
  Croft Avenue, Willow Road, … Knottingley"*) — one title covering many streets;
  a single geocode of this string is meaningless, and Azure will happily return a
  confident town centroid.
- **~214 rows starting with leftover glue** (*"the site of …"*, *"being a Garage
  on …"*, *"and 11 Wood Street ();"*).
- **Pure legalese with no address left**, e.g. *"and easement or right in
  perpetuity for all or any of the purposes of the London Electric Railway Acts
  1923 … filed at the Registry"*.

Every one of these is a wasted (or worse, silently wrong) paid call. A cheap
"geocodable?" gate before queueing — minimum length, contains a letter, not
tag-only, not matching `^Properties at`, stop-word density below a threshold —
would divert these to a review bucket instead. Wrong-but-confident results are
worse than no result, because Azure falls back to locality centroids.

### F3 (high, meaning): greedy unbounded `.*` in `clean_mines()` / `clean_airspace()` eats real address text

Both functions build patterns of the form `(START…).*(END…)` with a **greedy**
`.*` and no span limit, and the alternations have **no word boundaries**. Two
failure modes:

- **Over-match:** with several END markers in a string, the match extends to the
  *last* one, deleting genuine address text in between. Queue evidence:
  *"…being 17B Curzon Street, London within the area **@MNS** ground floor garage
  shown edged with red…"* — the middle of a real address replaced by `@MNS`.
  For airspace this is especially risky because `being` (an extremely common word)
  is in `airspace_end`.
- **Under-match:** if no END marker is present, nothing is replaced at all, and
  the whole mines description sails through (the `@MNS`-only queue rows in F2 are
  the flip side: START near position 0, END near the end, everything eaten).

Recommendation: make the middle lazy and bounded (e.g. `.{0,120}?`), anchor
alternation members with `\b`, and add a regression test file using the example
strings already present as comments in the code.

### F4 (medium, meaning): `nopc` simple rows get no cleaning at all

In `build_split_addresses()`, non-land no-postcode rows (`nopc`, 9,679 queue rows —
the second-largest CCOD block) take `AddressLine <- Property Address` and go
straight to splitting. None of `clean_land`/`clean_phrases` runs on them, which is
why the queue contains *"the site of Weeland Road, Knottingley"*, *"being
foreshore and sea bed at Bosham…"*, etc. — 2,983 queue rows are byte-identical to
their raw `Property Address`. These rows have no postcode, so the address text is
all the geocoder gets; leading glue like "the site of" measurably degrades
geocoder results. They should at least get the light-touch passes (spelling,
`@POS`-style glue removal, squish) even if the heavy phrase lists stay
boilerplate-only.

### F5 (medium): small parser bugs in `clean_flats()` / `split_numbers()`

- `clean_flats()` ordinal list has **`"eight"` instead of `"eighth"`**
  (`R/text_cleaning.R:381`); with the `\b` wrapper, "eighth floor flat" never
  matches.
- `split_numbers()` "last chance" rules are **case-asymmetric**: `gsub("odd\\)", …,
  ignore.case = FALSE)` and `gsub("\\bodd\\b ", …, ignore.case = FALSE)` but the
  `even` equivalents use `ignore.case = TRUE` (`R/address_functions.R:300-303,
  316-317`). `"(35 Odd)"` escapes normalisation where `"(35 Even)"` would not.
- `gsub("odd\\)","(ODD)")` turns `"(35 odd)"` into `"(35 (ODD)"` — unbalanced
  bracket left in the string.
- `parse_number_table()` sorts numbers as character (`order(nmbs)`), so a range
  expands as 10, 12, 2, 4… — harmless for geocoding but confusing in audits.
- Queue artefact `"1 st"` suggests ordinal suffixes ("1st", "2nd") are being split
  from their number somewhere in the explode path — worth a targeted look.

### F6 (medium, data files): the removal lists are stale 2022 artefacts with hygiene problems

`clean_strings.xlsx` (2,087 terms) and `long_strings.xlsx` (459 terms) are the only
two data files wired into the DAG, and both date from the 2022 corpus:

- **178 case-insensitive duplicate terms** and **5 NA rows** in
  `clean_strings.xlsx` (duplicates triple the already-repeated removal work — see
  F7).
- The **`flag` column is read but never used** — not by `clean_phrases()` nor by
  any 2022 script. 41 terms are flagged `"f"` (they are all directional fragments
  like `"@lnd @pos east of"`, i.e. plausibly "false positive — do not remove")
  **but they are removed anyway**. If `"f"` meant *keep*, meaning-destroying
  removals are happening silently. This needs a human decision: either honour the
  flag (`text_rem <- text_rem[is.na(text_rem$flag), ]`) or delete the column.
- The lists have **never been refreshed against 2026 boilerplate**. Queue
  residuals show what they miss: 159 rows still contain `being`, 30 `title`,
  15 `filed plan`, 14 `inclusive`, 11 `edged red`, 11 `odd`. The date regex in
  `clean_phrases()` only matches numeric `dd/mm/yyyy`, so *"…drawn on a Deed
  dated"* plus spelled-out dates ("12th March 1987") survive.
- `analyise_text()` exists precisely to mine new candidate phrases (n-gram stats
  after masking roads/places with `data/osm_unique_place_names.csv`) but is not
  part of the pipeline. Running it once over the current queue's `AddressLine`s
  would produce the 2026 refresh list cheaply.
- Everything else in `data/` (`common_land_terms*.csv/xlsx`, `failed_examples*`,
  `long_terms_nopc3.csv`, the 2022 `UK_freehold_*.Rds` intermediates, the OSM name
  CSVs) is **not referenced by the 2026 pipeline** — legacy inputs/outputs of the
  hand-run scripts. Worth moving to an `archive/` folder so it's obvious what the
  pipeline actually depends on.

### F7 (medium): `remove_strings()` — unbounded fixed-substring removal, repeated 3×

`remove_strings()` deletes each term as a **fixed substring with no word
boundaries**, three times per term. Risks and costs:

- Substring corruption: removing `"The former"` from *"the formerly known…"*
  leaves *"ly known…"*; any term that is a prefix/infix of a legitimate word can
  mangle it. Word-bounded regex (with `stri_replace_all_regex` and
  `\Q…\E`-escaped terms) is the safe equivalent.
- Cost: 2,087 terms × 3 passes ≈ 6,300 scans per address, then again inside a
  30-worker `future` pool hard-coded in `clean_phrases()`. Deduplicating the list
  (F6) and dropping the 3× loop in favour of a single pass repeated only while
  anything still matches would cut this dramatically.

### F8 (low): multi-postcode split leaves glue and `"()"` behind on the simple path

`split_multi_postcode()` tidies leading `"and "`/punctuation with a fragile chain
of eleven ordered `sub()` calls; queue row *"and 11 Wood Street (); Bradford"*
shows the escapes. Also, the `"()"` and stray-postcode strip only happens inside
`finish_boilerplate()` — `multi_postcode`-category rows on the simple path never
get it. One generic post-pass (strip empty brackets, collapse punctuation runs,
trim leading conjunctions) applied to *every* final `AddressLine` would replace
the chain and cover both paths.

### F9 (low): `@` tags are sent to the geocoder

The tags are meant as internal markers, and mostly get removed because
`clean_strings.xlsx` happens to contain tag combinations (`"@lnd @pos"` …) as
removal terms — an indirect and incomplete mechanism (the `@MNS` rows in F2 prove
it leaks). A final `stri_replace_all_regex(x, "@[A-Z]+", "")` + squish immediately
before queueing would guarantee clean geocoder input; keep the tagged intermediate
in a separate column since it usefully encodes *meaning* (what kind of title this
was) for later grading.

### F10 (informational): cleaning changes interact with carry-forward and queue keys

`carry_forward_unchanged()` joins 2026 `AddressLine` to the 2022 result's cleaned
`addressLine` **by exact string equality**, and `queue_key` is
`Title Number||AddressLine`. Two implications for acting on this audit:

- Any change to the cleaning functions changes `AddressLine` for some rows, which
  (a) drops some carry-forward matches (they fall back to needing geocoding — safe
  but costs quota) and (b) mints *new* queue keys, so already-queued rows would be
  re-added as fresh `pending` entries while the old rows linger. **Apply cleaning
  changes before the geocoding campaign starts in earnest, then rebuild the queue
  once** (or de-duplicate by Title Number + normalised address when appending).
- Current leakage is small (only ~1,000 of 64k queue rows are `unchanged` titles),
  so carry-forward is working; this is about sequencing future changes, not a
  present defect.

Also checked and fine: the 2026 extracts show no `Â£` mojibake (the
`clean_phrases()` regex for it targets a 2022 artefact and is now harmless);
`postcode_rx` is sound; `diff_titles()`' Title Number keying is solid.

---

## 3. Recommendations, ranked

**Quick wins (do before spending Azure quota):**

1. **Fix the simple-category postcode drop (F1)** — set `PostalCode <- Postcode`
   for `simple_short`/`simple_long`/`nopc` rows in `build_split_addresses()`, then
   let the rebuilt free-match stage drain the queue of easy rows. Biggest single
   improvement to both cost and accuracy.
2. **Add a geocodability gate in `build_geocode_queue()` (F2)** — divert tag-only,
   bare-number-without-postcode, `^Properties at`, and legalese-heavy rows to a
   `not_geocodable`/review status instead of `pending`.
3. **Strip all `@[A-Z]+` tags + a generic punctuation/glue tidy as a final pass
   (F8, F9)** on every `AddressLine` before matching/queueing.
4. **Decide the `flag` column's fate and dedupe `clean_strings.xlsx` (F6, F7).**

**Second wave (improves match rate and meaning):**

5. **Refresh the removal lists against 2026 residuals** — run `analyise_text()`
   over the current queue text, review the top n-grams, append to
   `clean_strings.xlsx`; add spelled-out-date and "Deed dated" patterns.
6. **Make `clean_mines`/`clean_airspace` lazy, bounded and word-bounded (F3)**,
   with a small regression test built from the example strings in the code
   comments and the queue evidence above.
7. **Give `nopc` rows the light cleaning passes (F4).**
8. **Fix the small parser bugs (F5)**: `eighth`, odd/even case symmetry,
   unbalanced `(ODD)` replacement.
9. **Widen `normalise_match_key()`**: fall back to (postcode, first building-name
   token) for named buildings, and consider a postcode-unique match (postcodes
   containing exactly one UPRN) — both lift the free-match rate before Azure.

**Process:**

10. Extend `audit_split_addresses()` with the queue-quality metrics used here
    (tag leakage, bare numbers, postcode presence, `AddressLine == Property
    Address` count) so regressions in cleaning quality show up in the
    `cleaning_audit` target instead of in spent quota.
11. Queue postcode-bearing rows first when batching (`run_geocode_batch` currently
    takes the head of the queue) — higher expected success per paid call.
