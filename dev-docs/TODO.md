# TODO

- [ ] **Rewrite the beeple-calendar parser in R.** The original Python helper
  (`parse_beeple_calendars.py`) is parked in `_to_delete/` as reference. Porting
  it to R keeps the whole project in one language. It feeds the survey-date
  workflow that now lives in `clean/finding_project_info.R` (the brain builds the
  survey-date files from the crosswalk + survey windows).

- [ ] **Fold survey-tag QC into the inat_bee_clean.R rewrite.** When
  `clean/inat_bee_clean.R` is rewritten, add back the two cross-checks the
  retired `survey_tag_qc.R` did (parked in `_to_delete/`, also in git history):
    1. Missing-tag recovery -- obs by a known surveyor, inside the CABR box, on a
       confirmed survey day, carrying no valid Cabrillo survey tag (forgotten
       hashtag). Suggest a tag to add.
    2. Misplaced-location flag -- obs that DO carry a Cabrillo survey tag but sit
       outside the CABR box (coordinates probably wrong). Review.
  Rewrite against the CURRENT schema: crosswalk columns are `what_for` /
  `inat_tag_variants` (not `type` / `category` / `inat_variants`), and the brain's
  membership rows use `source == "beeple-window"` (not `"observation"`).

- [ ] **Notes reviewer (standalone `clean/review_notes.R`).** Built and deployed,
  but deliberately NOT wired into the pipeline -- run it by hand. It walks each
  free-text obs note the brain flagged, one at a time, with a keyword guess
  (survey / metadata / not-survey / ambiguous), and remembers every decision per
  `obs_id` in `notes_reviewed.csv` (a reviewed note never comes back). Still to do:
  tune the keyword buckets (the "metadata" guess over-fires on weather words like
  "wind" that also appear in personal notes), and decide whether reviewed
  decisions should feed back into the brain's survey-membership call or stay an
  advisory side-file.

- [ ] **Obscured-location survey windows.** When a surveyor obscures their bee
  obs (Elena's 2022 was open, but others may not be), iNat fuzzes the coordinates
  outside the CABR box so the "surveyor+window" recovery can't place them. Short
  term: detect `coordinates_obscured` on a surveyor's obs near a window and label
  that review row `obscured-location` (+ show `place_guess`) so it can be ruled by
  hand. Long term (real fix): get true coords via an iNat project the surveyors
  trust with hidden coordinates (or user-to-user trust), then re-ingest and read
  `private_latitude` / `private_longitude`.

- [x] **Wire the plant ingest.** DONE 2026-07-17 -- `engine/pipelines/ingest_plants.R`
  pulls vascular plants (Tracheophyta 211194) for Point Loma (place 132551) into a
  SEPARATE cache (`data/observations/cache/inat_cache_plant.duckdb`) -> `export_flat_plant.rds`;
  run_pipeline step 2b runs it; `FPI_EXPORTS` has the `kind="plant"` slot (absent
  export guarded). Sandbox-tested: plant obs confirm survey days (e.g. Jorge's
  plant-only days) with the bee results untouched. REMAINING on the first real run:
  sanity-check the pull count / taxon id, and file the plant obs-fields
  (phenology / flowering) that surface in the field review.

- [ ] **Add iNaturalist API authentication (JWT) for private/obscured coords.**
  The pipeline makes anonymous **v1** requests today (public data only), so obscured
  obs stay obscured. To pull true coordinates, authenticate: get a JWT from
  https://www.inaturalist.org/users/api_token (expires every 24h), send it as an
  `Authorization: <token>` header read from an env var (e.g. `BEESCABR_INAT_TOKEN`,
  never hard-coded), wired into `.inat_build_request()` in `engine/api/inat_http.R`.
  IMPORTANT: auth alone only exposes YOUR OWN private coords -- for another
  surveyor's obscured obs you ALSO need their trust (a Cabrillo project they've
  joined + shared hidden coords, or user-to-user trust). Both pieces are required.
  Pairs with the "obscured-location survey windows" item above.
  When we eventually convert v1 -> **API v2**, keep this endpoint in the back pocket
  (project-observation / coords access):
  https://api.inaturalist.org/v2/docs/#/ProjectObservations/post_project_observations

- [ ] **Regenerate `per_survey_information.csv` in the new format.**
  Run `finding_project_info()` once (RStudio, or the full pipeline). The file on disk
  is still the pre-durability manual-fill version; a run rebuilds it with `n_speci`
  computed in-memory from the newest specimen `.xlsx`, the 11 specimen-only intern
  rows (source == "specimen-record"), and the `n/a` cells (non-lethal -> no specimens,
  lethal -> no iNat obs).

- [ ] **Give intermediate-rank rows their iNat taxon_id in `sd_bee_taxonomy_lookup.csv` (id-first backbone).**
  Bug: rank rows between genus and species (subgenus, tribe, ...) get a NAME but a
  blank `taxon_id` -- e.g. subgenus `Amblyapis` has a row but no id, though iNat has
  it (700707) and 5 of its species resolved (arida 1308040, ilicifoliae 361626,
  larreae 308692, leucura 271347, parva 308693).
  Root cause: the pyramid's id-bearing backbone is rebuilt from flattened NAME columns.
  `inat_flatten.R` keeps only per-rank `taxon_*_name` (its rank list even DROPS subgenus)
  and throws away the ancestor IDS that `/taxa/{id}`'s `ancestors` payload carries;
  `taxonomy_reference.R` (`merge_holway_resolved`) then backfills ids for species + genus
  only. So intermediate ranks never get an id.
  Fix (targeted, NOT a rewrite): make the id-bearing backbone ancestry/id-driven --
  for every resolved taxon (Holway-resolved species + observed best-id taxa) take
  `itself + its ancestor objects` (id/rank/name), union, dedup by taxon_id -> every rank
  gets its id for free (Amblyapis -> 700707). Needs `inat_flatten.R` to KEEP the ancestor
  ids (currently discarded). LEAVE the id-less Holway tail untouched: `reconcile_lookup_dupes()`
  already passes NA-`taxon_id` rows through, which is what protects unpublished "sp. nov."
  names (e.g. Hesperapis cactorum) from being dropped -- they then slot under a now-id'd
  genus/subgenus. Double-check against the saved baseline copies of
  holway_sd_bee_reference_table_v3.csv + sd_bee_taxonomy_lookup.csv (esp. that no id-less
  Holway rows disappear). API fallback (`/taxa?q=&rank=`) for any rank with no observed descendant.

- [ ] **Wire the genera/species accumulation analysis into `run_pipeline.R`.**
  Built and tested, deliberately run BY HAND for now (like the notes reviewer):
  `scripts/analysis/genera_and_species_accumulation.R` reads the three
  cleaned tables (`cabr_inat_bee_clean.csv`, `cabr_specimen_bee_clean.csv`,
  `master_per_survey_info.csv`) and writes the two survey-effort accumulation figures
  (cumulative species + genera, each with 8 lines = 4 transects x 2 methods: color =
  transect, solid = non-lethal photo / dashed = lethal net; no CI band) plus
  `transect_accumulation_summary.csv` into `data/analysis/`.
  To wire in: add a final analysis stage to `run_pipeline.R` that sources it after the
  clean stage (guard on the three inputs existing; needs vegan/dplyr/stringr). Depends on
  `master_per_survey_info.csv` being regenerated in the new format (see that item above).
