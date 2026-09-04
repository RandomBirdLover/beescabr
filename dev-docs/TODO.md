# TODO

**41 open items.** Completed work is not kept here -- the git history has it.

## Data — iNat bees

- [ ] **Obscured-location survey windows.** When a surveyor obscures their bee
  obs (Elena's 2022 was open, but others may not be), iNat fuzzes the coordinates
  outside the CABR box so the "surveyor+window" recovery can't place them. Short
  term: detect `coordinates_obscured` on a surveyor's obs near a window and label
  that review row `obscured-location` (+ show `place_guess`) so it can be ruled by
  hand. Long term (real fix): get true coords via an iNat project the surveyors
  trust with hidden coordinates (or user-to-user trust), then re-ingest and read
  `private_latitude` / `private_longitude`.
- [ ] **iNat observation cleanup** (beeple all years; worst: 2022 & 2025). Note: casual grade and obscured obs get dropped by the R pipeline, so these must be fixed on-platform first.
  1. **Pull + sort (R):** SD County Anthophila + target plants, Quality Grade = Any. Sort by tier (CABR box / Point Loma / SD County), by user (beeple + interns), and by date → rounds (define what a round is first).
  2. **Flag (R) — output 5 lists:** (a) missing tags on round dates, (b) tag ≠ location (spatial-join mismatch), (c) missing IDs (e.g. *Agapostemon* at genus), (d) missing obs fields (bee "visited flower?" / plant "flowering?"), (e) casual pile — set aside, don't fix in R.
  3. **Clean on-platform:** anyone-fixable → assign to beeple (tags, IDs, annotations). Observer-only (geoprivacy, captive flag, missing date/location) → contact observer. Triage casual pile: recoverable via project trust (obscured) / observer nudge (captive) / mark dead (no date or location).
  4. **Write cleaning instructions for beeple.**
  - Involves Jess, Patricia, James Hung, John Ascher. Coordination approach (claimed batches, ID disagreement process) and obs only the original observer can fix are still open questions.
- [ ] **Fix observations placed in the ocean:** some beeple/intern observations have GPS coordinates landing in water. Correct on iNat (original observer or curator) or flag in the pipeline for manual review.

## Data — iNat plants

- [ ] **Plant iNat export:** pull via "Point Loma Peninsula" place (~40k obs, under 200k cap). Add observation fields at download (this was missed last time). Quality grade = Any. **Scope: Point Loma / CABR only** — no SD County plant tier (county-wide plants exceed 200k cap; site level is the correct scope anyway). Don't treat iNat's place as ground truth — clip to own boundary in R via `st_within()`. *(Export pulled; `inat_plant_clean.R` not yet run.)*
- [ ] **Obscured plants:** for your own threatened plants, use `private_latitude`/`private_longitude` columns (true coords). Other people's obscured obs aren't exportable — get via project trust or add known species to checklist by hand. Host-plant ID use case: obscuration doesn't matter (name is visible).
- [ ] Clean plant iNat data
- [ ] Plant checklist
- [ ] Plant phenology

## Bee Specimens

- [ ] **Dr. Doug Yanega (UCR):** (a) 4 specimens to add to the official checklist — Jess says don't add to physical collection, add as "x" (museum specimen record only). (b) Needs to identify 70 *Colletes*, 10 *Hylaeus*, 15 *Perdita*, and 1 *Andrena* to species — requires an in-person trip to UCR.
- [ ] **Physical specimen box audit:** check boxes for duplicate specimens and remove any physical error flags. Identify any unidentified specimens still in box.
- [ ] **Get new SDNHM IDs from Shahan** to replace the 29 sdnhm_ids zeroed out in V13 (duplicate tags that need new labels).
- [ ] Formal specimen deposit to SDNHM (Shahan Derkarabetian)

## Pipeline design

- [ ] **Migrate the iNaturalist API from v1 to v2.** Everything currently goes through
  `https://api.inaturalist.org/v1/` (see `INAT_API_VERSION` in `scripts/config.R`).
  v2 docs: https://api.inaturalist.org/v2/docs/#/
  This is a real migration, not a URL swap:
    * v2 requires an explicit `fields` parameter -- you ask for exactly the fields you
      want and get nothing else, so `inat_flatten.R` has to be rewritten against the
      new response shape rather than the v1 shape it assumes today.
    * Endpoints and some parameter names differ; the ingest loop, the taxon cache, and
      the observation-field lookups all need re-checking.
    * Do it behind the existing injectable transport (`request_fn` / `request_text_fn`)
      so the flatten tests can run against recorded v2 responses before anything live.
    * Keep v1 working until v2 is proven -- the cache is the system of record, and a
      half-migrated ingest could write malformed rows into it.
  Pairs with the JWT authentication item: v2 is also the cleaner path to private
  coordinates for obscured observations.
- [ ] **Fold survey-tag QC into the inat_bee_clean.R rewrite.** When
  `inat_observations/inat_bee_clean.R` is rewritten, add back the two cross-checks the
  retired `survey_tag_qc.R` did (deleted; recoverable from git history):
    1. Missing-tag recovery -- obs by a known surveyor, inside the CABR box, on a
       confirmed survey day, carrying no valid Cabrillo survey tag (forgotten
       hashtag). Suggest a tag to add.
    2. Misplaced-location flag -- obs that DO carry a Cabrillo survey tag but sit
       outside the CABR box (coordinates probably wrong). Review.
  Rewrite against the CURRENT schema: crosswalk columns are `what_for` /
  `inat_tag_variants` (not `type` / `category` / `inat_variants`), and the brain's
  membership rows use `source == "beeple-window"` (not `"observation"`).
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
- [ ] **Give intermediate-rank rows their iNat taxon_id in `sd_bee_taxonomy_lookup_generated.csv` (id-first backbone).**
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
  holway_sd_bee_reference_table_v3_generated.csv + sd_bee_taxonomy_lookup_generated.csv (esp. that no id-less
  Holway rows disappear). API fallback (`/taxa?q=&rank=`) for any rank with no observed descendant.
- [ ] **Output files must be manually deleted before re-running.** `write_fresh()` does not overwrite existing CSVs — if a prior output exists, the new run silently skips the write and you get stale data. Delete the relevant generated files (under `data/inat_observations/inat_clean/`, `data/checklists/`, `data/analysis/`) before each run until this is fixed.
- [ ] **Ask Mitchell Nuckols:** does the interactive "did you review tags/fields?" prompt in `inat_bee_clean.R` conflict with how Taro wants to use this? If Taro just wants one button that runs everything and produces outputs (which is what the Rmd suggests), then stopping mid-run for user input breaks that. May need a different approach — e.g. always output the QC files and let the Rmd surface a warning instead of stopping.

## Spatial / infrastructure

- [ ] Spatial join: assign observations to transects using `buffer_10m`
- [ ] **Infer `end_transect` for non-lethal intern surveys:** use iNat obs timestamps + spatial join with transect shapefiles to determine which transect each intern finished on. Update `cabr_bee_survey_dates.csv` once inferred.
- [ ] **Casual-grade observations missing from export:** "Quality Grade = Any" does not appear to include Casual obs in the downloaded CSV (known iNat issue #4186). Need a second export pass with `quality_grade = Casual` and merge with main export.

## Analysis

- [ ] **Wire the genera/species accumulation analysis into `run_data_cleaning_pipeline.R`.**
  Built and tested, deliberately run BY HAND for now (like the notes reviewer):
  `scripts/analysis/genera_and_species_accumulation.R` reads the three
  cleaned tables (`cabr_inat_bee_clean_generated.csv`, `cabr_specimen_bee_clean_generated.csv`,
  `master_per_survey_info_generated.csv`) and writes the two survey-effort accumulation figures
  (cumulative species + genera, each with 8 lines = 4 transects x 2 methods: color =
  transect, solid = non-lethal photo / dashed = lethal net; no CI band) plus
  `transect_accumulation_summary.csv` into `data/analysis/`.
  To wire in: add a final analysis stage to `run_data_cleaning_pipeline.R` that sources it after the
  clean stage (guard on the three inputs existing; needs vegan/dplyr/stringr). Depends on
  `master_per_survey_info_generated.csv` being regenerated in the new format (see that item above).
- [ ] **Reconstruction of bee identifications:** specimens help identify non-IDed iNat obs; iNat helps direct future collecting efforts.
- [ ] **Independent bloom phenology for the availability baseline (refinement).** The selectivity test uses the community's realized plant *use* per cell as the availability proxy. A stronger version would build availability from the survey plant-bloom data (`phenology_activity.R`'s flowering records) so it's an independent bloom census rather than use-derived — this would also be the only real handle on the plant-detectability confound. (Note: as of 2026-08 no prepared plant-bloom dataset is available.)
- [ ] Bee phenology vs. plant phenology
- [ ] iNat vs. specimen / lethal vs. non-lethal comparison — do we find more bees with iNat or specimens?
- [ ] Camera quality comparison (camera vs. phone)
- [ ] Intern vs. beeple comparison
- [ ] Year vs. year comparison
- [ ] 10-minute survey analysis
- [ ] What should we target specifically for future collecting?
- [ ] **Find the bee by its plant's flowering season** — a synthesis of the
  plant–bee network and plant phenology. Proposed, never built: it needs plant
  phenology to be solid first, and `flower_flowering` is filled on 68 of 9,243 rows.

## Writing / presentation

- [ ] **Literature review:** read community science projects
- [ ] **Writing:** how many different bee species found at different geographic/taxonomic levels using iNaturalist
- [ ] Presentation

## Future

- [ ] Update methods and use iNat projects

## Handoff cleanup (opened 2026-09-03)

- [ ] **Sweep the 50 `.DS_Store` files under `data/`.** Gitignored, so they never reach
  Taro through git, but they ride along in a folder copy or a zip. One `find -delete`,
  and they regenerate harmlessly.
- [ ] **Decide what happens to `id_count`** in `people_manual.csv`. It is a hand-typed
  iNaturalist statistic (Jessica 8,258, Jon 12,173), stale the moment it is written, and
  it only orders the identifier list on the acknowledgements page. Three ways out:
  leave it, drop it and sort the list some other way, or DERIVE it by counting
  identifications per person from the observation cache. The cache holds 245,946
  identifications, so the data is already there -- the same JSON we decided not to prune.
- [ ] **Cut `PIPELINE_GUIDE.md` back to architecture.** It has accumulated
  task-level detail that now lives in the runbook and the per-folder explainers.
- [ ] **Triage this file.** 52 items, never reviewed; several are certainly done.
  Move anything finished to "Done" with the date.
