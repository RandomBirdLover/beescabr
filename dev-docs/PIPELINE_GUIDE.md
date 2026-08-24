# beescabr pipeline — session guide

A guide to the API + DuckDB pipeline: what changed, how it's structured, how to
extend it, and how to direct Claude to change it efficiently. See also
`CLAUDE.md` (agent rules).

## 1. What changed this session

The pipeline was reworked from a CSV-export + monolith design into a cached,
modular, API-driven one.

- **Data source: iNaturalist API → DuckDB cache** (the CSV export is retired).
  Observations are pulled once into `data/observations/cache/inat_cache.duckdb`, with a
  spatial `location` geometry column so you can run manual `ST_*` queries. The
  same DuckDB file caches taxon requests.
- **Monolith split.** The 1,300-line `native_bee_checklist.R` was broken into
  focused modules under `reference/` (Holway helpers, taxonomy reference, the
  taxonomy-lookup orchestrator) and `checklists/`. All tag / survey-membership
  logic later moved OUT of `inat_bee_clean.R` into the provenance "brain",
  `project_info/finding_project_info.R`; the clean scripts now just look up each
  observation's answer by `obs_id`.
- **One command.** `scripts/run_pipeline.R` ingests once, then builds
  checklists and cleans, reading the same fresh cache.
- **Ingest performance.** Raw API responses go straight to DuckDB, which parses
  + inserts + reports the pagination cursor in C++ — no R-side JSON parse or
  re-serialize. Pages insert as they arrive (flat memory) inside a transaction
  committed every N pages. Geometry is built with `ST_Point(lon,lat)`.
- **Robust flatten.** A malformed observation field (e.g. a `taxon` that's a
  bare id string) no longer crashes the read; a safe accessor yields `NA`.
- **Batched taxonomy.** Taxon resolution uses `/taxa/{ids}` (≤30 ids/request)
  with a throttle, instead of one request per taxon — fixes rate limiting.
- **Tests.** ~110 testthat assertions; pure logic runs anywhere, DB/network
  tests use temp DuckDB + injected fakes and skip when `duckdb` is absent.

## 2. Project structure

```
scripts/
  config.R                       constants: ids, CRS, paths, throttle, field map
  run_pipeline.R                 single entrypoint (ingest → brain → clean → checklists)
  utils/utils.R                  shared helpers (read_latest, …)
  observations/
    engine/
      db/store_conn.R            DuckDB connect + spatial/json + schema
      db/observations_store.R    obs upsert (raw page → DuckDB) + geometry + reads
      db/taxon_store.R           taxon request cache (id:/name: keys)
      db/decision_store.R        persisted Holway manual-disambiguation decisions
      api/inat_http.R            transport: retry/backoff; parsed + raw-text GETs
      api/inat_flatten.R         PURE: JSON → tibble, ofvs branch, taxon ancestry
      api/inat_cache.R           cache-first taxa + batched resolve_taxonomy
      pipelines/ingest_inat.R    THE bee fetch: API → DuckDB (incremental)
      pipelines/ingest_plants.R  plant fetch → separate plant cache
      pipelines/read_inat.R      DuckDB → export-shaped data frame (.rds)
    inat_bee_clean.R             bee clean table (looks up membership by obs_id)
    inat_plant_clean.R           plant clean table
    build_location_review_maps.R per-observer "pins to fix" maps (stage 7d)
    bee_forage.R                 bee-obs flower_visited plants
    build_field_id_map.R         iNat obs-field id map
    qc/inat_misid_qc.R           likely-misID review queue
  project_info/                  THE BRAIN + its reviewers
    finding_project_info.R       provenance: membership + unknown tags/fields/notes
    finding_survey_dates.R       master_per_survey_info.csv (per-survey record)
    finding_specimen_dates.R     specimen-record aggregation (in-memory)
    finding_beeple_calendar.R    beeple calendar PDFs → windows
    resolve_beeple_transects_per_survey.R  majority-transect resolver
    rescue_on_transect_surveys.R   on-transect untagged obs → surveys
    review_crosswalk.R / review_windows.R / review_notes.R  interactive reviewers
    collect_plant_names.R        plant-name review
  reference/                     taxonomy + Holway
    holway.R / holway_reference_build.R  Holway backfill + interactive resolver
    taxonomy_reference.R         bee taxonomy lookup builder (PURE)
    taxonomy_lookup_build.R      orchestrator: sd_bee_taxonomy_lookup.csv
    manual_overrides.R / resolve_missing_ids.R / verify.R
    plant_lookup_join.R / plant_taxonomy_lookup_build.R
  checklists/
    checklist_build.R            spatial_split / lookup_subtree / combine (PURE)
    cabr_bee_checklist.R / pl_bee_checklist.R / sd_bee_checklist.R
  analysis/
    coverage_cabr_vs_holway.R / not_on_holway.R
  specimens/
    specimen_clean_helpers.R             specimen QC transforms + review gate (PURE)
    specimen_bee_clean.R         orchestrator: clean_specimens() (interactive gate)
    specimen_raw_worklist.R      raw specimen worklist (by hand)
  spatial/spatial_utils.R        boundaries, PROJECT_CRS
tests/testthat/                  one test-<module>.R per module + fixtures/
```

Data flow: `ingest_inat` / `ingest_plants` (API→cache) → `read_inat`
(cache→export-shaped `.rds`) → the **brain** `finding_project_info` decides
survey membership once → `inat_bee_clean` / `inat_plant_clean` look that up by
`obs_id` → `reference/*` (taxonomy, Holway), `checklists/*`, and `analysis/*`
consume the clean tables. Taxonomy is resolved from the taxon cache during the read.

The export-shaped frame is **memoized** to `data/observations/cache/export_flat.rds`, keyed
by a content fingerprint of the observation + taxon caches. Re-runs that don't
change those inputs skip the (slow) flatten entirely; the two consumers in one
run share the in-memory copy. Force a rebuild with `BEESCABR_REFRESH_FLAT=1` or
by deleting the RDS.

## 3. Running it

```
Rscript scripts/run_pipeline.R                          # full run
BEESCABR_SKIP_INGEST=1 Rscript scripts/run_pipeline.R   # reuse cache only
BEESCABR_FULL_INGEST=1 Rscript scripts/run_pipeline.R   # re-fetch all obs
Rscript -e 'library(testthat); test_dir("tests/testthat")'   # tests
```

Tuning: `INAT_THROTTLE_SEC` (pause between API calls) and `commit_every` /
`per_page` / `TAXA_BATCH_SIZE` control request rate and batch sizes.

## 4. Adding or modifying a module (test-first)

The repo is TDD (see `CLAUDE.md`). To add a function — say a new obs-field
transform:

1. Decide the layer. A pure transform → `observations/engine/api/inat_flatten.R`
   or a `reference/*` / `checklists/*` file. Cache/DB behavior →
   `observations/engine/db/*` or `observations/engine/api/inat_cache.R`. Survey /
   tag / membership logic → the brain, `project_info/finding_project_info.R`.
2. **Write the test first** in the matching `tests/testthat/test-<module>.R`,
   with normal and edge cases. Run it and confirm it **fails**.
3. Implement the function in the module. Keep it pure if it can be; if it needs
   the API or DB, take a `request_fn` / `con` parameter so tests can inject a
   fake / temp store.
4. Loop the module tests red → green, then run the full suite.

To modify existing behavior: add/adjust the test to pin the new behavior
(watch it fail), then change the implementation until green. The tests encode
hard-won data quirks (join semantics, NA handling, malformed records) — if one
fails after your change, understand why before "fixing" the test.

## 5. Directing Claude efficiently

This codebase is structured so an agent can make surgical changes. When asking
Claude to work here:

- **Name the module and its test file.** "In `observations/engine/api/inat_flatten.R`,
  add X; put tests in `test-flatten.R`" beats "add X somewhere."
- **Ask for test-first explicitly** (it's the default per `CLAUDE.md`, but
  restating reinforces it): "write the failing test first, confirm red, then
  implement to green."
- **Specify inputs/outputs** for pure functions: shape in, shape out, edge
  cases. That's what the test will encode.
- **For API/DB work, say "inject a fake `request_fn` / use a temp DuckDB"** so
  the agent doesn't reach for the network or the real cache.
- **Point at the layer.** "This is transport, so it goes in `inat_http.R`,
  not the ingest loop" keeps changes in the right place.
- **Batch related edits and ask for one suite run at the end** rather than many
  round-trips.
- **Reference this guide** (and the header comment block at the top of the
  relevant module) so the agent loads the conventions instead of re-deriving them.
- **For iterative work, ask Claude to keep a scratch driver** (like the smoke
  run) to exercise the change end-to-end when the real API/DB isn't reachable.

Example prompt: *"Add an `updated_since` incremental mode to
`ingest_inat.R` so we can refresh edited observations. Test-first in
`test-db.R` with an injected fake `request_text_fn`; confirm the test fails,
then implement, then run the full suite."*
