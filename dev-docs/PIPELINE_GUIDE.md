# beescabr pipeline — session guide

A guide to the API + DuckDB pipeline: what changed, how it's structured, how to
extend it, and how to direct Claude to change it efficiently. See also
`CLAUDE.md` (agent rules).

## 1. How the pipeline is built

The pipeline is cached, modular and API-driven. It was reworked from an earlier
CSV-export + monolith design; the notes below explain the shape it has now and
why each piece exists.

- **Data source: iNaturalist API → DuckDB cache** (the CSV export is retired).
  Observations are pulled once into `data/inat_observations/cache/inat_cache.duckdb`, with a
  spatial `location` geometry column so you can run manual `ST_*` queries. The
  same DuckDB file caches taxon requests.
- **Monolith split.** The 1,300-line `native_bee_checklist.R` was broken into
  focused modules under `reference/` (Holway helpers, taxonomy reference, the
  taxonomy-lookup orchestrator) and `checklists/`. All tag / survey-membership
  logic later moved OUT of `inat_bee_clean.R` into the provenance "brain",
  `project_info/finding_project_info.R`; the clean scripts now just look up each
  observation's answer by `obs_id`.
- **One command.** `scripts/run_data_cleaning_pipeline.R` ingests once, then builds
  checklists and cleans, reading the same fresh cache.
- **Ingest performance.** Raw API responses go straight to DuckDB, which parses
  + inserts + reports the pagination cursor in C++ — no R-side JSON parse or
  re-serialize. Pages insert as they arrive (flat memory) inside a transaction
  committed every N pages. Geometry is built with `ST_Point(lon,lat)`.
- **Robust flatten.** A malformed observation field (e.g. a `taxon` that's a
  bare id string) no longer crashes the read; a safe accessor yields `NA`.
- **Batched taxonomy.** Taxon resolution uses `/taxa/{ids}` (≤30 ids/request)
  with a throttle, instead of one request per taxon — fixes rate limiting.
- **Tests.** ~860 testthat assertions; pure logic runs anywhere, DB/network
  tests use temp DuckDB + injected fakes and skip when `duckdb` is absent.

## 2. Project structure

```
scripts/
  config.R                       constants: ids, CRS, paths, throttle, field map
  run_data_cleaning_pipeline.R   single entrypoint (ingest → brain → clean → checklists)
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
    qc_review_inat_location_maps.R per-observer "pins to fix" maps (stage 7d)
    bee_forage.R                 bee-obs flower_visited plants
    build_field_id_map.R         iNat obs-field id map
    qc/qc_review_inat_misid.R           likely-misID review queue
  project_info/                  THE BRAIN + its reviewers
    finding_project_info.R       provenance: membership + unknown tags/fields/notes
    finding_survey_dates.R       master_per_survey_info.csv (per-survey record)
    finding_specimen_dates.R     specimen-record aggregation (in-memory)
    finding_beeple_calendar.R    beeple calendar PDFs → windows
    resolve_beeple_transects_per_survey.R  majority-transect resolver
    rescue_on_transect_surveys.R   on-transect untagged obs → surveys
    qc_review_mastercrosswalk.R / qc_review_survey_windows.R / qc_review_mastercrosswalk_notes.R  interactive reviewers
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

The export-shaped frame is **memoized** to `data/inat_observations/cache/export_flat.rds`, keyed
by a content fingerprint of the observation + taxon caches. Re-runs that don't
change those inputs skip the (slow) flatten entirely; the two consumers in one
run share the in-memory copy. Force a rebuild with `BEESCABR_REFRESH_FLAT=1` or
by deleting the RDS.

## 3. Running it

```
Rscript scripts/run_data_cleaning_pipeline.R                       # full run
BEESCABR_SKIP_INGEST=1 Rscript scripts/run_data_cleaning_pipeline.R  # reuse cache only
BEESCABR_FULL_INGEST=1 Rscript scripts/run_data_cleaning_pipeline.R  # re-fetch all obs
Rscript -e 'library(testthat); test_dir("tests/testthat")'   # tests
```

Tuning: `INAT_THROTTLE_SEC` (pause between API calls) and `commit_every` /
`per_page` / `TAXA_BATCH_SIZE` control request rate and batch sizes.

## 4. Adding or modifying a module (test-first)

The repo is TDD (see `CLAUDE.md`). To add a function — say a new obs-field
transform:

1. Decide the layer. A pure transform → `inat_observations/engine/api/inat_flatten.R`
   or a `reference/*` / `checklists/*` file. Cache/DB behavior →
   `inat_observations/engine/db/*` or `inat_observations/engine/api/inat_cache.R`. Survey /
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

- **Name the module and its test file.** "In `inat_observations/engine/api/inat_flatten.R`,
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

---

---

## 6. iNaturalist API — what calls what

Two scripts pull from the API rather than the CSV export:

- `scripts/inat_observations/inat_bee_clean.R` — fetches survey observations, tags, and observation fields via `/v1/observations`.
- `scripts/reference/taxonomy_lookup_build.R` — per-taxon taxonomy/ancestry lookups (~400 calls, ~3–4 min), via the v1 taxa endpoints.

**Endpoint:** `https://api.inaturalist.org/v1/observations`. Read-only; no authentication required. Max `per_page = 200`; `inat_bee_clean.R` pages with an `id_above` cursor. Rate limit: ~1 request/second; scripts include `Sys.sleep(1)`.

**Why the API and not the export:** iNat's CSV export only includes observation fields the *exporter* has personally used — fields attached by other observers are invisible. The API returns every field on every observation (`ofvs`), which is what the crosswalk triage requires.

**Why v1:** v1 is what iNaturalist's own site and apps run on — it is the most stable choice. v2 exists but has returned incomplete results on some queries. iNat's deprecated version is v0 (Rails), not v1.

**If v1 is retired:** swap `/v1/` → `/v2/` and add a `fields` parameter (v2 returns minimal data by default; v1 returns everything). Watch iNaturalist's forum (News & Updates) for any sunset notice.

---

---

## 7. Reviewing unknown tags and fields

Both `inat_bee_clean.R` and `inat_plant_clean.R` triage observations against the crosswalk. Anything unrecognized is ignored, but the console prints an **ACTION NEEDED** block and writes QC files. Check after each run.

### Unknown tags

**`data/inat_observations/inat_clean/qc/cabr_inat_bee_unknown_tags.csv`** (bees) and **`cabr_inat_plant_unknown_tags.csv`** (plants).

This list is normally long and mostly harmless — camera/lens tags (`D500`, `300mm f/4`), species names, photo filenames, `City Nature Challenge`, etc. Ignore those. Scan for one thing only: a tag that looks like a **missed survey tag** — a new typo or new survey year. If you spot one, add it as an `inat_variant` on the matching crosswalk row, re-run, and those observations move from `flag` to `keep`.

This list won't trend to zero and shouldn't.

### Unknown fields

A separate **ACTION NEEDED** block covers unknown observation fields — structured key-value fields (e.g. `Nesting bee`, `on ground?`) with no crosswalk row. Unlike unknown tags, this list **should trend toward zero**: every field observers actually use should eventually have a row telling the script what to do with it.

When you see an unknown field:
1. Look it up on iNat by field ID or name.
2. Decide: relevant? Add a row with `type = obs_field`. Not relevant? Add a row with `type = ignore`.
3. Re-run — it should disappear from the ACTION NEEDED block.

---

---

## 8. Console prompts — standard keys

Every interactive prompt in the pipeline uses one consistent set of keys, so you never have to guess what `Enter` will do. There are two kinds.

**Decision gates — stop, or keep going.** These appear when a run hits something you might want to fix first (duplicate specimen IDs, off-transect survey pins, spell-check flags). They all use the same words:

- Type **`skip`** to continue past the gate (leave it for later), or **`stop`** to halt the run so you can fix it now.
- Case-insensitive, and common synonyms work: `s` / `continue` / `c` / `go` / `ok` / `y` / `yes` all continue; `x` / `halt` / `fix` / `n` / `no` all stop.
- A bare **Enter**, or anything the prompt doesn't recognize, **re-asks** instead of guessing — so a stray keystroke can never silently skip a real problem or halt the run.

**Heads-up prompts — nothing to decide.** When a step is only telling you something (e.g. "here are the maps to send your surveyors"), it ends with **"Press Enter to continue"**, and there `Enter` always means continue.

**Item-by-item reviewers** — `review_crosswalk`, `review_windows` (and transect ties), 

- **`<Enter>`** — accept the highlighted (`*`) suggestion, where one is shown.
- **`s`** — skip this item for now (it comes back next run).
- **`q`** — save everything and quit the reviewer.
- **`?`** — show the key help again.

Each reviewer adds its own action keys on top of those — e.g. windows: `y`/`n`/`u` and `l`=list URLs; crosswalk: `i`=ignore, `n`=new concept, `1,2`=file under concepts; plant names: `a`=add-as-new, or a number to file under a canonical — all listed in that reviewer's on-screen legend.

---

---

## 9. The crosswalk reference (project_tags_fields.csv)

`data/project_info/project_tags_fields.csv` controls `inat_bee_clean.R` and `inat_plant_clean.R`. Adding or editing rows changes script behavior without touching any R code.

### Column reference

| Column | What it holds |
|--------|---------------|
| `name` | Human-readable tag or field name |
| `field_id` | iNat observation field ID(s); blank for tags and derived fields |
| `category` | Grouping label (Transect, Beeple, Intern, Exclude, Field, Timing-Weather, Derived, Location) |
| `type` | Controls how the script handles this row — see Type values below |
| `datatype` | Expected value type: `text`, `taxon`, `numeric`, `time` |
| `allowed_values` | Pipe-separated valid values; `"fill in"` = auto-fetch from iNat API on next run |
| `applies_to` | Which export this row applies to: `bee`, `plant`, `both`, or `exclude` |
| `method_context` | Survey method(s): `non-lethal`, `lethal`, `both`, or `n/a` |
| `current_location` | Where the script looks for this value: `tag_list`, `obs_field`, `description (notes)`, `tag_list (photo-metadata tags)` |
| `inat_variants` | Known alternate spellings/capitalizations in raw iNat data |
| `specimen_plot_variants` | How this tag/field appears in physical plot or specimen sheets |
| `verified` | `TRUE` = confirmed against real data; `FALSE` = added but not yet spot-checked |
| `notes` | What this tag/field means and when to use it |
| `script_rules` | Logic the script applies beyond allowed_values (normalization, derivation heuristics, overrides) |

### Type values

| type | Meaning |
|------|---------|
| `tag` | iNat observation tag (free-text label). Script looks in `tag_list`. |
| `obs_field` | iNat observation field (structured key-value). Script looks in `ofvs` from the API. |
| `notes_field` | Value extracted from the free-text description/notes field. |
| `derived` | Computed by the script from other data; not directly from iNat. |
| `ignore` | **Field exists in iNat data but is not processed. The observation is kept — only this field is skipped.** Use for survey-irrelevant fields (e.g. `Fasciation`, `Leaf aroma`). |
| `exclude` | **Tag marks an entire observation to be dropped.** The observation is removed, not just the field. Use for non-Cabrillo projects swept in by the SD County pull, or pilot data excluded by the PI. |

`ignore` and `exclude` are deliberately different: `ignore` = keep the obs, skip the field; `exclude` = drop the obs entirely.

---

---

## 10. Complex rank handling

iNaturalist uses **Complex** for cryptic species groups that can't be distinguished from photos (e.g. *Andrena osmioides*, *Diadasia australis*).

- Each taxon has `complex` (complex name) and `complex_taxon_id` columns in the checklist.
- Complexes are not excluded from richness counts — each unique `taxon_id` counts as one taxon.
- `complex` is the join key for matching iNat photo observations against museum specimens. Exact `taxon_id` match is preferred; complex-level matches are flagged separately.
- In Tier 2 / Holway-format outputs, `Complex` values are prefixed `"(Complex) "` (e.g. `"(Complex) Diadasia australis"`) so they aren't misread as confirmed species binomials.

---
