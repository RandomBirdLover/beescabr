# beescabr pipeline — session guide

A guide to the API + DuckDB pipeline: what changed, how it's structured, how to
extend it, and how to direct Claude to change it efficiently. See also
`CLAUDE.md` (agent rules) and `ARCHITECTURE_api_rewrite.md` (layer diagram).

## 1. What changed this session

The pipeline was reworked from a CSV-export + monolith design into a cached,
modular, API-driven one.

- **Data source: iNaturalist API → DuckDB cache** (the CSV export is retired).
  Observations are pulled once into `data/cache/inat_cache.duckdb`, with a
  spatial `location` geometry column so you can run manual `ST_*` queries. The
  same DuckDB file caches taxon requests.
- **Monolith split.** The 1,300-line `native_bee_checklist.R` became focused
  modules (Holway helpers, tier build, taxonomy reference, tier-2 merge) plus a
  thin orchestrator. `inat_bee_clean.R` similarly split its tag logic into
  `clean/triage.R`.
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
  config.R                     constants: ids, CRS, paths, throttle, field map
  run_pipeline.R               single entrypoint (ingest + export + clean)
  db/
    store_conn.R               DuckDB connect + spatial/json + schema
    observations_store.R       obs upsert (raw page -> DuckDB) + geometry + reads
    taxon_store.R              taxon request cache (id:/name: keys)
    decision_store.R           persisted Holway manual-disambiguation decisions
  api/
    inat_http.R                transport: retry/backoff; parsed + raw-text GETs
    inat_flatten.R             PURE: JSON -> tibble, ofvs branch, taxon ancestry
    inat_cache.R               cache-first taxa + batched resolve_taxonomy
  pipelines/
    ingest_inat.R              THE fetch: API -> DuckDB (incremental)
    read_inat.R                DuckDB -> export-shaped data frame
  checklists/
    holway.R                   Holway backfill + cross-check keys (PURE)
    checklist_tiers.R          Tier 1: spatial split, build, finalize
    taxonomy_reference.R       bee_taxonomy_lookup.csv builder (PURE)
    tier2_merge.R              Tier 2 merged checklists + specimen (PURE)
    taxonomy_lookup_build.R    orchestrator: build_taxonomy_lookup(con)
    legacy_checklists.R        PARKED old Tier 1/2 checklist writer (not in pipeline)
    holway_reference_build.R   interactive Holway -> iNat resolver
  clean/
    triage.R                   tag crosswalk + keep/flag/exclude (PURE)
    inat_bee_clean.R           orchestrator: clean_inat_bees(con)
    specimen_clean.R           specimen QC transforms + review gate (PURE)
    specimen_bee_clean.R       orchestrator: clean_specimens() (interactive gate)
tests/testthat/                one test-<module>.R per module + fixtures/
```

Data flow: `ingest_inat` (API→cache) → `read_inat` (cache→export-shaped frame)
→ `checklists/*` and `clean/*` consume that frame. Taxonomy is resolved from
the taxon cache during the read.

The export-shaped frame is **memoized** to `data/cache/export_flat.rds`, keyed
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

1. Decide the layer. A pure transform → `api/inat_flatten.R` or a
   `checklists/*` file. Cache/DB behavior → `db/*` or `api/inat_cache.R`.
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

- **Name the module and its test file.** "In `api/inat_flatten.R`, add X;
  put tests in `test-flatten.R`" beats "add X somewhere."
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
- **Reference this guide / `ARCHITECTURE_api_rewrite.md`** so the agent loads
  the conventions instead of re-deriving them.
- **For iterative work, ask Claude to keep a scratch driver** (like the smoke
  run) to exercise the change end-to-end when the real API/DB isn't reachable.

Example prompt: *"Add an `updated_since` incremental mode to
`ingest_inat.R` so we can refresh edited observations. Test-first in
`test-db.R` with an injected fake `request_text_fn`; confirm the test fails,
then implement, then run the full suite."*
