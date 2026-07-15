# beescabr — API + DuckDB rewrite architecture

_Added 2026-07-13. The pipeline no longer reads the iNaturalist CSV export.
Observations are pulled from the iNat API and cached in DuckDB (with a
spatial geometry column for manual queries); taxon requests are cached in
the same DuckDB file. The old `native_bee_checklist.R` monolith is split
into focused modules._

## Layers (dependencies point downward)

```
config.R                         constants: place/taxon ids, CRS, field-id map, paths

db/store_conn.R                  DuckDB connect + spatial/json extensions + schema
db/observations_store.R          observation objects + `location` GEOMETRY (EPSG:4326)
db/taxon_store.R                 taxon request cache (id: / name: keys)
db/decision_store.R              persisted Holway manual-disambiguation decisions

api/inat_http.R                  transport: retry/backoff, id-cursor pagination (PURE helpers)
api/inat_flatten.R               PURE json -> tibble: flatten_observation, flatten_ofvs, parse_taxon_ranks
api/inat_cache.R                 cache-first taxa + resolve_taxonomy (ties http + taxon_store)

pipelines/ingest_inat.R          THE fetch: API -> DuckDB (incremental by id cursor)
pipelines/read_inat.R            consume: DuckDB -> export-shaped data frame

checklists/holway.R              Holway backfill + cross-check keys (PURE)
checklists/checklist_tiers.R     Tier 1: spatial_split, build_checklist, finalize_checklist
checklists/taxonomy_reference.R  bee_taxonomy_lookup.csv builder (PURE)
checklists/tier2_merge.R         Tier 2 merged checklists + specimen evidence (PURE)
checklists/taxonomy_lookup_build.R  ORCHESTRATOR: sd_bee_taxonomy_lookup.csv (+ complex map)
checklists/legacy_checklists.R   PARKED old Tier 1/2 checklist writer (not in the pipeline)
checklists/holway_reference_build.R  interactive Holway -> iNat resolver (decision-cached)

clean/triage.R                   tag crosswalk + keep/flag/exclude (PURE)
clean/inat_bee_clean.R           observation cleaning (reads the cache, not the export)
```

## Key design choices

- **One fetch entrypoint.** Only `ingest_inat.R` calls the API for
  observations. The checklist and clean scripts both read the same cache,
  so they never diverge. Run with `BEESCABR_SKIP_INGEST=1` to reuse the
  cache offline.
- **ofvs datatype branching.** `flatten_ofvs` reads taxon-datatype fields
  from `ofv$taxon$name` (not `ofv$value`, which is the numeric id) — the
  reason the old code thought API obs-fields were unreliable.
- **Geometry column.** `inat_observations.location` is EPSG:4326 so manual
  queries are portable: `ST_Transform(location,'EPSG:4326','EPSG:26946')`
  for metric distances. Tier polygon filtering still runs in `sf`.
- **Manual intervention is reproducible.** The Holway builder persists every
  pick/skip in `holway_decisions`, so reruns never re-prompt.

## Run order

Single command — ingest, build the taxonomy lookup, and clean, in one run
(ingest happens once; both stages read the same fresh cache). Checklists are
parked (see legacy_checklists.R) while the new per-source stage is built:

```
Rscript scripts/run_pipeline.R
#   BEESCABR_SKIP_INGEST=1   reuse the cache, skip the API
#   BEESCABR_FULL_INGEST=1   re-walk the whole place (not incremental)
```

`run_pipeline.R` sources `taxonomy_lookup_build.R` (defines
`build_taxonomy_lookup(con)`) and `inat_bee_clean.R` (defines
`clean_inat_bees(con)`) and drives them. Each stage script is still runnable
on its own (it ingests then runs its stage) for debugging:

```
Rscript scripts/checklists/taxonomy_lookup_build.R  # ingest + lookup (checklists parked)
Rscript scripts/clean/inat_bee_clean.R              # ingest + triage pass
```

Occasional / interactive steps, NOT part of the single command:

```
Rscript scripts/utils/survey_dates.R                # official date files (then re-run clean for date recovery)
BEESCABR_RUN_HOLWAY=1 Rscript scripts/checklists/holway_reference_build.R
```

## Tests

`Rscript -e 'library(testthat); test_dir("tests/testthat")'`

Pure-logic tests (flatten, config, holway, tiers, tier2, triage, http,
taxonomy-reference, holway-select) run anywhere. DB-backed tests
(`test-db.R`) auto-skip unless the `duckdb` R package is installed, and use
a temp DuckDB file — they do not touch the real cache.
