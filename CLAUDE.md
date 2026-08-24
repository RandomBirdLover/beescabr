# CLAUDE.md — agent instructions for the beescabr repo

Guidance for Claude / coding agents working in this repository. Read this
before writing code. For a task-oriented guide and the layer map, see
`dev-docs/PIPELINE_GUIDE.md`.

## Project in one line

An R pipeline that ingests iNaturalist bee observations into a DuckDB cache
and builds tiered species checklists + a cleaned observation table for
Cabrillo National Monument / San Diego County.

## Git: never commit or push

Do **not** run `git commit` or `git push` (or any command that creates commits
or writes to a remote) on the user's behalf. The user handles all commits and
pushes themselves. Make and save file changes as asked, then stop — leave the
staging, committing, and pushing to them. If a task seems to call for a commit,
describe what you changed and let the user commit it.

## Golden rule: test-first (red/green), always

**`tests/` is LOCAL-ONLY (since 2026-08-24).** It is gitignored and no longer part
of the pushed repo, so a fresh clone will not have it. The rule below still applies
when working on the machine that has the suite; if `tests/` is absent, say so plainly
rather than skipping the check silently, and still deliver the test alongside the
change. Nothing in `tests/` is called by any pipeline — see `tests/README.md`.

For **every new function or behavior change**, follow this loop. Do not write
implementation code before a failing test exists.

1. **Write the test first.** Add or extend `tests/testthat/test-<module>.R`
   with concrete cases for the new behavior (normal + edge cases).
2. **Run it and confirm it FAILS (red).** A test that passes before you've
   written the code is not testing the new behavior — fix the test until it
   fails for the right reason.
   ```
   Rscript -e 'library(testthat); test_file("tests/testthat/test-<module>.R")'
   ```
3. **Write the minimal implementation** to make it pass.
4. **Re-run the module tests; iterate red → green** until all pass.
5. **Run the full suite** and confirm no regressions before finishing:
   ```
   Rscript -e 'library(testthat); test_dir("tests/testthat")'
   ```

State explicitly, in your response, that you confirmed the test failed before
implementing and passed after. If you cannot run R, say so and still deliver
the test first, written to fail.

## What to test how

- **Pure functions** (parsing, transforms, checklist logic, tag triage) —
  test directly with in-memory fixtures. These are the bulk of the suite and
  run anywhere. Prefer extracting logic into pure functions specifically so it
  is unit-testable.
- **Network code** — never hit the real API in a test. Inject a fake via the
  `request_fn` / `request_text_fn` parameter that every API-touching function
  accepts.
- **DuckDB code** — use a temp database and guard with
  `skip_if_not_installed("duckdb")` (helper `skip_if_no_store()` /
  `open_temp_store()` in `test-db.R`). Never touch the real cache in a test.

## Architecture conventions

- **Layering** (deps point downward): `config.R` → `db/*` → `api/*` →
  `pipelines/*` → `checklists/*` & `clean/*`. Keep new code in the layer that
  matches its job; don't reach upward.
- **Single responsibility per module.** Transport, cache policy, and JSON
  transforms are deliberately separate files. Keep them that way.
- **One fetch entrypoint.** Only `pipelines/ingest_inat.R` calls the
  observations API. Consumers read the DuckDB cache.
- **Centralize constants** in `config.R` (ids, CRS, paths, throttle, field
  map). Don't hardcode them in modules.
- **Source guards.** Modules source deps with `if (!exists("sym")) source(...)`
  so files can be sourced individually and by the runner without double-loading
  or auto-running `main()`.
- The CSV export is **retired** — never reintroduce `read.csv` of an export.

## Running the pipeline

```
Rscript scripts/run_data_cleaning_pipeline.R              # ingest + checklists + clean
BEESCABR_SKIP_INGEST=1 Rscript scripts/run_data_cleaning_pipeline.R   # reuse cache, no API
BEESCABR_FULL_INGEST=1 Rscript scripts/run_data_cleaning_pipeline.R   # re-fetch everything
```

## Style

Minimal, idiomatic R (tidyverse where the codebase already uses it). Comment
the *why* for non-obvious logic (rate limits, join semantics, data quirks),
not the obvious. Match the surrounding file's conventions.
