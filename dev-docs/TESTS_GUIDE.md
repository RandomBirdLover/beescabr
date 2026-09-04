# `tests/` — what this folder is, and what it is not

**Short version:** nothing in this folder is part of the pipelines. You can run the
data cleaning, analysis, and publishing pipelines with this folder deleted and every
figure, table, and web page would come out identical. It is a safety net you run on
purpose, not machinery the pipelines depend on. It IS tracked in the repo, so a fresh
clone gets it -- that is deliberate, so a new maintainer inherits the safety net.

## It is not called by any pipeline

The dependency runs strictly one way:

```
tests/  ──sources──>  scripts/     (test files read the pipeline code to check it)
scripts/  ──never──>  tests/       (no pipeline script loads anything from here)
```

Verified 2026-08-24: across all of `scripts/`, there are **zero** `source()` calls
referencing this folder. A few pipeline files mention `tests/` in a code *comment*
(pointing at the test that covers them), but no code path executes it.

Nothing here is published either. `docs/` is what GitHub Pages serves; this folder
is repo source code that website visitors never see.

## So why keep it

Because this pipeline's failures are usually silent. If a change quietly started
dropping records, mis-joining plant names, or mismatching a determiner, no error
would appear and no page would look broken. The numbers would simply be wrong.
These 31 files / 278 checks are how that gets caught.

They cover the parts where that risk is highest: `inat-bee-clean`, `specimen`,
`specimen-determiners`, `checklist-build`, `taxonomy-reference`, `plant-crosswalk`,
`verify-prompt`, and the `publish` step that builds the landing page.

The whole folder is ~284 KB, so it costs nothing to carry.

## Running them

`testthat` is not installed by default on every machine. Once:

```
install.packages("testthat", repos="https://cloud.r-project.org")
```

Then the whole suite, or one file:

```
library(testthat); test_dir("tests/testthat")
library(testthat); test_file("tests/testthat/test-publish.R")
```

Tests never touch the real cache or the real API: DuckDB tests use a temp database,
and network code is faked through the `request_fn` parameter.

## If you add code

`CLAUDE.md` sets the rule: write the test first, watch it fail, then write the code.
`tests/testthat/helper.R` provides `src()` to load a module from `scripts/` and `fx()`
to reach fixtures, so tests work no matter which directory they are run from.
