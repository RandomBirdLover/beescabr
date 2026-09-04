# Tests

**1,539 checks that catch silent mistakes.** Not part of any pipeline — delete
this folder and every figure comes out identical.

## Run them

```r
install.packages("testthat")                    # once
library(testthat); test_dir("tests/testthat")   # all
library(testthat); test_file("tests/testthat/test-publish.R")   # one
```

## Why they exist

This pipeline fails *quietly*. A bad change drops records or mis-joins a name —
no error, no broken page, just wrong numbers.

| Caught this session | How |
|---|---|
| Specimen cleaning silently skipped for a week | `PATHS` key check |
| 60 specimens with no taxonomy | fallback test |
| A pipeline that published stale pages | source-path check |

## Rules

| | |
|---|---|
| **Write the test first** | Watch it fail, then write the code. See `CLAUDE.md`. |
| **Never hit the real API** | Fake it through `request_fn`. |
| **Never touch the real cache** | DuckDB tests use a temp database. |

`helper.R` gives you `src()` to load a script and `fx()` to reach fixtures.
