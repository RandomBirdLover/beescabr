# config.R declares what every PATHS entry IS -- a hand-maintained input, a file the
# pipeline generates, or an optional cache. check_paths() enforces that contract, so a
# rename can never again leave config.R pointing at a file nobody writes (which is how
# seven stale checklist entries survived unnoticed until 2026-08-25).

src("config.R")

test_that("every PATHS entry is classified in PATH_KIND", {
  scalars <- names(Filter(function(v) is.character(v) && length(v) == 1, PATHS))
  expect_setequal(scalars, names(PATH_KIND))
})

test_that("PATH_KIND only uses the three known kinds", {
  expect_true(all(unlist(PATH_KIND) %in% c("input", "output", "optional")))
})

test_that("check_paths reports a missing INPUT as a problem", {
  fake <- list(a = tempfile(fileext = ".csv"))
  res  <- check_paths(paths = fake, kinds = list(a = "input"))
  expect_equal(nrow(res), 1L)
  expect_equal(res$key, "a")
  expect_equal(res$kind, "input")
})

test_that("check_paths ignores a missing OPTIONAL file", {
  fake <- list(a = tempfile(fileext = ".csv"))
  expect_equal(nrow(check_paths(paths = fake, kinds = list(a = "optional"))), 0L)
})

test_that("check_paths(stage=) checks only the kind asked for", {
  f <- tempfile(fileext = ".csv"); writeLines("x", f)
  paths <- list(good = f, bad_in = tempfile(), bad_out = tempfile())
  kinds <- list(good = "input", bad_in = "input", bad_out = "output")
  expect_equal(check_paths(paths, kinds, stage = "input")$key,  "bad_in")
  expect_equal(check_paths(paths, kinds, stage = "output")$key, "bad_out")
  expect_equal(nrow(check_paths(paths, kinds)), 2L)          # no stage = both
})

test_that("check_paths passes clean when every file exists", {
  f <- tempfile(fileext = ".csv"); writeLines("x", f)
  expect_equal(nrow(check_paths(list(a = f), list(a = "input"))), 0L)
})

test_that("the REAL config passes its own input contract", {
  # PATHS are repo-root-relative and testthat runs from tests/testthat/, so check
  # from the root -- the same reason helper.R's src() sets the wd while sourcing.
  old <- setwd(.beescabr_root()); on.exit(setwd(old), add = TRUE)
  missing <- check_paths(stage = "input")
  expect_equal(nrow(missing), 0L,
               info = paste("missing inputs:", paste(missing$key, collapse = ", ")))
})

test_that("a file the pipeline generates is never gated as a hand-maintained input", {
  # PATH_KIND "input" drives the pre-flight in run_data_cleaning_pipeline.R: the run
  # STOPS if the file is missing and says "these are maintained by hand". Classifying a
  # GENERATED file that way sends someone off to hand-create a file the pipeline writes
  # itself (finding_project_info.R:348 writes master_per_survey_info_generated.csv every run).
  generated <- c("per_survey")
  expect_equal(unlist(PATH_KIND[generated]), setNames(rep("output", length(generated)), generated))
})
