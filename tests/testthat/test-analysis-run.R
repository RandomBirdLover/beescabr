# The analysis loop is best-effort: a script that fails must not stop the other 36.
# But "best-effort" used to mean a script could compute the WRONG answer silently --
# R collected its warnings, printed "There were 50 or more warnings" at the very end
# detached from any script name, and the tally still said 0 failed. These helpers make
# a warning attributable, and promote the one warning class that is never harmless.

src("utils/analysis_run.R")

# a fake "script": a function we can make warn or fail on demand
runner <- function(f) function(nm) f()

test_that("a clean script is ok and reports no warnings", {
  r <- run_analysis_script("clean.R", source_fn = runner(function() invisible(TRUE)))
  expect_true(r$ok)
  expect_length(r$warnings, 0)
})

test_that("a benign warning is recorded but does not fail the script", {
  r <- run_analysis_script("noisy.R", source_fn = runner(function() { warning("deprecated thing"); TRUE }))
  expect_true(r$ok)
  expect_match(r$warnings, "deprecated thing")
})

test_that("an unknown-column warning FAILS the script", {
  # this is the least_sampled_bees.R bug: a select() dropped is_genus, so `$` returned
  # NULL and every row silently took the wrong branch. Never harmless.
  r <- run_analysis_script("bug.R", source_fn = runner(function() {
    warning("Unknown or uninitialised column: `is_genus`."); TRUE }))
  expect_false(r$ok)
  expect_match(r$error, "is_genus")
})

test_that("a script that errors is reported, not propagated", {
  r <- run_analysis_script("boom.R", source_fn = runner(function() stop("kaboom")))
  expect_false(r$ok)
  expect_match(r$error, "kaboom")
})

test_that("warnings do not leak between scripts", {
  a <- run_analysis_script("a.R", source_fn = runner(function() { warning("only mine"); TRUE }))
  b <- run_analysis_script("b.R", source_fn = runner(function() invisible(TRUE)))
  expect_length(a$warnings, 1)
  expect_length(b$warnings, 0)
})

test_that("repeated identical warnings are counted, not listed 58 times", {
  r <- run_analysis_script("many.R", source_fn = runner(function() {
    for (i in 1:58) warning("Unknown thing"); TRUE }))
  expect_equal(length(r$warnings), 58)
})

# ---- the tally line ----------------------------------------------------------
test_that("a clean run says so plainly", {
  res <- list(list(ok = TRUE, warnings = character(0)), list(ok = TRUE, warnings = character(0)))
  expect_equal(analysis_tally(c("a.R", "b.R"), res), "Ran 2 analysis scripts; 0 failed.")
})

test_that("the tally NAMES the scripts that warned", {
  res <- list(list(ok = TRUE, warnings = "x"), list(ok = TRUE, warnings = character(0)))
  m <- analysis_tally(c("least_sampled_bees.R", "b.R"), res)
  expect_match(m, "1 with warnings")
  expect_match(m, "least_sampled_bees.R", fixed = TRUE)
})

test_that("the tally names failures too, and counts both", {
  res <- list(list(ok = FALSE, warnings = character(0)), list(ok = TRUE, warnings = "x"))
  m <- analysis_tally(c("broke.R", "warned.R"), res)
  expect_match(m, "1 failed")
  expect_match(m, "broke.R", fixed = TRUE)
  expect_match(m, "warned.R", fixed = TRUE)
})
