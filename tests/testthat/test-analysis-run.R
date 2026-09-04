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

# Taro's report: "my analysis pipeline has 6 warnings but it doesn't tell me what
# they are." Correct -- run_analysis_script() captured every message, and
# analysis_tally() printed only the script NAMES. Knowing that six of 37 scripts
# warned, without a word of what they said, is not actionable: you cannot tell a
# harmless "NAs introduced by coercion" from a dropped column.
test_that("the warning report prints the actual messages, under each script", {
  res <- list(list(ok = TRUE, warnings = c("NAs introduced by coercion"), error = NA_character_),
              list(ok = TRUE, warnings = character(0), error = NA_character_),
              list(ok = TRUE, warnings = c("longer object length is not a multiple",
                                           "NAs introduced by coercion"), error = NA_character_))
  out <- analysis_warning_report(c("a.R", "b.R", "c.R"), res)
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "a.R", fixed = TRUE)
  expect_match(txt, "c.R", fixed = TRUE)
  expect_false(grepl("b.R", txt, fixed = TRUE))          # no warnings -> not listed
  expect_match(txt, "NAs introduced by coercion", fixed = TRUE)
  expect_match(txt, "longer object length", fixed = TRUE)
})

test_that("a warning repeated many times is reported once, with its count", {
  res <- list(list(ok = TRUE, warnings = rep("NAs introduced by coercion", 40),
                   error = NA_character_))
  txt <- paste(analysis_warning_report("a.R", res), collapse = "\n")
  expect_equal(lengths(regmatches(txt, gregexpr("NAs introduced by coercion", txt)))[[1]], 1)
  expect_match(txt, "40", fixed = TRUE)
})

test_that("nothing to report gives nothing", {
  res <- list(list(ok = TRUE, warnings = character(0), error = NA_character_))
  expect_equal(analysis_warning_report("a.R", res), character(0))
})
