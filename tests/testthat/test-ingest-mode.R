# The run-mode menu. Taro's feedback (2026-08-25): nobody remembers env-var flag
# names, so an interactive run should ASK. The prompt is a pure function with an
# injected reader so it can be tested without a console.

src("utils/ingest_mode.R")

# testthat runs non-interactively, so tests that expect the menu must say so
# explicitly; `quiet` keeps the menu text out of the test output.
ask <- function(read_fn, ...) ingest_mode_flags(read_fn = read_fn, is_interactive = TRUE,
                                                preset = character(0), say = function(...) invisible(), ...)

test_that("choosing 1 gives a normal incremental run", {
  f <- ask(function(...) "1")
  expect_equal(f[["BEESCABR_SKIP_INGEST"]], "0")
  expect_equal(f[["BEESCABR_FULL_INGEST"]], "0")
})

test_that("choosing 2 pulls bees but skips the plant fetch", {
  f <- ask(function(...) "2")
  expect_equal(f[["BEESCABR_SKIP_INGEST"]], "0")
  expect_equal(f[["BEESCABR_SKIP_PLANTS"]], "1")   # the pipeline warns loudly about this
  expect_equal(f[["BEESCABR_FULL_INGEST"]], "0")
})

test_that("choosing 3 skips iNaturalist entirely", {
  f <- ask(function(...) "3")
  expect_equal(f[["BEESCABR_SKIP_INGEST"]], "1")
  expect_equal(f[["BEESCABR_FULL_INGEST"]], "0")
})

test_that("choosing 4 forces the full rebuild", {
  f <- ask(function(...) "4")
  expect_equal(f[["BEESCABR_SKIP_INGEST"]], "0")
  expect_equal(f[["BEESCABR_FULL_INGEST"]], "1")
})

test_that("only the bees-only mode leaves plants stale", {
  for (a in c("1", "3", "4")) expect_equal(ask(function(...) a)[["BEESCABR_SKIP_PLANTS"]], "0")
  expect_equal(ask(function(...) "2")[["BEESCABR_SKIP_PLANTS"]], "1")
})

test_that("pressing Enter takes the safe default (normal run)", {
  f <- ask(function(...) "")
  expect_equal(f[["BEESCABR_SKIP_INGEST"]], "0")
  expect_equal(f[["BEESCABR_FULL_INGEST"]], "0")
})

test_that("an unrecognized answer re-asks rather than guessing", {
  answers <- c("banana", "9", "3")
  i <- 0
  f <- ask(function(...) { i <<- i + 1; answers[i] })
  expect_equal(i, 3L)                      # asked three times
  expect_equal(f[["BEESCABR_SKIP_INGEST"]], "1")  # honoured the valid one
})

test_that("every mode sets EVERY flag, so a leftover flag cannot survive", {
  # this is the stuck-flag bug the file header warns about: a FULL_INGEST=1 left
  # over from a previous session silently making every later run a slow rebuild.
  for (ans in c("1", "2", "3", "4")) {
    f <- ask(function(...) ans)
    expect_setequal(names(f), c("BEESCABR_SKIP_INGEST", "BEESCABR_FULL_INGEST",
                                "BEESCABR_SKIP_PLANTS", "BEESCABR_REFRESH"))
  }
})

test_that("non-interactive runs never prompt and change nothing", {
  called <- FALSE
  f <- ingest_mode_flags(read_fn = function(...) { called <<- TRUE; "3" },
                         is_interactive = FALSE, say = function(...) invisible())
  expect_false(called)
  expect_null(f)
})

test_that("a flag set on purpose beforehand skips the prompt", {
  called <- FALSE
  f <- ingest_mode_flags(read_fn = function(...) { called <<- TRUE; "1" },
                         is_interactive = TRUE, preset = c(BEESCABR_SKIP_INGEST = "1"),
                         say = function(...) invisible())
  expect_false(called)
  expect_null(f)
})

test_that("the full rebuild carries an expertise warning; the others do not", {
  expect_true(nzchar(paste(INGEST_MODES[["4"]]$warn, collapse = "")))
  for (k in c("1", "2", "3")) expect_null(INGEST_MODES[[k]]$warn)
})

test_that("the warning is shown when the full rebuild is chosen", {
  said <- character(0)
  ingest_mode_flags(read_fn = function(...) "4", is_interactive = TRUE,
                    preset = character(0),
                    say = function(...) said <<- c(said, paste0(...)))
  expect_true(any(grepl("bee expertise", said, ignore.case = TRUE)))
  expect_true(any(grepl("renamed|name change", said, ignore.case = TRUE)))
})

test_that("a normal run shows no expertise warning", {
  said <- character(0)
  ingest_mode_flags(read_fn = function(...) "1", is_interactive = TRUE,
                    preset = character(0),
                    say = function(...) said <<- c(said, paste0(...)))
  expect_false(any(grepl("bee expertise", said, ignore.case = TRUE)))
})
