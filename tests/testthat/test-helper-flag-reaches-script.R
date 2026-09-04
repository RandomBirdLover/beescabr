# bee_plant_matrix.R and bee_plant_explorer.R each end with a build guarded by
#     if (!exists("BPM_SOURCED_FOR_HELPERS")) { ...run the whole analysis... }
# and their tests set that flag before sourcing, meaning "give me the helpers,
# do not run the build".
#
# It never worked. src() calls source(), which evaluates in globalenv(); a flag
# assigned at the top of a testthat file lives in that FILE's environment, which
# is not on globalenv's search path. So the flag was invisible, the guard opened,
# and every single test run executed both analyses against the REAL data/ folder
# -- writing bee_plant_matrix.csv, bee_plant_pairs.csv and bee_plant_explorer.html.
# CLAUDE.md's rule is that a test never touches real data. This is that rule,
# enforced instead of assumed.

test_that("a flag set the way the tests set it reaches the sourced file", {
  tf <- tempfile(fileext = ".R")
  writeLines('.saw_flag <<- exists("BPM_SOURCED_FOR_HELPERS")', tf)
  on.exit({ unlink(tf); if (exists(".saw_flag", envir = globalenv()))
              rm(".saw_flag", envir = globalenv()) }, add = TRUE)

  # exactly what test-bee-plant-matrix.R does: assign here, then src()
  BPM_SOURCED_FOR_HELPERS <- TRUE
  src_flagged(tf, flags = "BPM_SOURCED_FOR_HELPERS")
  expect_true(get(".saw_flag", envir = globalenv()))
})

test_that("src_flagged leaves no flag behind in globalenv", {
  tf <- tempfile(fileext = ".R"); writeLines("invisible(NULL)", tf)
  on.exit(unlink(tf), add = TRUE)
  src_flagged(tf, flags = "BPM_SOURCED_FOR_HELPERS")
  expect_false(exists("BPM_SOURCED_FOR_HELPERS", envir = globalenv(), inherits = FALSE))
})

test_that("sourcing the two builders for helpers writes nothing under data/", {
  outs <- c("data/analysis/2026_generated/reference/nps_summary/bee_plant_matrix.csv",
            "data/analysis/2026_generated/reference/nps_summary/bee_plant_pairs.csv",
            "data/analysis/2026_generated/reference/bee_plant/website/bee_plant_explorer.html")
  outs <- file.path(.beescabr_root(), outs)
  skip_if_not(any(file.exists(outs)), "no generated outputs on this machine")
  before <- file.mtime(outs)

  src_helpers("analysis/bee_plant_matrix.R",   "BPM_SOURCED_FOR_HELPERS")
  src_helpers("analysis/bee_plant_explorer.R", "BPE_SOURCED_FOR_HELPERS")

  expect_equal(file.mtime(outs), before,
               info = "a test rebuilt a real data/ file -- the helper guard is open")
})
