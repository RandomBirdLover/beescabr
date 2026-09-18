# test-no-unguarded-build-source.R -- the suite must not rebuild the operator's data.
#
# A script that writes files ends in a build block guarded by
#   if (!exists("<X>_SOURCED_FOR_HELPERS")) { ... }
# so a test can borrow its functions without running it. src_helpers() sets that flag;
# a plain src() does not, and the build then runs against the real data/ folder.
#
# This is not hypothetical. Two tests written on 2026-09-17 used a bare src() and
# silently overwrote records_near_transect.png, records_near_transect_total.png and
# bee_plant_explorer.html mid-suite. Nothing failed; the files just changed under the
# operator while she was mid-run.
suppressWarnings(suppressMessages(library(testthat)))

# PURE: given a script's text and a test file's text, which guarded scripts does the
# test source without their flag? Returns the offending "<test> -> <script>" pairs.
unguarded_build_sources <- function(scripts, tests) {
  guarded <- Filter(Negate(is.null), lapply(names(scripts), function(path) {
    m <- regmatches(scripts[[path]],
                    regexpr("[A-Za-z0-9_]+_SOURCED_FOR_HELPERS", scripts[[path]]))
    if (!length(m)) NULL else list(path = path, flag = m[1])
  }))
  bad <- character(0)
  for (g in guarded) {
    rel <- sub("^scripts/", "", g$path)
    for (tf in names(tests)) {
      txt <- tests[[tf]]
      # sourced by path, but the flag never named anywhere in that test file
      if (grepl(paste0('src\\("', rel, '"'), txt) && !grepl(g$flag, txt, fixed = TRUE))
        bad <- c(bad, sprintf("%s -> %s (needs %s)", tf, rel, g$flag))
    }
  }
  sort(bad)
}

test_that("unguarded_build_sources spots a bare src() of a guarded script", {
  scripts <- list("scripts/analysis/x.R" = 'if (!exists("X_SOURCED_FOR_HELPERS")) { build() }')
  bad <- list("test-x.R" = 'src("analysis/x.R")')
  ok  <- list("test-x.R" = 'src_helpers("analysis/x.R", "X_SOURCED_FOR_HELPERS")')
  expect_length(unguarded_build_sources(scripts, bad), 1L)
  expect_match(unguarded_build_sources(scripts, bad), "needs X_SOURCED_FOR_HELPERS")
  expect_length(unguarded_build_sources(scripts, ok), 0L)
})

test_that("a script with no build guard is not flagged", {
  scripts <- list("scripts/analysis/pure.R" = "f <- function() 1")
  expect_length(unguarded_build_sources(scripts, list("t.R" = 'src("analysis/pure.R")')), 0L)
})

test_that("no test in this suite sources a guarded script unguarded", {
  root <- .beescabr_root()
  sp <- list.files(file.path(root, "scripts"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
  scripts <- setNames(lapply(sp, function(f) paste(readLines(f, warn = FALSE), collapse = "\n")),
                      sub(paste0("^", root, "/"), "", sp))
  tp <- list.files(file.path(root, "tests/testthat"), pattern = "^test-.*[.]R$", full.names = TRUE)
  tests <- setNames(lapply(tp, function(f) paste(readLines(f, warn = FALSE), collapse = "\n")),
                    basename(tp))
  expect_equal(unguarded_build_sources(scripts, tests), character(0))
})
