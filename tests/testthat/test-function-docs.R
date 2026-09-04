# A function called from another file is this project's public API: somebody who
# did not write it has to work out what to pass and what comes back. The comment
# above it is where that answer belongs, and roxygen is the shape R programmers
# already read -- @param per argument, @return for the result.
#
# The doc is GENERATED from those blocks (dev-docs/FUNCTIONS.md). That is the
# whole point: the master table in ANALYSIS_DECISIONS.md drifted to half-true
# because it was hand-kept, and a reference nobody can trust is worse than none.
# These tests fail when a function joins the API without docs, and when the
# generated file no longer matches the source.
src("utils/function_docs.R")

.root <- .beescabr_root()

test_that("every cross-file function has a roxygen block", {
  api <- r_api_functions(file.path(.root, "scripts"))
  undocumented <- api$name[!api$documented]
  expect_equal(sort(undocumented), character(0),
               info = paste("no #' block:", paste(sort(undocumented), collapse = ", ")))
})

# Only where a #' block exists. 111 functions still carry the plain # one-liner
# they were written with, which the generator reads and the reference prints --
# a real sentence beats an empty cell. @param is required of anything converted
# to roxygen, so the blocks that exist are complete ones.
test_that("a roxygen block documents every argument the function takes", {
  api <- r_api_functions(file.path(.root, "scripts"))
  api <- api[api$roxygen, ]
  expect_equal(sort(api$name[!api$params_complete]), character(0),
               info = "a #' block is missing an @param")
})

test_that("FUNCTIONS.md is what the generator produces from the source", {
  md <- file.path(.root, "dev-docs", "FUNCTIONS.md")
  expect_true(file.exists(md))
  expect_equal(readLines(md, warn = FALSE),
               functions_md(file.path(.root, "scripts")),
               info = "stale -- regenerate with scripts/utils/function_docs.R")
})
