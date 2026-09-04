# One list, in config.R, of everything the pipeline needs. Before this, 23 scripts each
# installed their own packages and nothing listed the whole set, so a fresh machine found
# missing packages one failure at a time. config.R holds the LIST (it is the constants
# file); installing lives in a script you run deliberately, because config.R is sourced by
# every script and must not touch the network at load.

# helpers only -- sourcing it plainly now RUNS the installer, which would put a
# network call in the test suite
src_helpers("utils/install_requirements.R", "INSTALL_SOURCED_FOR_HELPERS")

test_that("the package list is a constant in config.R", {
  expect_true(exists("BEESCABR_PACKAGES"))
  expect_type(BEESCABR_PACKAGES, "character")
  expect_gt(length(BEESCABR_PACKAGES), 20)
})

test_that("the list holds the packages the pipeline cannot run without", {
  for (p in c("dplyr", "sf", "duckdb", "httr2", "jsonlite", "rredlist", "ggplot2", "leaflet"))
    expect_true(p %in% BEESCABR_PACKAGES, info = p)
})

# There is ONE list. A second, "optional" list meant install_requirements.R named
# those packages and walked past them, so a fresh machine finished setup still
# missing something and only found out later, in a script that looked unrelated.
# Everything the project uses gets installed.
test_that("there is one package list, and it covers what was once optional", {
  expect_false(exists("BEESCABR_PACKAGES_OPTIONAL"))
  for (p in c("askpass", "getPass", "ragg"))
    expect_true(p %in% BEESCABR_PACKAGES, info = p)
})

test_that("the list has no duplicates", {
  expect_equal(anyDuplicated(BEESCABR_PACKAGES), 0L)
})

test_that("missing packages are found without installing anything", {
  have <- function(p) p %in% c("dplyr", "sf")
  expect_equal(pkgs_missing(c("dplyr", "sf", "duckdb"), have_fn = have), "duckdb")
  expect_length(pkgs_missing(c("dplyr", "sf"), have_fn = have), 0)
})

test_that("install_requirements reports, and installs only what is missing", {
  asked <- character(0)
  res <- install_requirements(
    pkgs = c("dplyr", "duckdb"),
    have_fn = function(p) p == "dplyr",
    install_fn = function(p, ...) asked <<- c(asked, p),
    say = function(...) invisible())
  expect_equal(asked, "duckdb")          # dplyr already present, not reinstalled
  expect_equal(res$missing, "duckdb")
})

test_that("nothing missing means nothing installed", {
  asked <- character(0)
  install_requirements(pkgs = c("dplyr"),
                       have_fn = function(p) TRUE,
                       install_fn = function(p, ...) asked <<- c(asked, p),
                       say = function(...) invisible())
  expect_length(asked, 0)
})

test_that("a failed install is reported, not silently swallowed", {
  res <- install_requirements(
    pkgs = "duckdb",
    have_fn = function(p) FALSE,                       # still absent after the attempt
    install_fn = function(p, ...) invisible(NULL),
    say = function(...) invisible())
  expect_equal(res$failed, "duckdb")
})

# ---- the per-script guard --------------------------------------------------------
# The 24 scripts used to each run install.packages() on load. That duplicated the
# dependency list (their blocks covered 14 packages; the real list is 36, so they had
# already drifted) and turned "run one script" into "silently install software". They now
# CHECK and stop with the one command that fixes it, and install_requirements.R installs.

src("config.R")

test_that("nothing happens when the packages are present", {
  expect_true(beescabr_require(c("dplyr", "sf"), have_fn = function(p) TRUE,
                               stop_fn = function(...) stop("should not be called")))
})

test_that("a missing package stops the script", {
  expect_error(
    beescabr_require("sf", have_fn = function(p) FALSE),
    "sf")
})

test_that("the error names the command that fixes it, not just the package", {
  msg <- tryCatch(beescabr_require(c("sf", "duckdb"), have_fn = function(p) FALSE),
                  error = function(e) conditionMessage(e))
  expect_match(msg, "install_requirements.R", fixed = TRUE)
  expect_match(msg, "sf", fixed = TRUE)
  expect_match(msg, "duckdb", fixed = TRUE)
})

test_that("only the MISSING packages are named", {
  msg <- tryCatch(beescabr_require(c("dplyr", "duckdb"),
                                   have_fn = function(p) p == "dplyr"),
                  error = function(e) conditionMessage(e))
  expect_match(msg, "duckdb", fixed = TRUE)
  expect_false(grepl("dplyr", msg, fixed = TRUE))
})

test_that("it never installs anything", {
  # the guard is a CHECK; installing is install_requirements.R's job alone
  expect_false(any(grepl("install.packages", deparse(beescabr_require), fixed = TRUE)))
})

# The file ended with `if (sys.nframe() == 0) install_requirements()`, meaning
# "run when this is the top-level script". But sys.nframe() is 0 only at the true
# top level, and source() adds frames -- it is 4 under source(). So the documented
# command, the one in PIPELINE_GUIDE.md and CLAUDE.md and printed by
# beescabr_require()'s own error message,
#
#     source("scripts/utils/install_requirements.R")
#
# defined the functions, installed nothing, and printed nothing. On a machine that
# already had the packages it looked like it worked. On a fresh one it silently
# did nothing, and the next script failed on a missing package.
test_that("sourcing install_requirements.R actually runs the check", {
  skip_if(!nzchar(Sys.which("Rscript")), "Rscript not on PATH")
  root <- .beescabr_root()
  out <- suppressWarnings(system2(
    "Rscript", c("-e", shQuote(sprintf(
      'setwd(%s); source("scripts/utils/install_requirements.R")', shQuote(root)))),
    stdout = TRUE, stderr = TRUE))
  expect_match(paste(out, collapse = "\n"), "packages",
               info = "sourcing the installer printed nothing -- the run guard never fired")
})

test_that("sourcing it for the helpers does NOT run the check", {
  skip_if(!nzchar(Sys.which("Rscript")), "Rscript not on PATH")
  root <- .beescabr_root()
  out <- suppressWarnings(system2(
    "Rscript", c("-e", shQuote(sprintf(
      'setwd(%s); INSTALL_SOURCED_FOR_HELPERS <- TRUE; source("scripts/utils/install_requirements.R")',
      shQuote(root)))),
    stdout = TRUE, stderr = TRUE))
  expect_false(grepl("Checking the", paste(out, collapse = "\n"), fixed = TRUE))
})
