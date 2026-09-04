# The package list lives in config.R and nowhere else, on purpose. A hand-written
# page listing 39 packages would drift the moment one was added -- which is exactly
# how BEESCABR_PACKAGES_OPTIONAL ended up naming packages nothing installed.
# So PACKAGES.md is generated from the list, and this fails when it goes stale.
src_helpers("utils/install_requirements.R", "INSTALL_SOURCED_FOR_HELPERS")

# testthat runs from tests/testthat/, so the generator's repo-root-relative default
# for config.R does not resolve. Point it at the real one.
.cfg <- file.path(.beescabr_root(), "scripts", "config.R")

test_that("every declared package appears in the page", {
  md <- packages_md(.cfg)
  for (p in BEESCABR_PACKAGES)
    expect_true(any(grepl(paste0("`", p, "`"), md, fixed = TRUE)), info = p)
})

test_that("the page names no package the project does not declare", {
  md <- paste(packages_md(.cfg), collapse = "\n")
  listed <- unique(gsub("`", "", regmatches(md, gregexpr("`[a-zA-Z0-9.]+`", md))[[1]]))
  extra <- setdiff(listed, c(BEESCABR_PACKAGES, "config.R", "R",
                             "scripts/utils/install_requirements.R", "BEESCABR_PACKAGES"))
  expect_equal(sort(extra), character(0))
})

test_that("the page says how to install and how to add one", {
  md <- paste(packages_md(.cfg), collapse = "\n")
  expect_match(md, "install_requirements.R", fixed = TRUE)
  expect_match(md, "config.R", fixed = TRUE)
})

test_that("dev-docs/PACKAGES.md is what the generator produces", {
  f <- file.path(.beescabr_root(), "dev-docs", "PACKAGES.md")
  expect_true(file.exists(f))
  expect_equal(readLines(f, warn = FALSE), packages_md(.cfg),
               info = "stale -- rerun write_packages_md(.cfg)")
})
