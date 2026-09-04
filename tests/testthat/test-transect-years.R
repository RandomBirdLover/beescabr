# Which transects existed in a given year.
#
# A published report is a claim about a season. OT was first surveyed in 2024, so
# a 2023 report map drawn today would show four transects and make it look as
# though nobody walked OT that year, when there was nothing to walk. The overall
# map (data/spatial/) shows what exists now; a report map shows that year only.
#
# The years are DECLARED in transect_years_manual.csv, not inferred from the
# survey record: "first surveyed" is not "first established", and a transect cut
# in November with no surveys until spring would be dated wrong by inference.

src("analysis/transect_years.R")

.years <- data.frame(
  transect   = c("BST", "TP", "UPMON", "OT", "OLD"),
  first_year = c(2021, 2021, 2021, 2024, 2021),
  last_year  = c(NA, NA, NA, NA, 2023),
  stringsAsFactors = FALSE)

test_that("a year before a transect existed excludes it", {
  expect_equal(transects_in_year(2023, .years), c("BST", "OLD", "TP", "UPMON"))
})

test_that("the year a transect appears includes it", {
  expect_true("OT" %in% transects_in_year(2024, .years))
})

test_that("a retired transect drops out after its last year", {
  expect_true("OLD" %in% transects_in_year(2023, .years))
  expect_false("OLD" %in% transects_in_year(2024, .years))
})

test_that("a blank last_year means still in use", {
  expect_true(all(c("BST", "TP", "UPMON", "OT") %in% transects_in_year(2026, .years)))
})

test_that("no year given returns everything -- that is the overall map", {
  expect_equal(transects_in_year(NULL, .years), sort(.years$transect))
})

test_that("a missing declaration keeps every transect rather than dropping it", {
  # losing a transect silently from a map is worse than showing one too many
  expect_equal(transects_in_year(2023, NULL), character(0))
  expect_equal(transects_in_year(2023, .years[0, ]), character(0))
})
