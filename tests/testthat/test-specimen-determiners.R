library(testthat)
library(dplyr)

# resolve_determiners(): map a specimen determination code ("initials + surname", e.g. "JL Mullins")
# to a determiner's iNaturalist username by matching surname + first initial against the identifier
# roster. Flags "unknown" (no match) and "ambiguous" (shared surname + initial) instead of guessing.
# Read-only -- never writes the roster.

src("specimens/specimen_clean_helpers.R")

.roster <- data.frame(
  first_name           = c("Jessica", "John", "Joel", "James", "Jane"),
  last_name            = c("Mullins", "Ascher", "Gardner", "Smith", "Smith"),
  inaturalist_username = c("jessmullins", "johnascher", "chloralictus", "jsmith1", "jsmith2"),
  stringsAsFactors = FALSE)

test_that("codes map to the roster username by surname + first initial", {
  r <- resolve_determiners(c("JL Mullins", "JS Ascher", "J Gardner"), .roster)
  expect_equal(r$determiner, c("jessmullins", "johnascher", "chloralictus"))
  expect_true(all(r$status == "matched"))
})

test_that("an unknown surname is flagged, never mismatched", {
  r <- resolve_determiners("XY Nobody", .roster)
  expect_true(is.na(r$determiner))
  expect_equal(r$status, "unknown")
})

test_that("a shared surname + initial is flagged ambiguous", {
  r <- resolve_determiners("J Smith", .roster)   # two J. Smith in the fixture
  expect_true(is.na(r$determiner))
  expect_equal(r$status, "ambiguous")
})

test_that("a blank or NA code records no determination (NA status)", {
  r <- resolve_determiners(c("", NA), .roster)
  expect_true(all(is.na(r$determiner)))
  expect_true(all(is.na(r$status)))
})
