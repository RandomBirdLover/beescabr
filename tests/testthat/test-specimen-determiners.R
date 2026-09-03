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

# --- the code stored in the roster -------------------------------------------
# The roster now carries `determiner_code`: the determination string exactly as it
# is written on the specimen label. Matching that stored key is what CLAUDE.md's
# taxon rules ask for -- the surname + initial rule assembles a name at both ends
# and guesses, which silently loses multi-word surnames.

.coded <- data.frame(
  first_name           = c("John",       "Diego",      "Jessica", "Joel"),
  last_name            = c("Ascher",     "de Pedro",   "Mullins", "Gardner"),
  inaturalist_username = c("johnascher", "diegodp",    "jessmullins", "chloralictus"),
  determiner_code      = c("JS Ascher",  "D de Pedro", "",        NA),
  stringsAsFactors = FALSE)

test_that("the stored code matches the label exactly", {
  r <- resolve_determiners("JS Ascher", .coded)
  expect_equal(r$determiner, "johnascher")
  expect_equal(r$status, "matched")
})

test_that("a multi-word surname resolves through the code, not the initials rule", {
  # "D de Pedro" reads its surname as "Pedro" under the old rule and is lost
  r <- resolve_determiners("D de Pedro", .coded)
  expect_equal(r$determiner, "diegodp")
  expect_equal(r$status, "matched")
})

test_that("a row with no code still resolves by surname + first initial", {
  # Mullins has a blank code, Gardner NA: neither may go unmatched
  r <- resolve_determiners(c("JL Mullins", "J Gardner"), .coded)
  expect_equal(r$determiner, c("jessmullins", "chloralictus"))
  expect_true(all(r$status == "matched"))
})

test_that("a blank code never matches a blank determination", {
  # the match(NA, x) trap: an empty code column must not swallow an empty label
  r <- resolve_determiners(c("", NA), .coded)
  expect_true(all(is.na(r$determiner)))
  expect_true(all(is.na(r$status)))
})

test_that("two people sharing a code is ambiguous, never a guess", {
  d <- .coded; d$determiner_code <- c("JS Ascher", "JS Ascher", "", NA)
  r <- resolve_determiners("JS Ascher", d)
  expect_true(is.na(r$determiner))
  expect_equal(r$status, "ambiguous")
})

test_that("the code is matched case- and whitespace-insensitively", {
  r <- resolve_determiners(c("js ascher", "  JS Ascher  "), .coded)
  expect_equal(r$determiner, c("johnascher", "johnascher"))
})
