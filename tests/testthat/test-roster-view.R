# The tag rebuild needs to know who was a beeple in a given year, so it does not
# regenerate intern days that the log already holds. That used to come from
# surveyor_roster.csv's per-year `role`. people_manual.csv has no years, so the view is
# derived instead -- from the intern log, which is an INPUT, so nothing is circular.
#
# The rule: you are an intern in a year if the intern log names you that year.
# Otherwise you are a beeple. Beeple never net, so their method is fixed.

src("project_info/roster_view.R")

.people <- data.frame(
  person_id            = c("p001", "p002", "p003"),
  first_name           = c("Cindy", "Brandi", "John"),
  last_name            = c("Pencek", "Sanchez", "Ascher"),
  inaturalist_username = c("carrotpeople", "randombirdlover", "johnascher"),
  collector_code       = "", determiner_code = "",
  surveyor             = c(TRUE, TRUE, FALSE),      # Ascher identifies, never surveys
  stringsAsFactors = FALSE)

.log <- data.frame(year = c(2024, 2024), person_ids = c("p002", "p002"),
                   method = "non-lethal", technique = "photo", stringsAsFactors = FALSE)

test_that("only surveyors appear", {
  v <- roster_view(.people, .log, years = 2023:2024)
  expect_false("johnascher" %in% v$inaturalist_username)
  expect_setequal(unique(v$inaturalist_username), c("carrotpeople", "randombirdlover"))
})

test_that("someone the intern log names that year is an intern", {
  v <- roster_view(.people, .log, years = 2023:2024)
  expect_equal(v$role[v$inaturalist_username == "randombirdlover" & v$year == 2024], "intern")
})

test_that("the same person is a beeple in a year the log does not name them", {
  # this is the case a per-person role cannot express, and it is why role is per-year
  v <- roster_view(.people, .log, years = 2023:2024)
  expect_equal(v$role[v$inaturalist_username == "randombirdlover" & v$year == 2023], "beeple")
})

test_that("an intern's method comes from the log, a beeple's is fixed", {
  v <- roster_view(.people, .log, years = 2023:2024)
  expect_equal(v$method[v$inaturalist_username == "randombirdlover" & v$year == 2024], "non-lethal")
  b <- v[v$inaturalist_username == "carrotpeople", ]
  expect_true(all(b$method == "non-lethal") && all(b$technique == "photo"))
})

test_that("a netting year carries lethal/net through", {
  lg <- data.frame(year = 2021, person_ids = "p002", method = "lethal",
                   technique = "net", stringsAsFactors = FALSE)
  v <- roster_view(.people, lg, years = 2021)
  expect_equal(v$method[v$inaturalist_username == "randombirdlover"], "lethal")
  expect_equal(v$technique[v$inaturalist_username == "randombirdlover"], "net")
})

test_that("every surveyor gets a row per year, so the 'one of ours' gate is complete", {
  v <- roster_view(.people, .log, years = 2021:2026)
  expect_equal(nrow(v), 2 * 6)
})

# --- surveyor_type: declared, not guessed ------------------------------------
# people_manual.csv declares what KIND of surveyor someone is. Deriving it from
# the intern log alone said "beeple" for every year an intern was not logged, so
# a 2024 intern read as a 2025 beeple and could be offered as the answer to a
# 2025 beeple calendar window. The declaration settles that.

.typed <- data.frame(
  person_id            = c("p001", "p002"),
  first_name           = c("Julia", "Julia"),
  last_name            = c("Keum", "Showalter"),
  inaturalist_username = c("julaliak", "jwanderer6"),
  collector_code = "", determiner_code = "", surveyor = TRUE,
  surveyor_type        = c("intern", "beeple"),
  stringsAsFactors = FALSE)

test_that("the declared type is used in every year", {
  v <- roster_view(.typed, NULL, years = 2024:2025)
  expect_equal(v$role[v$inaturalist_username == "julaliak"],   c("intern", "intern"))
  expect_equal(v$role[v$inaturalist_username == "jwanderer6"], c("beeple", "beeple"))
})

test_that("the intern log still forces intern for a year it names", {
  # belt and braces: if the log says they netted that year, they are an intern
  # that year whatever the column says
  lg <- data.frame(year = 2025, person_ids = "p002", method = "lethal",
                   technique = "net", stringsAsFactors = FALSE)
  v <- roster_view(.typed, lg, years = 2024:2025)
  expect_equal(v$role[v$inaturalist_username == "jwanderer6" & v$year == 2025], "intern")
  expect_equal(v$role[v$inaturalist_username == "jwanderer6" & v$year == 2024], "beeple")
})

test_that("a missing surveyor_type column falls back to the log", {
  p <- .typed; p$surveyor_type <- NULL
  lg <- data.frame(year = 2024, person_ids = "p001", method = "lethal",
                   technique = "net", stringsAsFactors = FALSE)
  v <- roster_view(p, lg, years = 2024)
  expect_equal(v$role[v$inaturalist_username == "julaliak"], "intern")
  expect_equal(v$role[v$inaturalist_username == "jwanderer6"], "beeple")
})
