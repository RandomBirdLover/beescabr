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
