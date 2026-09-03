# participation.csv -- who was in the field, which year, in what capacity.
#
# GENERATED from the survey record, never hand-typed. The reason is a real bug:
# the old roster recorded who was ASSIGNED, so Michael Ready's 2022 row existed
# for six calendar windows he never showed up to. The pipeline already refuses
# to turn a calendar assignment into a survey ("no tag = not a survey day");
# this file applies the same rule to people.

src("project_info/build_participation.R")

.people <- data.frame(
  person_id            = c("p001", "p002", "p003"),
  first_name           = c("Cindy", "Sam", "Amy"),
  last_name            = c("Pencek", "O'Dell", "Geffre"),
  inaturalist_username = c("carrotpeople", "wranglebees", ""),
  collector_code       = c("", "S O'Dell", "A Geffre"),
  determiner_code      = "",
  stringsAsFactors = FALSE)

.surveys <- data.frame(
  year               = c(2021, 2021, 2022, 2022),
  role               = c("beeple", "intern", "intern", "beeple"),
  method             = c("non-lethal", "lethal", "lethal", "non-lethal"),
  surveyors          = c("Cindy Pencek", "Sam O'Dell, Amy Geffre", "CSBI Interns", "Cindy Pencek"),
  inat_username      = c("carrotpeople", "n/a", "n/a", "carrotpeople"),
  specimen_collector = c("", "", "CSBI Interns", ""),
  stringsAsFactors = FALSE)

test_that("a person-year is recorded once per role and method", {
  p <- participation_from_surveys(.surveys, .people)
  expect_equal(nrow(p), 4)
  expect_equal(sort(p$person_id), c("p001", "p001", "p002", "p003"))
})

test_that("people are found by name, by museum label, and by iNat handle", {
  p <- participation_from_surveys(.surveys, .people)
  expect_true(all(c("p002", "p003") %in% p$person_id[p$year == 2021 & p$role == "intern"]))
  expect_equal(p$role[p$person_id == "p001" & p$year == 2021], "beeple")
})

test_that("a group label produces no participation row", {
  # "CSBI Interns" is not a person; inventing rows for it would credit nobody
  p <- participation_from_surveys(.surveys, .people)
  expect_equal(sum(p$year == 2022 & p$role == "intern"), 0)
})

test_that("one person doing two methods in a year gets a row for each", {
  s <- rbind(.surveys, data.frame(year = 2021, role = "intern", method = "non-lethal",
                                  surveyors = "Sam O'Dell", inat_username = "n/a",
                                  specimen_collector = "", stringsAsFactors = FALSE))
  p <- participation_from_surveys(s, .people)
  expect_equal(sort(p$method[p$person_id == "p002" & p$year == 2021]), c("lethal", "non-lethal"))
})

test_that("duplicate surveys do not duplicate a person-year", {
  s <- rbind(.surveys, .surveys)
  expect_equal(nrow(participation_from_surveys(s, .people)),
               nrow(participation_from_surveys(.surveys, .people)))
})

test_that("an empty survey record yields an empty table, not an error", {
  p <- participation_from_surveys(.surveys[0, ], .people)
  expect_equal(nrow(p), 0)
  expect_equal(names(p), c("person_id", "year", "role", "method"))
})
