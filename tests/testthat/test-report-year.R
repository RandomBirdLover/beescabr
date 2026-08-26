# The report/journal output folders are stamped with a SEASON YEAR so each year's
# deliverables stay frozen as the record of what was reported. 2026 is Brandi's
# season; a new year should not require editing code.

src("config.R")

test_that("the season year defaults to the current calendar year", {
  expect_equal(beescabr_season_year(today = as.Date("2027-04-18")), 2027L)
  expect_equal(beescabr_season_year(today = as.Date("2026-12-31")), 2026L)
})

test_that("an explicit override wins over the calendar", {
  expect_equal(beescabr_season_year(override = "2026", today = as.Date("2029-01-01")), 2026L)
})

test_that("a nonsense override is ignored rather than trusted", {
  expect_equal(beescabr_season_year(override = "banana", today = as.Date("2026-05-01")), 2026L)
  expect_equal(beescabr_season_year(override = "",       today = as.Date("2026-05-01")), 2026L)
})

test_that("the output folders carry the season year", {
  expect_equal(beescabr_report_dir(2026L),  "data/analysis/nps_report_2026")
  expect_equal(beescabr_journal_dir(2026L), "data/analysis/journal_paper_2026")
  expect_equal(beescabr_report_dir(2027L),  "data/analysis/nps_report_2027")
})

test_that("the live constants track whatever season this run is", {
  # not pinned to 2026: that would fail the moment the calendar rolls over, which is
  # exactly the auto-advance this feature exists to provide.
  expect_equal(DIR_REPORT,  beescabr_report_dir(BEESCABR_SEASON))
  expect_equal(DIR_JOURNAL, beescabr_journal_dir(BEESCABR_SEASON))
  expect_true(grepl(as.character(BEESCABR_SEASON), DIR_REPORT, fixed = TRUE))
})
