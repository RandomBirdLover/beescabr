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

test_that("the output folder carries the season year", {
  expect_equal(beescabr_analysis_dir(2026L), "data/analysis/2026_generated")
  expect_equal(beescabr_analysis_dir(2027L), "data/analysis/2027_generated")
})

test_that("the report and the paper share one folder per season", {
  # they are two framings of the same year, not two analyses -- 11 scripts write to
  # both. Journal figures are told apart by "_journal" in the FILENAME, not by folder.
  expect_equal(beescabr_report_dir(2026L), beescabr_journal_dir(2026L))
  expect_equal(beescabr_report_dir(2026L), beescabr_analysis_dir(2026L))
})

test_that("the live constants track whatever season this run is", {
  # not pinned to 2026: that would fail the moment the calendar rolls over, which is
  # exactly the auto-advance this feature exists to provide.
  expect_equal(DIR_REPORT,  beescabr_report_dir(BEESCABR_SEASON))
  expect_equal(DIR_JOURNAL, beescabr_journal_dir(BEESCABR_SEASON))
  expect_true(grepl(as.character(BEESCABR_SEASON), DIR_REPORT, fixed = TRUE))
})
