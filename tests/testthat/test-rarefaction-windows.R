# Each journal comparison needs its OWN window of records, and the window is the
# whole reason the comparison is or is not valid -- so it lives beside the name
# rather than inline in two scripts that can drift.
source(file.path("..", "..", "scripts", "analysis", "rarefaction_names.R"))

rec <- expand.grid(year = 2021:2025, month = 1:12,
                   surveyor = c("beeple", "intern"), method = c("lethal", "nonlethal"),
                   stringsAsFactors = FALSE)
rec$obs_type <- ifelse(rec$method == "lethal", "specimen", "observation")

test_that("each comparison declares its own folder", {
  expect_equal(rare_window_dir("by_method"),   "fair_method_2021_2023")
  expect_equal(rare_window_dir("by_observer"), "fair_observer_2024")
  expect_null(rare_window_dir("by_year"))
})

test_that("the method window is Mar-Oct 2021-2023 and keeps BOTH methods", {
  w <- rare_window_records(rec, "by_method")
  expect_setequal(unique(w$year), 2021:2023)
  expect_setequal(unique(w$month), 3:10)
  expect_setequal(unique(w$method), c("lethal", "nonlethal"))
})

test_that("the observer window is May-Sep 2024 and holds METHOD CONSTANT", {
  # this is the whole point of the 2024 window: both groups photographing, so a
  # difference is about the observers rather than about netting vs photographing
  w <- rare_window_records(rec, "by_observer")
  expect_equal(unique(w$year), 2024)
  expect_setequal(unique(w$month), 5:9)
  expect_equal(unique(w$method), "nonlethal")
  expect_setequal(unique(w$surveyor), c("beeple", "intern"))
})

test_that("the grouping column and its levels come with the window", {
  expect_equal(rare_window("by_method")$group, "obs_type")
  expect_equal(rare_window("by_method")$levels, c("observation", "specimen"))
  expect_equal(rare_window("by_observer")$group, "surveyor")
  expect_equal(rare_window("by_observer")$levels, c("beeple", "intern"))
})

test_that("a report dimension has no window and is refused", {
  expect_null(rare_window("by_transect"))
  expect_error(rare_window_records(rec, "by_transect"), "no window")
})
