library(testthat)
library(dplyr)

crosswalk_fx <- function() {
  tibble::tibble(
    type = c("tag", "tag", "tag", "tag", "field"),
    name = c("Cabrillo2021BeeSurvey", "CabrilloBee10MinuteSurvey", "UPMON", "iNatCollectionProject", "Interaction->Visited flower of"),
    category = c("Beeple", "General", "Transect", "Exclude", "obs_field"),
    inat_variants = c("Cabrillo2021; cabrillo2021beesurvey", NA, "UP MON", NA, NA)
  )
}

test_that("build_tag_map expands variants and drops the non-tag row", {
  src("clean/triage.R")
  tm <- build_tag_map(crosswalk_fx())
  expect_true("cabrillo2021" %in% tm$key)          # variant normalized
  expect_true("cabrillo2021beesurvey" %in% tm$key)
  expect_false(any(tm$category == "obs_field"))    # field row excluded
})

test_that("triage_from_tag_list marks a survey observation keep-worthy", {
  src("clean/triage.R")
  tm <- build_tag_map(crosswalk_fx())
  df <- tibble::tibble(obs_id = c(1L, 2L, 3L),
                       tag_list = c("Cabrillo2021BeeSurvey, UPMON",
                                    "iNatCollectionProject",
                                    NA))
  res <- triage_from_tag_list(df, tm)
  t <- res$triage
  expect_true(t$has_survey[t$obs_id == 1])
  expect_equal(t$transect[t$obs_id == 1], "UPMON")
  expect_equal(t$survey_year[t$obs_id == 1], "2021")
  expect_true(t$is_exclude[t$obs_id == 2])
  # obs 3 has no tags -> defaults
  expect_false(t$has_survey[t$obs_id == 3])
})

test_that("triage_from_tag_list reports unrecognized tags", {
  src("clean/triage.R")
  tm <- build_tag_map(crosswalk_fx())
  df <- tibble::tibble(obs_id = 1L, tag_list = "SomeTypoTag2099")
  res <- triage_from_tag_list(df, tm)
  expect_true("SomeTypoTag2099" %in% res$unknown$tag)
})

test_that("norm_key strips hash and lowercases", {
  src("clean/triage.R")
  expect_equal(norm_key("#UPMON "), "upmon")
})
