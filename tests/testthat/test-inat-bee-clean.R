library(testthat)
library(dplyr)

# ibc_fill_taxonomy(): PURE taxonomy fill for the clean iNat bee table -- joins the taxonomy
# lookup by taxon_id, fills scientific_name / common_name / kingdom..subspecies, and keeps the
# output schema stable (every IBC_TAXONOMY_COLS column present, blank when unmatched).

test_that("ibc_fill_taxonomy fills names/ids/taxonomy from the lookup by taxon_id", {
  src("observations/inat_bee_clean.R")
  df <- tibble(obs_id = c("a", "b", "c"), taxon_id = c("100", "200", "999"))
  lookup <- tibble(
    taxon_id        = c("100", "200"),
    scientific_name = c("Bombus vosnesenskii", "Apis mellifera"),
    common_name     = c("Yellow-faced Bumble Bee", "Western Honey Bee"),
    family          = c("Apidae", "Apidae"),
    genus           = c("Bombus", "Apis"),
    species         = c("vosnesenskii", "mellifera"))
  out <- ibc_fill_taxonomy(df, lookup)

  expect_equal(out$scientific_name[out$obs_id == "a"], "Bombus vosnesenskii")
  expect_equal(out$genus[out$obs_id == "b"], "Apis")
  expect_true(is.na(out$scientific_name[out$obs_id == "c"]))   # taxon_id 999 absent -> blank
  expect_true(all(IBC_TAXONOMY_COLS %in% names(out)))          # full schema present
  expect_equal(nrow(out), 3L)                                  # join must not multiply rows
})

test_that("ibc_fill_taxonomy is schema-stable when the lookup is NULL or empty", {
  src("observations/inat_bee_clean.R")
  out <- ibc_fill_taxonomy(tibble(obs_id = "a", taxon_id = "100"), NULL)
  expect_true(all(IBC_TAXONOMY_COLS %in% names(out)))
  expect_true(is.na(out$scientific_name))
})

test_that("ibc_fill_taxonomy replaces stale taxonomy columns without duplicating them", {
  src("observations/inat_bee_clean.R")
  df <- tibble(obs_id = "a", taxon_id = "100", scientific_name = "STALE", genus = "STALE")
  lookup <- tibble(taxon_id = "100", scientific_name = "Bombus vosnesenskii", genus = "Bombus")
  out <- ibc_fill_taxonomy(df, lookup)
  expect_equal(out$scientific_name, "Bombus vosnesenskii")    # replaced, not "STALE"
  expect_equal(sum(names(out) == "scientific_name"), 1L)      # no duplicate column
})

test_that("ibc_fill_taxonomy coerces integer taxon_id keys before joining", {
  src("observations/inat_bee_clean.R")
  df <- tibble(obs_id = "a", taxon_id = 100L)                 # integer key on the obs side
  lookup <- tibble(taxon_id = 100L, scientific_name = "Bombus vosnesenskii", genus = "Bombus")
  out <- ibc_fill_taxonomy(df, lookup)
  expect_equal(out$scientific_name, "Bombus vosnesenskii")
})

# ibc_bee_situation(): on_flower > on_ground > nest > missing. A recorded flower_visited
# PLANT counts as on_flower even when the bee_on_flower flag is FALSE (surveyors log the
# plant, not the flag). "missing" -> handed back to scientists to annotate.

test_that("ibc_bee_situation prioritizes, and a recorded plant counts as on_flower", {
  src("observations/inat_bee_clean.R")
  df <- tibble(
    bee_on_flower  = c(TRUE,  FALSE,                 FALSE, FALSE, FALSE, TRUE),
    flower_visited = c(NA,    "Encelia californica", NA,    NA,    NA,    NA),
    bee_on_ground  = c(FALSE, FALSE,                 TRUE,  FALSE, FALSE, TRUE),
    bee_nest       = c(FALSE, FALSE,                 FALSE, TRUE,  FALSE, FALSE),
    bee_in_nest    = c(FALSE, FALSE,                 FALSE, FALSE, FALSE, FALSE))
  expect_equal(ibc_bee_situation(df),
               c("on_flower",   # flag set
                 "on_flower",   # plant recorded, flag FALSE
                 "on_ground",
                 "nest",        # bee_nest
                 "missing",     # nothing recorded
                 "on_flower"))  # on_flower wins over on_ground
})

test_that("ibc_bee_situation counts bee_in_nest as nest and is NA/blank-safe", {
  src("observations/inat_bee_clean.R")
  df <- tibble(
    bee_on_flower  = c(NA,    FALSE),
    flower_visited = c("",    NA),          # "" is not a recorded plant
    bee_on_ground  = c(NA,    FALSE),
    bee_nest       = c(FALSE, NA),
    bee_in_nest    = c(FALSE, TRUE))
  expect_equal(ibc_bee_situation(df), c("missing", "nest"))
})

test_that("ibc_bee_situation returns 'missing' when the behavior columns are absent", {
  src("observations/inat_bee_clean.R")
  expect_equal(ibc_bee_situation(tibble(obs_id = c("1", "2"))), c("missing", "missing"))
})
