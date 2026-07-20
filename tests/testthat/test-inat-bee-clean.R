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
