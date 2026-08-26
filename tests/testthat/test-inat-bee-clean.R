library(testthat)
library(dplyr)

# ibc_fill_taxonomy(): PURE taxonomy fill for the clean iNat bee table -- joins the taxonomy
# lookup by taxon_id, fills scientific_name / common_name / kingdom..subspecies, and keeps the
# output schema stable (every IBC_TAXONOMY_COLS column present, blank when unmatched).

test_that("ibc_fill_taxonomy fills names/ids/taxonomy from the lookup by taxon_id", {
  src("inat_observations/inat_bee_clean.R")
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
  src("inat_observations/inat_bee_clean.R")
  out <- ibc_fill_taxonomy(tibble(obs_id = "a", taxon_id = "100"), NULL)
  expect_true(all(IBC_TAXONOMY_COLS %in% names(out)))
  expect_true(is.na(out$scientific_name))
})

test_that("ibc_fill_taxonomy replaces stale taxonomy columns without duplicating them", {
  src("inat_observations/inat_bee_clean.R")
  df <- tibble(obs_id = "a", taxon_id = "100", scientific_name = "STALE", genus = "STALE")
  lookup <- tibble(taxon_id = "100", scientific_name = "Bombus vosnesenskii", genus = "Bombus")
  out <- ibc_fill_taxonomy(df, lookup)
  expect_equal(out$scientific_name, "Bombus vosnesenskii")    # replaced, not "STALE"
  expect_equal(sum(names(out) == "scientific_name"), 1L)      # no duplicate column
})

test_that("ibc_fill_taxonomy coerces integer taxon_id keys before joining", {
  src("inat_observations/inat_bee_clean.R")
  df <- tibble(obs_id = "a", taxon_id = 100L)                 # integer key on the obs side
  lookup <- tibble(taxon_id = 100L, scientific_name = "Bombus vosnesenskii", genus = "Bombus")
  out <- ibc_fill_taxonomy(df, lookup)
  expect_equal(out$scientific_name, "Bombus vosnesenskii")
})

# ibc_bee_situation(): on_flower > on_ground > nest > missing. A recorded flower_visited
# PLANT counts as on_flower even when the bee_on_flower flag is FALSE (surveyors log the
# plant, not the flag). "missing" -> handed back to scientists to annotate.

test_that("ibc_bee_situation prioritizes, and a recorded plant counts as on_flower", {
  src("inat_observations/inat_bee_clean.R")
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
  src("inat_observations/inat_bee_clean.R")
  df <- tibble(
    bee_on_flower  = c(NA,    FALSE),
    flower_visited = c("",    NA),          # "" is not a recorded plant
    bee_on_ground  = c(NA,    FALSE),
    bee_nest       = c(FALSE, NA),
    bee_in_nest    = c(FALSE, TRUE))
  expect_equal(ibc_bee_situation(df), c("missing", "nest"))
})

test_that("ibc_bee_situation returns 'missing' when the behavior columns are absent", {
  src("inat_observations/inat_bee_clean.R")
  expect_equal(ibc_bee_situation(tibble(obs_id = c("1", "2"))), c("missing", "missing"))
})

# ibc_fix_behavior(): hand-back worklist -- a flower_visited with no flower_taxon_id
# (non-plant/typo, e.g. a butterfly mis-tagged as a flower) OR a survey obs that
# recorded no behavior at all.

test_that("ibc_fix_behavior flags a non-plant flower and a survey obs missing all fields", {
  src("inat_observations/inat_bee_clean.R")
  clean <- tibble(
    obs_id          = c("a", "b", "c", "d"),
    is_survey       = c(TRUE, TRUE, FALSE, TRUE),
    bee_situation   = c("on_flower", "missing", "on_flower", "on_flower"),
    flower_visited  = c("Apodemia virgulti", NA, "Encelia californica", "Encelia californica"),
    flower_taxon_id = c(NA, NA, "106", "106"),
    # plant_genus matters: a flower that resolves to a taxon but carries NO genus is
    # flagged flower_not_to_genus (the Diadasia-on-"cactus" case). c and d are meant
    # to be GOOD records, so they must carry a genus.
    plant_genus     = c(NA, NA, "Encelia", "Encelia"),
    url             = "u")
  fx <- ibc_fix_behavior(clean)
  expect_equal(fx$fix_reason[fx$obs_id == "a"], "flower_not_a_plant_or_unresolved")  # butterfly tag
  expect_equal(fx$fix_reason[fx$obs_id == "b"], "missing_all_behavior_fields")       # survey, nothing recorded
  expect_false(any(fx$obs_id %in% c("c", "d")))                                       # good flowers -> not flagged
  expect_true("flower_visited" %in% names(fx))                                        # the bad value is shown for the fixer
})

test_that("ibc_fix_behavior is column-safe when optional columns are absent", {
  src("inat_observations/inat_bee_clean.R")
  expect_equal(nrow(ibc_fix_behavior(tibble(obs_id = c("x", "y")))), 0L)
})

# location_needs_fix moved OUT of the clean table into a review worklist (ibc_location_review),
# mirroring the specimen side. The flag must NOT be a clean-table column any more.
test_that("location_needs_fix is not a clean-table column, and ibc_location_review lists bad pins", {
  src("inat_observations/inat_bee_clean.R")
  expect_false("location_needs_fix" %in% IBC_COLUMN_ORDER)      # dropped from the clean schema
  df <- tibble(
    obs_id = c("a", "b", "c"), observer = "x", observed_on = "2024-01-01", transect = "TP",
    taxon_id = c("1","2","3"), scientific_name = "Bombus sp", latitude = 1, longitude = 2, url = "u",
    location_needs_fix = c(TRUE, FALSE, TRUE))
  rev <- ibc_location_review(df)
  expect_setequal(rev$obs_id, c("a", "c"))                      # only the flagged pins
  expect_true(all(grepl("check the pin", rev$fix_reason)))
  expect_false("location_needs_fix" %in% names(rev))            # the flag itself isn't carried
  expect_equal(nrow(ibc_location_review(df[df$location_needs_fix == FALSE, ])), 0)  # none flagged -> empty
})
