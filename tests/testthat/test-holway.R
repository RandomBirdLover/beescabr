library(testthat)
library(dplyr)

test_that("clean_holway_species strips CF/MSN/aff./sp.nov qualifiers", {
  src("reference/holway.R")
  expect_equal(clean_holway_species("CF annectens"), "annectens")
  expect_equal(clean_holway_species("MSN foo"), "foo")
  expect_equal(clean_holway_species("aff. miserabilis sp. nov."), "miserabilis")
  expect_equal(clean_holway_species("AFF salicicola"), "salicicola")   # uppercase AFF, no period
  expect_equal(clean_holway_species("bar sp. nov."), "bar")
  expect_equal(clean_holway_species("plain"), "plain")
  expect_equal(clean_holway_species("affinis"), "affinis")             # real epithet, NOT the marker
})

test_that("holway_qualifier extracts the CF/MSN/aff./sp. nov. marker", {
  src("reference/holway.R")
  expect_equal(holway_qualifier("CF annectens"), "CF")
  expect_equal(holway_qualifier("MSN pilosifrons"), "MSN")
  expect_equal(holway_qualifier("MSN atripes sp. nov."), "MSN sp. nov.")
  expect_equal(holway_qualifier("aff. miserabilis sp. nov."), "aff. sp. nov.")
  expect_equal(holway_qualifier("AFF salicicola"), "aff.")     # uppercase AFF normalized to aff.
  expect_equal(holway_qualifier("AFF hurdi"), "aff.")
  expect_equal(holway_qualifier("sp. nov."), "sp. nov.")
  expect_true(is.na(holway_qualifier("robustior")))            # plain name
  expect_true(is.na(holway_qualifier("affinis")))              # real epithet, NOT the aff. marker
  expect_true(is.na(holway_qualifier("californicus / fervidus"))) # slash pair, no marker
  # vectorized
  expect_equal(holway_qualifier(c("CF a", "plain", NA)), c("CF", NA, NA))
})

test_that("split_holway_species separates the packed species + subspecies", {
  src("reference/holway.R")
  out <- split_holway_species(c("copelandica albomarginata", "annectens",
                                "CF baeriae", "", NA_character_))
  expect_equal(out$species,    c("copelandica", "annectens", "baeriae", NA, NA))
  expect_equal(out$subspecies, c("albomarginata", NA, NA, NA, NA))
})

test_that("split_holway_species treats a slash pair as species, never subspecies", {
  src("reference/holway.R")
  out <- split_holway_species(c("californicus / fervidus", "sonorus / sonorus"))
  expect_equal(out$species,    c("californicus", "sonorus"))
  expect_equal(out$subspecies, c(NA_character_, NA_character_))  # "/ B" is NOT a subspecies
})

test_that("holway_genus_taxonomy collapses conflicting tribes to first alphabetical", {
  src("reference/holway.R")
  holway <- tibble::tibble(
    genus     = c("Protandrena", "Protandrena", "Andrena"),
    family    = c("Andrenidae", "Andrenidae", "Andrenidae"),
    subfamily = c("Panurginae", "Panurginae", "Andreninae"),
    tribe     = c("Protandrenini", "Panurgini", "Andrenini"),
    species_raw = c("a", "b", "c")
  )
  out <- suppressMessages(holway_genus_taxonomy(holway))
  # one row per genus
  expect_equal(nrow(out), 2)
  prot <- out |> filter(genus == "Protandrena")
  # alphabetical first tribe wins: Panurgini < Protandrenini
  expect_equal(prot$tribe_holway, "Panurgini")
})

test_that("backfill_taxonomy fills blank tribe from Holway, keeps iNat when present", {
  src("reference/holway.R")
  genus_lookup <- tibble::tibble(
    genus = "Colletes", family_holway = "Colletidae",
    subfamily_holway = "Colletinae", tribe_holway = "Colletini"
  )
  bees <- tibble::tibble(
    genus = c("Colletes", "Andrena"),
    family = c("Colletidae", "Andrenidae"),
    subfamily = c("", "Andreninae"),
    tribe = c("", "Andrenini")   # Colletes blank -> filled; Andrena present -> kept
  )
  out <- backfill_taxonomy(bees, genus_lookup)
  expect_equal(out$tribe[out$genus == "Colletes"], "Colletini")
  expect_equal(out$tribe[out$genus == "Andrena"], "Andrenini")
})

test_that("holway_match_keys builds lowercased genus_species keys", {
  src("reference/holway.R")
  holway <- tibble::tibble(genus = c("Andrena", "Colletes"),
                           species_raw = c("CF annectens", "hyalinus"))
  keys <- holway_match_keys(holway)
  expect_true("andrena_annectens" %in% keys)
  expect_true("colletes_hyalinus" %in% keys)
})

test_that("holway_match_keys keys on species only and expands slash pairs", {
  src("reference/holway.R")
  holway <- tibble::tibble(
    genus       = c("Ashmeadiella", "Bombus"),
    species_raw = c("cactorum basalis", "californicus / fervidus"))
  keys <- holway_match_keys(holway)
  expect_true("ashmeadiella_cactorum" %in% keys)                # subspecies dropped -> matches species row
  expect_false("ashmeadiella_cactorum basalis" %in% keys)       # the packed form is gone
  expect_true(all(c("bombus_californicus", "bombus_fervidus") %in% keys))  # both slash names
})
