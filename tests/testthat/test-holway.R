library(testthat)
library(dplyr)

test_that("clean_holway_species strips CF/MSN/sp.nov qualifiers", {
  src("checklists/holway.R")
  expect_equal(clean_holway_species("CF annectens"), "annectens")
  expect_equal(clean_holway_species("MSN foo"), "foo")
  expect_equal(clean_holway_species("bar sp. nov."), "bar")
  expect_equal(clean_holway_species("plain"), "plain")
})

test_that("split_holway_species separates the packed species + subspecies", {
  src("checklists/holway.R")
  out <- split_holway_species(c("copelandica albomarginata", "annectens",
                                "CF baeriae", "", NA_character_))
  expect_equal(out$species,    c("copelandica", "annectens", "baeriae", NA, NA))
  expect_equal(out$subspecies, c("albomarginata", NA, NA, NA, NA))
})

test_that("holway_genus_taxonomy collapses conflicting tribes to first alphabetical", {
  src("checklists/holway.R")
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
  src("checklists/holway.R")
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
  src("checklists/holway.R")
  holway <- tibble::tibble(genus = c("Andrena", "Colletes"),
                           species_raw = c("CF annectens", "hyalinus"))
  keys <- holway_match_keys(holway)
  expect_true("andrena_annectens" %in% keys)
  expect_true("colletes_hyalinus" %in% keys)
})
