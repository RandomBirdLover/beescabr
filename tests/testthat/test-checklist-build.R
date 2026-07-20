library(testthat)
library(dplyr)

make_obs <- function() {
  tibble::tibble(
    taxon_id = c(1, 1, 2, 3, 4),
    scientific_name = c("Melissodes robustior", "Melissodes robustior",
                        "Andrena", "Halictidae", "Agapostemon texanus"),
    common_name = NA_character_,
    kingdom = "Animalia", phylum = "Arthropoda", class = "Insecta",
    order = "Hymenoptera", superfamily = "Apoidea",
    family = c("Apidae", "Apidae", "Andrenidae", "Halictidae", "Halictidae"),
    subfamily = NA_character_, tribe = NA_character_,
    genus = c("Melissodes", "Melissodes", "Andrena", NA, "Agapostemon"),
    species = c("Melissodes robustior", "Melissodes robustior", NA, NA, "Agapostemon texanus"),
    subspecies = NA_character_
  )
}

test_that("build_checklist dedupes by taxon_id and drops genus-less rows", {
  src("checklists/checklist_build.R")
  out <- suppressMessages(build_checklist(make_obs(), "T"))
  # taxon_id 1 deduped; taxon_id 3 (Halictidae, no genus) dropped
  expect_equal(sort(out$taxon_id), c(1, 2, 4))
  expect_false(any(is.na(out$genus)))
})

test_that("finalize_checklist parses species epithet and joins subgenus/complex", {
  src("checklists/checklist_build.R")
  cl <- suppressMessages(build_checklist(make_obs(), "T"))
  lookup <- tibble::tibble(
    taxon_id = c(1, 2, 4),
    subgenus = c("Melissodes", NA, NA),
    complex = c(NA, NA, "Agapostemon texanus"),
    complex_taxon_id = c(NA, NA, 999L)
  )
  fin <- finalize_checklist(cl, lookup)
  # species column reduced to epithet
  expect_equal(fin$species[fin$taxon_id == 1], "robustior")
  expect_equal(fin$species[fin$taxon_id == 4], "texanus")
  # genus-only row keeps NA species
  expect_true(is.na(fin$species[fin$taxon_id == 2]))
  expect_equal(fin$subgenus[fin$taxon_id == 1], "Melissodes")
})

test_that("taxonomy_lookup_from_bees derives the subgenus/complex map", {
  src("checklists/checklist_build.R")
  bees <- tibble::tibble(
    taxon_id = c(1, 1, 2),
    subgenus = c("Melissodes", "Melissodes", NA),
    complex = NA_character_,
    complex_taxon_id = NA_integer_
  )
  lk <- taxonomy_lookup_from_bees(bees)
  expect_equal(nrow(lk), 2)
  expect_equal(lk$subgenus[lk$taxon_id == 1], "Melissodes")
})
