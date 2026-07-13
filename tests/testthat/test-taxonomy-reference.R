library(testthat)
library(dplyr)

test_that("build_bee_taxonomy_lookup emits genus + species + higher-rank rows", {
  src("config.R")
  src("clean/verify.R")
  src("checklists/taxonomy_reference.R")

  holway <- tibble::tibble(
    source_sheet = c("Described", "Described"),
    family = c("Apidae", "Apidae"),
    subfamily = c("Apinae", "Apinae"),
    tribe = c("Eucerini", "Eucerini"),
    genus = c("Melissodes", "Melissodes"),
    subgenus = c("", ""),
    species_raw = c("robustior", "")   # one species row, one genus-only
  )

  # taxon 1: identified to species. taxon 2: identified only to the subgenus
  # (species = NA) -- this is what makes a distinct subgenus row survive, just
  # like real iNat data where a subgenus row needs a subgenus-level obs.
  # taxon 3 = Agapostemon subtilior: OBSERVED on iNat, NOT in Holway.
  checklist_sd <- tibble::tibble(
    taxon_id = c(1L, 2L, 3L),
    scientific_name = c("Melissodes robustior", "Melissodes", "Agapostemon subtilior"),
    common_name = NA_character_,
    kingdom="Animalia", phylum="Arthropoda", class="Insecta",
    order="Hymenoptera", superfamily="Apoidea",
    family=c("Apidae","Apidae","Halictidae"), subfamily=c("Apinae","Apinae","Halictinae"),
    tribe=c("Eucerini","Eucerini","Halictini"),
    genus=c("Melissodes","Melissodes","Agapostemon"), subgenus=c("Melissodes","Melissodes",NA),
    complex=NA_character_, complex_taxon_id=NA_integer_,
    species=c("robustior", NA_character_, "subtilior"), subspecies=NA_character_
  )

  bees <- tibble::tibble(
    taxon_id = 99L, scientific_name = "Halictidae", common_name = NA_character_,
    genus = NA_character_, family = "Halictidae",
    subfamily = NA_character_, tribe = NA_character_, subtribe = NA_character_
  )

  lookup <- build_bee_taxonomy_lookup(holway, checklist_sd, bees)

  expect_true(all(c("genus","species","subgenus","family") %in% lookup$rank))
  # NEW: an iNat-only species (not in Holway) now appears
  expect_true("Agapostemon subtilior" %in% lookup$scientific_name)
  # metadata columns exist and are first
  expect_equal(names(lookup)[1:5], c("taxon_id","scientific_name","rank","verified","holway_status"))
  # subtribe sits between tribe and genus
  expect_true("subtribe" %in% names(lookup))
  # Holway species verified; iNat-only species not
  expect_true(filter(lookup, scientific_name == "Melissodes robustior")$verified[1])
  expect_false(filter(lookup, scientific_name == "Agapostemon subtilior")$verified[1])
  # Tentative/Unpublished tracking column present
  expect_true("holway_status" %in% names(lookup))
})
