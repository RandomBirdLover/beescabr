library(testthat)
library(dplyr)

test_that("build_bee_taxonomy_lookup emits genus + species + higher-rank rows", {
  src("checklists/taxonomy_reference.R")

  holway <- tibble::tibble(
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
  checklist_sd <- tibble::tibble(
    taxon_id = c(1L, 2L),
    scientific_name = c("Melissodes robustior", "Melissodes"), common_name = NA_character_,
    kingdom="Animalia", phylum="Arthropoda", class="Insecta",
    order="Hymenoptera", superfamily="Apoidea",
    family="Apidae", subfamily="Apinae", tribe="Eucerini",
    genus="Melissodes", subgenus="Melissodes",
    complex=NA_character_, complex_taxon_id=NA_integer_,
    species=c("robustior", NA_character_), subspecies=NA_character_
  )

  bees <- tibble::tibble(
    taxon_id = 99L, scientific_name = "Halictidae", common_name = NA_character_,
    genus = NA_character_, family = "Halictidae",
    subfamily = NA_character_, tribe = NA_character_
  )

  lookup <- build_bee_taxonomy_lookup(holway, checklist_sd, bees)

  expect_true("genus" %in% lookup$rank)
  expect_true("species" %in% lookup$rank)
  expect_true("subgenus" %in% lookup$rank)
  # higher-rank family row from bees (genus blank)
  expect_true("family" %in% lookup$rank)
  # the species row carries a taxon_id from the iNat checklist join
  sp <- lookup |> filter(rank == "species", genus == "Melissodes")
  expect_equal(sp$taxon_id[1], 1L)
})
