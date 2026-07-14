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

  # specimen record: Melissodes robustior is in the museum collection
  spec <- tibble::tibble(genus = "melissodes", species = "robustior", has_cabr_specimen = TRUE)
  lookup <- build_bee_taxonomy_lookup(holway, checklist_sd, bees, specimen_species = spec)

  expect_true(all(c("genus","species","subgenus","family") %in% lookup$rank))
  # NEW: an iNat-only species (not in Holway) now appears
  expect_true("Agapostemon subtilior" %in% lookup$scientific_name)
  # metadata + membership columns, in order (common_name now right after scientific_name)
  expect_equal(names(lookup)[1:9],
               c("taxon_id","scientific_name","common_name","rank","verified","holway_status",
                 "in_holway","in_inat","in_cabr_specimens"))
  expect_true("subtribe" %in% names(lookup))

  mr <- filter(lookup, scientific_name == "Melissodes robustior")
  ag <- filter(lookup, scientific_name == "Agapostemon subtilior")
  # Holway species: verified, in Holway, on iNat, and has a specimen
  expect_true(mr$verified[1]); expect_true(mr$in_holway[1])
  expect_true(mr$in_inat[1]);  expect_true(mr$in_cabr_specimens[1])
  # iNat-only species: not verified, NOT in Holway, on iNat, no specimen
  expect_false(ag$verified[1]); expect_false(ag$in_holway[1])
  expect_true(ag$in_inat[1]);   expect_false(ag$in_cabr_specimens[1])
})

test_that("original CF/MSN qualifier is preserved in the species column but not in matching", {
  src("config.R")
  src("clean/verify.R")
  src("checklists/taxonomy_reference.R")

  holway <- tibble::tibble(
    source_sheet = c("Tentative", "Described"),
    family = c("Andrenidae", "Andrenidae"),
    subfamily = c("Andreninae", "Andreninae"),
    tribe = c("Andrenini", "Andrenini"),
    genus = c("Andrena", "Andrena"),
    subgenus = c("", ""),
    species_raw = c("CF annectens", "MSN baeriae")   # both carry qualifiers
  )
  # no iNat observations of these species
  checklist_sd <- tibble::tibble(
    taxon_id = integer(0), scientific_name = character(0), common_name = character(0),
    kingdom=character(0), phylum=character(0), class=character(0),
    order=character(0), superfamily=character(0),
    family=character(0), subfamily=character(0), tribe=character(0),
    genus=character(0), subgenus=character(0),
    complex=character(0), complex_taxon_id=integer(0),
    species=character(0), subspecies=character(0)
  )
  bees <- tibble::tibble(
    taxon_id = integer(0), scientific_name = character(0), common_name = character(0),
    genus = character(0), family = character(0),
    subfamily = character(0), tribe = character(0), subtribe = character(0)
  )
  lookup <- build_bee_taxonomy_lookup(holway, checklist_sd, bees)

  sp <- lookup$species[lookup$rank == "species"]
  # the ORIGINAL qualifier text survives in the species column
  expect_true("CF annectens" %in% sp)
  expect_true("MSN baeriae" %in% sp)
  # ...but matching still recognized them as Holway taxa (stripped), so in_holway TRUE
  cf <- lookup[lookup$rank == "species" & lookup$species == "CF annectens", ]
  expect_true(cf$in_holway[1])
})

test_that("merge_holway_resolved fills taxon_id + scientific_name on Holway rows only where missing", {
  src("config.R")
  src("clean/verify.R")
  src("checklists/taxonomy_reference.R")

  deduped <- tibble::tibble(
    taxon_id        = c(NA_integer_, 500L, NA_integer_),
    scientific_name = c(NA_character_, "Andrena baeriae", NA_character_),
    rank            = c("species", "species", "genus"),
    genus           = c("Andrena", "Andrena", "Colletes"),
    species         = c("annectens", "baeriae", NA_character_),
    subspecies      = NA_character_
  )
  holway_resolved <- tibble::tibble(
    taxon_id        = c(111L, 222L, 333L),
    scientific_name = c("Andrena annectens", "Andrena baeriae", "Colletes"),
    rank            = c("species", "species", "genus"),
    genus           = c("Andrena", "Andrena", "Colletes"),
    species         = c("annectens", "baeriae", NA_character_)
  )
  out <- merge_holway_resolved(deduped, holway_resolved)
  # unobserved Holway species gets a taxon_id + real binomial name
  ann <- out[out$species == "annectens" & !is.na(out$species), ]
  expect_equal(ann$taxon_id[1], 111L)
  expect_equal(ann$scientific_name[1], "Andrena annectens")
  # a row that already had a taxon_id keeps it (not overwritten)
  bae <- out[out$species == "baeriae" & !is.na(out$species), ]
  expect_equal(bae$taxon_id[1], 500L)
  # genus row filled from the genus-level entry
  col <- out[out$rank == "genus", ]
  expect_equal(col$taxon_id[1], 333L)
  expect_equal(col$scientific_name[1], "Colletes")
})

test_that("merge_holway_resolved is a no-op when the reference table is absent/empty", {
  src("checklists/taxonomy_reference.R")
  d <- tibble::tibble(taxon_id = NA_integer_, scientific_name = NA_character_,
                      rank = "species", genus = "Andrena", species = "annectens",
                      subspecies = NA_character_)
  expect_identical(merge_holway_resolved(d, NULL), d)
  expect_identical(merge_holway_resolved(d, d[0, ]), d)
})

test_that("enriched Holway ref fills taxon_id but does NOT flip in_inat for unobserved taxa", {
  src("config.R")
  src("clean/verify.R")
  src("checklists/taxonomy_reference.R")

  holway <- tibble::tibble(
    source_sheet = "Described",
    family = "Andrenidae", subfamily = "Andreninae", tribe = "Andrenini",
    genus = "Andrena", subgenus = "", species_raw = "annectens"
  )
  # NOT observed on iNat in SD County
  checklist_sd <- tibble::tibble(
    taxon_id = integer(0), scientific_name = character(0), common_name = character(0),
    kingdom=character(0), phylum=character(0), class=character(0),
    order=character(0), superfamily=character(0),
    family=character(0), subfamily=character(0), tribe=character(0),
    genus=character(0), subgenus=character(0),
    complex=character(0), complex_taxon_id=integer(0),
    species=character(0), subspecies=character(0))
  bees <- tibble::tibble(
    taxon_id = integer(0), scientific_name = character(0), common_name = character(0),
    genus = character(0), family = character(0),
    subfamily = character(0), tribe = character(0), subtribe = character(0))
  holway_resolved <- tibble::tibble(
    taxon_id = 111L, scientific_name = "Andrena annectens",
    rank = "species", genus = "Andrena", species = "annectens")

  lookup <- build_bee_taxonomy_lookup(holway, checklist_sd, bees,
                                      holway_resolved = holway_resolved)
  row <- lookup[lookup$rank == "species", ]
  expect_equal(row$taxon_id[1], 111L)               # filled from enriched table
  expect_equal(row$scientific_name[1], "Andrena annectens")
  expect_false(row$in_inat[1])                      # never observed -> still FALSE
  expect_true(row$in_holway[1])
})
