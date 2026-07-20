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

# --- normalized-tree builder (parent rows from the lookup) ------------------------------
mini_lookup <- function() tibble::tibble(
  taxon_id = c(10,11,12,13,14),
  rank = c("family","genus","species","genus","species"),
  scientific_name = c("Apidae","Bombus","Bombus vosnesenskii","Anthophora","Anthophora urbana"),
  common_name = c(NA,NA,"Yellow-faced Bumble Bee",NA,NA),
  order="Hymenoptera", family="Apidae",
  subfamily=c(NA,"Apinae","Apinae","Apinae","Apinae"),
  tribe=c(NA,"Bombini","Bombini","Anthophorini","Anthophorini"),
  genus=c(NA,"Bombus","Bombus","Anthophora","Anthophora"),
  subgenus=c(NA,NA,"Pyrobombus",NA,NA), complex=NA_character_,
  species=c(NA,NA,"vosnesenskii",NA,"urbana"), subspecies=NA_character_)

test_that("lookup_subtree includes the ancestor rows of present taxa, not unrelated ones", {
  src("checklists/checklist_build.R")
  lk <- mini_lookup()
  present <- lk |> filter(taxon_id == 12)                 # only Bombus vosnesenskii observed
  out <- suppressMessages(lookup_subtree(lk, present, "T"))
  expect_setequal(out$taxon_id, c(10,11,12))              # species + genus Bombus + family Apidae
  expect_false(any(out$taxon_id %in% c(13,14)))           # Anthophora branch excluded
  expect_true(all(c("taxon_id","taxon_rank","scientific_name","common_name",
                    "order","family","subfamily","tribe","genus","subgenus",
                    "complex","species","subspecies") %in% names(out)))
  expect_equal(out$taxon_rank[out$taxon_id==11], "genus") # parent row carries its own rank
})

test_that("lookup_subtree keeps a specimen-only leaf that isn't in the lookup (blank taxon_id)", {
  src("checklists/checklist_build.R")
  lk <- mini_lookup()
  present <- tibble::tibble(taxon_id=NA_integer_, rank="species",
    scientific_name="Colletes phaceliae", common_name=NA_character_, order="Hymenoptera",
    family="Colletidae", subfamily="Colletinae", tribe="Colletini",
    genus="Colletes", subgenus=NA_character_, complex=NA_character_,
    species="phaceliae", subspecies=NA_character_)
  out <- suppressMessages(lookup_subtree(lk, present, "spec"))
  expect_true("phaceliae" %in% out$species)               # specimen-only leaf kept
  expect_true(is.na(out$taxon_id[out$species=="phaceliae" & !is.na(out$species)][1]))
})

test_that("combine_checklists unions taxa with per-source boolean flags named after the sources", {
  src("checklists/checklist_build.R")
  lk <- mini_lookup()
  inat <- suppressMessages(lookup_subtree(lk, lk |> filter(taxon_id==12)))   # Bombus tree
  spec <- suppressMessages(lookup_subtree(lk, lk |> filter(taxon_id==14)))   # Anthophora tree
  comb <- combine_checklists(list(specimen=spec, inat=inat))
  expect_true(all(c("specimen","inat") %in% names(comb)))
  # family Apidae is in BOTH trees -> both TRUE
  ap <- comb[comb$taxon_rank=="family", ]
  expect_true(ap$specimen && ap$inat)
  # Bombus genus only in inat
  bb <- comb[comb$genus=="Bombus" & comb$taxon_rank=="genus", ]
  expect_true(bb$inat && !bb$specimen)
})
