library(testthat)
library(dplyr)

tier1 <- function() {
  tibble::tibble(
    taxon_id = 1:3,
    scientific_name = c("Melissodes robustior", "Andrena", "Agapostemon texanus"),
    common_name = NA_character_,
    kingdom="Animalia", phylum="Arthropoda", class="Insecta",
    order="Hymenoptera", superfamily="Apoidea",
    family = c("Apidae", "Andrenidae", "Halictidae"),
    subfamily = NA_character_, tribe = NA_character_,
    genus = c("Melissodes", "Andrena", "Agapostemon"),
    subgenus = NA_character_,
    complex = c(NA, NA, "Agapostemon texanus"),
    complex_taxon_id = c(NA, NA, 999L),
    species = c("robustior", NA, "texanus"),
    subspecies = NA_character_
  )
}

test_that("genus-only rows are kept in Tier 2", {
  src("checklists/tier2_merge.R")
  out <- suppressMessages(build_tier2_checklist(tier1(), NULL))
  expect_true("Andrena" %in% out$Genus)
  # the genus-only Andrena row has blank species
  andrena <- out |> filter(Genus == "Andrena")
  expect_true(is.na(andrena$Species))
})

test_that("Tier 2 without specimen evidence leaves Museum Collection blank", {
  # Contract the automated pipeline relies on: when the specimen clean file is
  # absent, build_tier2_checklist is called with NULL and must still produce a
  # valid Tier 2 (no specimen marks) rather than requiring specimen data.
  src("checklists/tier2_merge.R")
  out <- suppressMessages(build_tier2_checklist(tier1(), NULL))
  expect_true(all(is.na(out$`Museum Collection`)))
  expect_true(all(out$iNaturalist == "X"))
  expect_equal(nrow(out), 3)
})

test_that("complex values are prefixed and iNaturalist column always X", {
  src("checklists/tier2_merge.R")
  out <- suppressMessages(build_tier2_checklist(tier1(), NULL))
  agap <- out |> filter(Genus == "Agapostemon")
  expect_equal(agap$Complex, "(Complex) Agapostemon texanus")
  expect_true(all(out$iNaturalist == "X"))
})

test_that("specimen evidence marks Museum Collection without multiplying rows", {
  src("checklists/tier2_merge.R")
  specimens <- tibble::tibble(
    genus = c("Melissodes", "andrena"),          # mixed case on purpose
    species = c("robustior", NA)
  )
  spec_tbl <- specimen_species_table(specimens)
  out <- suppressMessages(build_tier2_checklist(tier1(), spec_tbl))
  # no row multiplication
  expect_equal(nrow(out), 3)
  # species-level specimen match
  mel <- out |> filter(Genus == "Melissodes")
  expect_equal(mel$`Museum Collection`, "X")
  # genus-only specimen matches the genus-only row
  andrena <- out |> filter(Genus == "Andrena")
  expect_equal(andrena$`Museum Collection`, "X")
  # Agapostemon has no specimen
  agap <- out |> filter(Genus == "Agapostemon")
  expect_true(is.na(agap$`Museum Collection`))
})

test_that("Holway cross-check flags absent species, blanks genus-only rows", {
  src("checklists/tier2_merge.R")
  keys <- c("melissodes_robustior")  # Agapostemon texanus intentionally absent
  out <- suppressMessages(build_tier2_checklist(tier1(), NULL, holway_keys = keys,
                                                run_holway_check = TRUE, label = "SD"))
  mel <- out |> filter(Genus == "Melissodes")
  agap <- out |> filter(Genus == "Agapostemon")
  andrena <- out |> filter(Genus == "Andrena")
  expect_equal(mel$`Found in Holway checklist?`, "Yes")
  expect_equal(agap$`Found in Holway checklist?`, "No")
  expect_true(is.na(andrena$`Found in Holway checklist?`))  # genus-only: not applicable
})

test_that("build_specimen_checklist keeps genus-required unique taxa", {
  src("checklists/tier2_merge.R")
  specimens <- tibble::tibble(
    order="Hymenoptera", family="Apidae", subfamily=NA, tribe=NA,
    genus=c("Melissodes","Melissodes",""), subgenus=NA,
    complex=NA, complex_taxon_id=NA, species=c("robustior","robustior",NA),
    subspecies=NA
  )
  out <- build_specimen_checklist(specimens)
  expect_equal(nrow(out), 1)  # dedup + drop blank genus
  expect_equal(out$genus, "Melissodes")
})
