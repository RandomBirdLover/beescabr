library(testthat)
library(dplyr)

# inat_plant_clean.R mirrors inat_bee_clean.R but scoped to the surveyors' PLANT obs:
#  * flower_flowering is the ONE annotation (phenology value from the crosswalk field)
#  * taxonomy comes straight from the plant iNat export (taxon_*_name -> schema columns)
#  * ipc_norm_transect collapses transect variants exactly like the bee cleaner

test_that("ipc_flowering pulls the flowering phenology value from the crosswalk field", {
  src("observations/inat_plant_clean.R")
  cw <- tempfile(fileext = ".csv")
  readr::write_csv(tibble(name = "flower_flowering",
                          inat_field_variants = "flowering?; flowering",
                          inat_tag_variants = NA_character_), cw)
  ex <- tibble(id = c("1", "2"), tag_list = c(NA, NA),
               `field:flowering?` = c("Flowering", NA))
  out <- ipc_flowering(ex, cw)
  expect_equal(out$flower_flowering[out$obs_id == "1"], "Flowering")
  expect_true(is.na(out$flower_flowering[out$obs_id == "2"]))
  expect_equal(nrow(out), 2L)
})

test_that("ipc_flowering returns a blank column when the crosswalk is absent", {
  src("observations/inat_plant_clean.R")
  ex <- tibble(id = c("1", "2"), tag_list = c(NA, NA))
  out <- ipc_flowering(ex, tempfile(fileext = ".csv"))   # nonexistent path
  expect_true("flower_flowering" %in% names(out))
  expect_true(all(is.na(out$flower_flowering)))
})

test_that("ipc_taxonomy_from_export maps ranked export columns to the schema + epithet", {
  src("observations/inat_plant_clean.R")
  ex <- tibble(id = "1",
               scientific_name = "Encelia californica",
               common_name = "California Brittlebush",
               taxon_kingdom_name = "Plantae", taxon_family_name = "Asteraceae",
               taxon_genus_name = "Encelia", taxon_species_name = "Encelia californica")
  out <- ipc_taxonomy_from_export(ex)
  expect_equal(out$kingdom, "Plantae")
  expect_equal(out$family, "Asteraceae")
  expect_equal(out$genus, "Encelia")
  expect_equal(out$species, "californica")               # epithet split from the binomial
  expect_equal(out$scientific_name, "Encelia californica")
  expect_true(all(IPC_TAXONOMY_COLS %in% names(out)))    # full schema present, blanks where absent
})

test_that("ipc_norm_transect collapses transect variants like the bee cleaner", {
  src("observations/inat_plant_clean.R")
  expect_equal(ipc_norm_transect(c("TP1", "#tp2", "UPMON-A", "OT", "")),
               c("TP", "TP", "UPMON", "OT", NA))
})
