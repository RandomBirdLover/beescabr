library(testthat)
library(dplyr)
library(readr)

# attach_flower_ids(): join a table's flower_visited (plant name) to the plant
# taxonomy lookup, adding flower_taxon_id + flower_in_park. Used by both the bee
# and specimen cleaners so every record's flower carries its plant id + in-park flag.

src("reference/plant_lookup_join.R")

test_that("attach_flower_ids maps flower name -> taxon_id + in_park, NA when unmatched", {
  lk <- tempfile(fileext = ".csv")
  write.csv(tibble(taxon_id = c("101", "201"), scientific_name = c("Acmispon glaber", "Madia"),
                   rank = c("species", "genus"), in_cabr_park_at_all = c("TRUE", "FALSE")),
            lk, row.names = FALSE, na = "")
  df  <- tibble(flower_visited = c("Acmispon glaber", "Madia", "Nonexistent plant", NA))
  out <- attach_flower_ids(df, lookup_path = lk)
  expect_equal(out$flower_taxon_id, c("101", "201", NA, NA))
  expect_equal(out$flower_in_park,  c(TRUE, FALSE, NA, NA))
})

test_that("attach_flower_ids matches case-insensitively and prefers the species row", {
  lk <- tempfile(fileext = ".csv")
  write.csv(tibble(taxon_id = c("9", "10"), scientific_name = c("Encelia", "encelia californica"),
                   rank = c("genus", "species"), in_cabr_park_at_all = c("TRUE", "TRUE")),
            lk, row.names = FALSE, na = "")
  out <- attach_flower_ids(tibble(flower_visited = c("Encelia californica")), lookup_path = lk)
  expect_equal(out$flower_taxon_id, "10")   # species row, case-insensitive
})

test_that("attach_flower_ids folds a subspecies flower onto its species row", {
  lk <- tempfile(fileext = ".csv")
  write.csv(tibble(taxon_id = "601", scientific_name = "Isocoma menziesii",
                   rank = "species", in_cabr_park_at_all = "TRUE"),
            lk, row.names = FALSE, na = "")
  out <- attach_flower_ids(tibble(flower_visited = "Isocoma menziesii sedoides"), lookup_path = lk)
  expect_equal(out$flower_taxon_id, "601")   # trinomial rolled up to its species row
  expect_true(out$flower_in_park)
})

test_that("attach_flower_ids adds NA columns when the lookup file is absent", {
  out <- attach_flower_ids(tibble(flower_visited = "x"), lookup_path = tempfile(fileext = ".csv"))
  expect_true(all(c("flower_taxon_id", "flower_in_park") %in% names(out)))
  expect_true(is.na(out$flower_taxon_id[1]))
})
