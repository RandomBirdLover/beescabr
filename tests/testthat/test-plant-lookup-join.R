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

test_that("plant_name_parts splits a name into genus + full binomial (blank species for genus-only)", {
  p <- plant_name_parts(c("Encelia californica", "Madia", "Madia sp.",
                          "Isocoma menziesii sedoides", "Cactaceae", NA, ""))
  expect_equal(p$plant_genus,   c("Encelia", "Madia", "Madia", "Isocoma", NA, NA, NA))
  expect_equal(p$plant_species, c("Encelia californica", NA, NA, "Isocoma menziesii", NA, NA, NA))
})

test_that("attach_flower_ids adds plant_genus + full-binomial plant_species from the flower name", {
  lk <- tempfile(fileext = ".csv")
  write.csv(tibble(taxon_id = "101", scientific_name = "Acmispon glaber",
                   rank = "species", in_cabr_park_at_all = "TRUE"), lk, row.names = FALSE, na = "")
  out <- attach_flower_ids(tibble(flower_visited = c("Acmispon glaber", "Madia sp.", "Isocoma menziesii sedoides", NA)),
                           lookup_path = lk)
  expect_equal(out$plant_genus,   c("Acmispon", "Madia", "Isocoma", NA))
  expect_equal(out$plant_species, c("Acmispon glaber", NA, "Isocoma menziesii", NA))
})

test_that("attach_flower_ids adds NA columns when the lookup file is absent", {
  out <- attach_flower_ids(tibble(flower_visited = "x"), lookup_path = tempfile(fileext = ".csv"))
  expect_true(all(c("flower_taxon_id", "flower_in_park") %in% names(out)))
  expect_true(is.na(out$flower_taxon_id[1]))
})

# --- a taxon ABOVE genus is not a genus -------------------------------------
# plant_genus is word 1 of the flower name. The only rank guard was "ends in aceae",
# so "Angiospermae", "Faboideae", "Madieae", "Magnoliopsida" and nine others sailed
# through and sat in a column called plant_genus. The lookup already records each at
# its true rank with a BLANK genus, so the lookup settles it -- joined on taxon_id,
# never on the name we just split apart (CLAUDE.md, Taxon identity).
#
# The test is the lookup's GENUS COLUMN, not rank == "genus". Testing rank literally
# would blank every species-rank row too, and would throw away the section/subgenus
# names (Trachynia -> Brachypodium, Cepa -> Allium, Tenageia -> Juncus) whose genus
# the lookup already knows. The record is always kept; only the genus goes blank.
.plk <- function() {
  p <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    taxon_id        = c("11",       "12",       "13",         "14",       "15"),
    scientific_name = c("Encelia",  "Encelia californica", "Angiospermae", "Trachynia", "Madieae"),
    rank            = c("genus",    "species",  "subphylum",  "section",  "tribe"),
    genus           = c("Encelia",  "Encelia",  "",           "Brachypodium", ""),
    in_cabr_park_at_all = rep("TRUE", 5),
    stringsAsFactors = FALSE), p, row.names = FALSE)
  p
}

test_that("an above-genus taxon gets a blank plant_genus, and keeps its row", {
  src("reference/taxonomy/plant_lookup_join.R")
  out <- plant_name_parts(c("Angiospermae", "Madieae", "Encelia californica"),
                          taxon_id = c("13", "15", "12"), lookup_path = .plk())
  expect_equal(out$plant_genus, c(NA, NA, "Encelia"))
  expect_length(out$plant_genus, 3L)            # nothing dropped
})

test_that("a rank BELOW genus keeps the genus the lookup knows", {
  src("reference/taxonomy/plant_lookup_join.R")
  out <- plant_name_parts("Trachynia", taxon_id = "14", lookup_path = .plk())
  expect_false(is.na(out$plant_genus))          # a real plant, not junk
})

test_that("species and genus rows are untouched", {
  src("reference/taxonomy/plant_lookup_join.R")
  out <- plant_name_parts(c("Encelia", "Encelia californica"),
                          taxon_id = c("11", "12"), lookup_path = .plk())
  expect_equal(out$plant_genus, c("Encelia", "Encelia"))
  expect_equal(out$plant_species, c(NA, "Encelia californica"))
})

test_that("a blank or unknown id changes nothing (fail open)", {
  src("reference/taxonomy/plant_lookup_join.R")
  # blank id must NOT match the lookup's own blank-id rows -- CLAUDE.md rule 3
  out <- plant_name_parts(c("Solanum", "Salvia"), taxon_id = c("", "99999"),
                          lookup_path = .plk())
  expect_equal(out$plant_genus, c("Solanum", "Salvia"))
})

test_that("calling it the old way, with no id, behaves exactly as before", {
  src("reference/taxonomy/plant_lookup_join.R")
  expect_equal(plant_name_parts("Angiospermae")$plant_genus, "Angiospermae")
  expect_equal(plant_name_parts("Asteraceae")$plant_genus, NA_character_)
  expect_equal(plant_name_parts("Encelia californica")$plant_species, "Encelia californica")
})
