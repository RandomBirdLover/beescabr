library(testthat)
library(dplyr)

# bee_forage_names(): distinct flower_visited plant names (+ obs counts) from the
# flattened bee export, SCOPED to the CABR membership (bees inside the park), using
# the crosswalk flower-field coalesce. Non-plants (e.g. a butterfly mis-tagged in the
# flower field) pass through here; they're dropped later by the plant resolver.

src("observations/bee_forage.R")

.mk_export <- function(path) {
  ex <- data.frame(
    id = as.character(1:5),
    `field:interaction->visited flower of` = c("Encelia californica", "Euphorbia misera", "", "Apodemia virgulti", "Encelia californica"),
    `field:name of associated plant`       = c("", "", "Leptosyne maritima", "", ""),
    tag_list = "",
    check.names = FALSE, stringsAsFactors = FALSE)
  saveRDS(ex, path); path
}
.mk_cw <- function(path) {
  write.csv(data.frame(
    name = c("flower_visited", "bee_on_flower"),
    inat_field_variants = c("interaction->visited flower of; name of associated plant", ""),
    stringsAsFactors = FALSE),
    path, row.names = FALSE, na = ""); path
}
.mk_mem <- function(path, ids, status = "keep") {
  write.csv(data.frame(obs_id = as.character(ids), kind = "bee", status = status, stringsAsFactors = FALSE),
            path, row.names = FALSE, na = ""); path
}

test_that("bee_forage_names coalesces visited-plant fields into distinct plants + counts", {
  fg <- bee_forage_names(.mk_export(tempfile(fileext = ".rds")), .mk_cw(tempfile(fileext = ".csv")),
                         .mk_mem(tempfile(fileext = ".csv"), 1:5))
  expect_true(all(c("scientific_name", "n_obs") %in% names(fg)))
  expect_equal(fg$n_obs[fg$scientific_name == "Encelia californica"], 2L)   # primary field, twice
  expect_true("Leptosyne maritima" %in% fg$scientific_name)                 # filled from the secondary field
  expect_true("Apodemia virgulti"  %in% fg$scientific_name)                 # non-plant survives extraction
})

test_that("bee_forage_names scopes to CABR membership (drops out-of-park / excluded obs)", {
  ex <- .mk_export(tempfile(fileext = ".rds")); cw <- .mk_cw(tempfile(fileext = ".csv"))
  fg <- bee_forage_names(ex, cw, .mk_mem(tempfile(fileext = ".csv"), c(1, 2, 3)))  # keep obs 1-3 only
  expect_equal(fg$n_obs[fg$scientific_name == "Encelia californica"], 1L)   # obs 5 out of scope
  expect_false("Apodemia virgulti" %in% fg$scientific_name)                 # obs 4 out of scope
})

test_that("bee_forage_names returns empty when membership is missing (fail safe, no county-wide pull)", {
  ex <- .mk_export(tempfile(fileext = ".rds")); cw <- .mk_cw(tempfile(fileext = ".csv"))
  expect_equal(nrow(bee_forage_names(ex, cw, tempfile(fileext = ".csv"))), 0L)
})

test_that("bee_forage_names returns an empty frame when the export/crosswalk is missing", {
  expect_equal(nrow(bee_forage_names(tempfile(fileext = ".rds"), tempfile(fileext = ".csv"),
                                     .mk_mem(tempfile(fileext = ".csv"), 1:5))), 0L)
})
