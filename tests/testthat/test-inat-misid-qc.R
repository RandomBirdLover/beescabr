library(testthat)
library(dplyr)

# misID QC: flag species-level iNat bee obs that are non-research-grade, unvouchered by a
# specimen, AND absent from Holway -> a review queue for a human to verify. Advisory only.

src("observations/qc/inat_misid_qc.R")

test_that("imq_norm / imq_binom normalize + roll to binomial", {
  expect_equal(imq_norm("  Bombus   VOSNESENSKII "), "bombus vosnesenskii")
  expect_equal(imq_binom("Bombus vosnesenskii vosnesenskii"), "bombus vosnesenskii")  # subspecies -> binomial
  expect_equal(imq_binom("Apis"), "apis")                                             # 1 word stays
})

test_that("imq_ref_set carries normalized names, rolled binomials, and ids", {
  ref <- imq_ref_set(tibble(scientific_name = c("Bombus vosnesenskii vosnesenskii", "Halictus tripartitus"),
                            taxon_id = c("100", "200")))
  expect_true("bombus vosnesenskii" %in% ref$names)   # rolled binomial of the subspecies
  expect_true("halictus tripartitus" %in% ref$names)
  expect_setequal(ref$ids, c("100", "200"))
})

test_that("imq_flag: species/subspecies + non-research + unvouchered + not-on-Holway only", {
  inat <- tibble(
    obs_id          = as.character(1:6),
    scientific_name = c("Andrena rara", "Bombus known", "Halictus vouched", "Genus only", "Andrena rara", "Andrena rara"),
    taxon_id        = c("11", "12", "13", "14", "11", "99"),
    taxon_rank      = c("species", "species", "species", "genus", "subspecies", "species"),
    quality_grade   = c("needs_id", "needs_id", "needs_id", "needs_id", "casual", "research"),
    observer = "x", observed_on = "2024-01-01", url = "u")
  spec <- imq_ref_set(tibble(scientific_name = "Halictus vouched", taxon_id = "13"))  # #3 vouchered
  hol  <- imq_ref_set(tibble(scientific_name = "Bombus known",     taxon_id = "12"))  # #2 on Holway
  out  <- imq_flag(inat, spec, hol)
  expect_setequal(out$obs_id, c("1", "5"))                       # species + subspecies, unvouched, not-holway
  expect_false(any(c("2", "3", "4", "6") %in% out$obs_id))       # holway / vouched / genus / research excluded
  expect_true(all(grepl("verify", out$reason)))
})

test_that("inat_misid_qc runs end-to-end and returns the queue columns", {
  d <- tempfile(fileext = ".csv")
  write.csv(tibble(obs_id = "1", observed_on = "2024", observer = "x", scientific_name = "Andrena rara",
                   taxon_id = "11", taxon_rank = "species", quality_grade = "needs_id", url = "u"),
            d, row.names = FALSE, na = "")
  out <- inat_misid_qc(inat_path = d, specimen_path = tempfile(fileext = ".csv"),
                       holway_path = tempfile(fileext = ".csv"), write = FALSE, verbose = FALSE)
  expect_equal(nrow(out), 1L)
  expect_setequal(names(out), IMQ_OUT_COLS)
})
