# A specimen identified to a SUBSPECIES the lookup does not carry must still get
# its species (and its lineage). Before this, fill_coarse_ids() only rescued rows
# with a BLANK species, so a row like
#     genus Colletes | species hyalinus | subspecies gaudialis
# failed the exact three-key join, was skipped by the coarse fallback, and came
# out with no taxon_id and no lineage at all -- blank class, order, family.
# 60 of 980 specimen rows were in that state.
src("specimens/specimen_clean_helpers.R")

lookup <- data.frame(
  taxon_id = c(127741L, 217441L),
  rank     = c("genus", "species"),
  scientific_name = c("Colletes", "Colletes hyalinus"),
  phylum = "Arthropoda", class = "Insecta", order = "Hymenoptera",
  family = "Colletidae", subfamily = "Colletinae", tribe = "Colletini",
  genus = c("Colletes", "Colletes"), subgenus = c("", ""),
  species = c("", "hyalinus"), subspecies = c("", ""),
  stringsAsFactors = FALSE)

row_ss <- data.frame(genus = "Colletes", subgenus = "", complex = "(Complex) Colletes hyalinus",
                     species = "hyalinus", subspecies = "gaudialis",
                     class = NA_character_, order = NA_character_, family = NA_character_,
                     stringsAsFactors = FALSE)

test_that("a subspecies the lookup lacks falls back to its SPECIES, not to nothing", {
  out <- fill_coarse_ids(row_ss, lookup)
  expect_equal(out$taxon_id, 217441L)
  expect_equal(out$taxon_rank, "species")
})

test_that("the fallback fills the higher lineage that was blank", {
  out <- fill_coarse_ids(row_ss, lookup)
  expect_equal(out$class, "Insecta")
  expect_equal(out$order, "Hymenoptera")
  expect_equal(out$family, "Colletidae")
})

test_that("the recorded subspecies is kept -- the label still says what it says", {
  out <- fill_coarse_ids(row_ss, lookup)
  expect_equal(out$subspecies, "gaudialis")
})

test_that("a species-level row is still NOT demoted to the genus", {
  # the rule the earlier fix established and this change must not break: a specimen
  # identified to species is not made to look like a genus-level record
  r <- row_ss; r$species <- "nosuchspecies"; r$subspecies <- ""; r$complex <- ""
  expect_true(is.na(fill_coarse_ids(r, lookup)$taxon_id))
})

test_that("a below-species row whose species is also absent stays unresolved", {
  r <- row_ss; r$species <- "nosuchspecies"
  expect_true(is.na(fill_coarse_ids(r, lookup)$taxon_id))
})

test_that("a row that already resolved is left alone", {
  r <- row_ss; r$taxon_id <- 999L; r$taxon_rank <- "species"
  expect_equal(fill_coarse_ids(r, lookup)$taxon_id, 999L)
})
