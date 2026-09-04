# A subspecies the lookup does not carry must be FLAGGED, so the prompt asks for its
# taxon_id and the answer is saved.
#
# It was not. compute_taxonomy_flags() tested genus, genus+species, complex and
# subgenus -- never subspecies. So 60 specimens of Colletes hyalinus gaudialis passed
# every check (the genus is known, the species is known), nothing was flagged, nobody
# was asked, and the iNaturalist id for that subspecies -- 345235, already sitting in
# the local taxon cache as a child of 217441 -- was never used.
src("specimens/specimen_clean_helpers.R")

tax <- data.frame(genus = "Colletes", species = "hyalinus", subgenus = "",
                  subspecies = "", complex = "", stringsAsFactors = FALSE)
inat <- data.frame(genus = character(), species = character(), stringsAsFactors = FALSE)

rows <- function(ss) data.frame(ucsd_id = "X1", genus = "Colletes", subgenus = "",
                                complex = "", species = "hyalinus", subspecies = ss,
                                stringsAsFactors = FALSE)

test_that("an unknown subspecies is flagged for review", {
  k <- build_known_names(tax, inat)
  out <- compute_taxonomy_flags(rows("gaudialis"), k$genera, k$genus_species,
                                k$subgenera, k$complexes, k$subspecies)
  expect_equal(nrow(out), 1)
  expect_match(out$flag_reason, "subspecies")
})

test_that("a KNOWN subspecies is not flagged", {
  tax2 <- rbind(tax, data.frame(genus = "Colletes", species = "hyalinus",
                                subgenus = "", subspecies = "gaudialis", complex = "",
                                stringsAsFactors = FALSE))
  k <- build_known_names(tax2, inat)
  out <- compute_taxonomy_flags(rows("gaudialis"), k$genera, k$genus_species,
                                k$subgenera, k$complexes, k$subspecies)
  expect_equal(nrow(out), 0)
})

test_that("a blank subspecies is not flagged", {
  k <- build_known_names(tax, inat)
  out <- compute_taxonomy_flags(rows(""), k$genera, k$genus_species,
                                k$subgenera, k$complexes, k$subspecies)
  expect_equal(nrow(out), 0)
})

test_that("build_known_names reports the subspecies the lookup knows", {
  tax2 <- rbind(tax, data.frame(genus = "Colletes", species = "hyalinus",
                                subgenus = "", subspecies = "gaudialis", complex = "",
                                stringsAsFactors = FALSE))
  k <- build_known_names(tax2, inat)
  expect_true("subspecies" %in% names(k))
  expect_equal(nrow(k$subspecies), 1)
})
