library(testthat)
library(dplyr)

# drop_phantom_additions(): a curated specimen-addition is kept only while its taxon still has
# >= 1 record in the CURRENT cleaned specimen OR iNat table (matched by taxon_id OR scientific_name,
# the same rule the verify prompt uses). A row whose evidence vanished (e.g. a specimen re-ID'd away)
# is a PHANTOM and is skipped at build time -- WITHOUT editing specimen_additions.csv, so it silently
# reactivates if the bee is collected/photographed again.

# pre-source the guarded deps so taxonomy_reference.R's internal (cwd-relative) source() calls skip
src("reference/verify.R")
src("reference/holway.R")
src("reference/taxonomy_reference.R")

.adds <- data.frame(
  taxon_id        = c(100,             200,             300,               400),
  scientific_name = c("Genus phantom", "Genus refound", "Genus holwayonly", "Genus withinat"),
  rank            = "species", stringsAsFactors = FALSE)
.spec <- data.frame(taxon_id = 200L, scientific_name = "Genus refound", stringsAsFactors = FALSE)
.inat <- data.frame(taxon_id = 400L, scientific_name = "Genus withinat",
                    quality_grade = "research", stringsAsFactors = FALSE)

test_that("phantom additions (no current specimen or iNat evidence) are dropped", {
  kept <- drop_phantom_additions(.adds, .spec, .inat)$taxon_id
  expect_false(100 %in% kept)   # never found now -> phantom
  expect_false(300 %in% kept)   # if Holway later lists it, Holway carries it; the addition is dropped
})

test_that("additions with live specimen or iNat evidence are kept", {
  kept <- drop_phantom_additions(.adds, .spec, .inat)$taxon_id
  expect_true(200 %in% kept)    # re-found at CABR (a specimen)
  expect_true(400 %in% kept)    # has an iNat record
})

test_that("evidence matches by scientific_name when taxon_id is absent", {
  a2 <- data.frame(scientific_name = c("Genus refound", "Genus phantom"),
                   rank = "species", stringsAsFactors = FALSE)
  kept <- drop_phantom_additions(a2, .spec, NULL)$scientific_name
  expect_true("Genus refound" %in% kept)
  expect_false("Genus phantom" %in% kept)
})

test_that("missing / empty cleaned tables drop everything and never error", {
  expect_equal(nrow(drop_phantom_additions(.adds, NULL, NULL)), 0L)
  expect_equal(nrow(drop_phantom_additions(.adds, .spec[0, ], .inat[0, ])), 0L)
})

test_that("empty or NULL additions pass through unchanged", {
  expect_equal(nrow(drop_phantom_additions(.adds[0, , drop = FALSE], .spec, .inat)), 0L)
  expect_null(drop_phantom_additions(NULL, .spec, .inat))
})

# apply_verified_ids(): rows appended AFTER build_bee_taxonomy_lookup (the specimen-additions merge)
# miss the verified_taxa.csv memory pass -- re-applying it keeps a verified addition from re-asking
# in the pass-2 prompt every run.
test_that("apply_verified_ids flips remembered taxa TRUE and touches nothing else", {
  lk <- data.frame(taxon_id = c(747170, 999, NA),
                   verified = c(FALSE, FALSE, FALSE), stringsAsFactors = FALSE)
  r <- apply_verified_ids(lk, c(747170L))
  expect_true(r$verified[1])                       # remembered -> verified
  expect_false(r$verified[2])                      # not remembered -> unchanged
  expect_false(r$verified[3])                      # NA taxon_id -> unchanged, no error
})

test_that("apply_verified_ids never un-verifies and survives empty input", {
  lk <- data.frame(taxon_id = 1, verified = TRUE, stringsAsFactors = FALSE)
  expect_true(apply_verified_ids(lk, integer(0))$verified)
  expect_equal(nrow(apply_verified_ids(lk[0, ], c(1L))), 0L)
  expect_null(apply_verified_ids(NULL, c(1L)))
})
