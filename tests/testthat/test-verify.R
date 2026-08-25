library(testthat)
library(dplyr)

holway <- function() tibble::tibble(
  source_sheet = c("Described", "Described", "Tentative"),
  genus   = c("Andrena", "Melissodes", "Andrena"),
  subgenus = c("Diandrena", "", "(Callandrena)"),
  species_raw = c("baeriae", "robustior", "CF annectens")
)

test_that("holway_name_sets builds lowercased genus/subgenus/species sets", {
  src("reference/verify.R")
  s <- holway_name_sets(holway())
  expect_true("andrena" %in% s$genus)
  expect_true("diandrena" %in% s$subgenus)          # parens-free
  expect_true("callandrena" %in% s$subgenus)        # parens stripped
  expect_true("melissodes robustior" %in% s$species)
  expect_true("andrena annectens" %in% s$species)   # CF qualifier stripped
})

test_that("flag_new_taxa flags taxa new-to-Holway at the right rank", {
  src("reference/verify.R")
  s <- holway_name_sets(holway())
  obs <- tibble::tibble(
    taxon_id = 1:4,
    genus      = c("Melissodes", "Agapostemon", "Andrena", "Andrena"),
    subgenus   = c(NA, NA, NA, NA),
    complex    = c(NA, NA, "Andrena complex", NA),
    species    = c("robustior", "subtilior", NA, "baeriae"),
    subspecies = c(NA, NA, NA, NA)
  )
  out <- flag_new_taxa(obs, s, verified_ids = integer(0))
  # Melissodes robustior is in Holway -> not new
  expect_false(out$needs_verification[1])
  # Agapostemon subtilior: genus new AND species new
  expect_true(out$needs_verification[2])
  expect_match(out$new_at_rank[2], "genus")
  expect_match(out$new_at_rank[2], "species")
  # complex present -> always new (Holway has no complexes)
  expect_true(out$needs_verification[3])
  expect_match(out$new_at_rank[3], "complex")
  # Andrena baeriae in Holway -> not new
  expect_false(out$needs_verification[4])
})

test_that("flag_new_taxa: a species carrying its parent complex is NOT new; a complex-rank row IS", {
  src("reference/verify.R")
  sets <- list(genus = "triepeolus", subgenus = "triepeolus",
               species = "triepeolus segregatus", subspecies = character(0))
  df <- tibble::tibble(
    taxon_id   = c(1L, 2L),
    genus      = "Triepeolus", subgenus = "Triepeolus", complex = "Triepeolus simplex",
    species    = c("segregatus", NA_character_), subspecies = NA_character_,
    rank       = c("species", "complex"))
  out <- flag_new_taxa(df, sets)
  expect_true(is.na(out$new_at_rank[1]))            # Holway species w/ parent complex -> NOT new
  expect_false(out$needs_verification[1])
  expect_match(out$new_at_rank[2], "complex")       # a real complex-rank row -> new
  expect_true(out$needs_verification[2])
})

test_that("flag_new_taxa respects the verified list", {
  src("reference/verify.R")
  s <- holway_name_sets(holway())
  obs <- tibble::tibble(taxon_id = 99, genus = "Agapostemon", subgenus = NA,
                        complex = NA, species = "subtilior", subspecies = NA)
  # unverified -> flagged
  expect_true(flag_new_taxa(obs, s, integer(0))$needs_verification)
  # once verified -> no longer flagged
  expect_false(flag_new_taxa(obs, s, verified_ids = 99L)$needs_verification)
})

test_that("Holway subspecies packed in species_raw are recognized, not flagged new", {
  src("reference/verify.R")
  holway <- tibble::tibble(genus = "Ashmeadiella", subgenus = "",
                           species_raw = "cactorum basalis")
  s <- holway_name_sets(holway)
  expect_true("ashmeadiella cactorum basalis" %in% s$subspecies)
  expect_true("ashmeadiella cactorum" %in% s$species)          # species-level implied
  obs <- tibble::tibble(taxon_id = 1, genus = "Ashmeadiella", subgenus = NA,
                        complex = NA, species = "cactorum", subspecies = "basalis")
  expect_false(flag_new_taxa(obs, s, integer(0))$needs_verification[1])
  # a subspecies Holway does NOT list is still flagged
  obs2 <- tibble::tibble(taxon_id = 2, genus = "Ashmeadiella", subgenus = NA,
                         complex = NA, species = "cactorum", subspecies = "nototherum")
  expect_true(flag_new_taxa(obs2, s, integer(0))$needs_verification[1])
})

test_that("load_verified_taxa returns ids, or empty when file absent", {
  src("reference/verify.R")
  expect_length(load_verified_taxa(tempfile()), 0)          # missing file
  p <- tempfile(fileext = ".csv")
  write.csv(data.frame(taxon_id = c(10, 20), verified = c("Y", "Y")), p, row.names = FALSE)
  expect_setequal(load_verified_taxa(p), c(10L, 20L))
})
