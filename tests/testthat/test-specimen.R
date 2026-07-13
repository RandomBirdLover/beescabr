library(testthat)
library(dplyr)

test_that("standardize_specimen_names fixes casing", {
  src("clean/specimen_clean.R")
  df <- tibble::tibble(genus = c("ANDRENA", "melissodes"),
                       species = c("Robustior", "SUBTILIOR"),
                       subspecies = c(NA, "FOO"))
  out <- standardize_specimen_names(df)
  expect_equal(out$genus, c("Andrena", "Melissodes"))
  expect_equal(out$species, c("robustior", "subtilior"))
  expect_equal(out$subspecies, c(NA, "foo"))
})

test_that("fill_specimen_taxonomy coalesces blanks from the lookup only", {
  src("clean/specimen_clean.R")
  df <- tibble::tibble(genus = "Colletes", species = "hyalinus", subspecies = NA_character_,
                       family = "", subfamily = NA_character_, tribe = "KeepMe")
  lk <- tibble::tibble(genus = "Colletes", species = "hyalinus", subspecies = NA_character_,
                       family = "Colletidae", subfamily = "Colletinae", tribe = "Colletini")
  out <- fill_specimen_taxonomy(df, lk)
  expect_equal(out$family, "Colletidae")     # blank filled
  expect_equal(out$subfamily, "Colletinae")  # NA filled
  expect_equal(out$tribe, "KeepMe")          # present value kept
})

test_that("compute_taxonomy_flags flags unknown genus and unknown genus+species", {
  src("clean/specimen_clean.R")
  df <- tibble::tibble(
    ucsd_id = 1:3, sdnhm_id = 0,
    genus = c("Andrena", "Augochorella", "Andrena"),   # Augochorella = typo
    species = c("baeriae", "pomoniella", "notaspecies"),
    subspecies = NA_character_)
  known_genera <- c("Andrena")
  known_gs <- tibble::tibble(genus = "Andrena", species = "baeriae")
  flags <- compute_taxonomy_flags(df, known_genera, known_gs)
  expect_true("Augochorella" %in% flags$genus)
  expect_true(any(flags$flag_reason == "genus not in taxonomy lookup"))
  expect_true(any(flags$flag_reason == "genus+species combo not in taxonomy lookup"))
  # Andrena baeriae is known -> not flagged
  expect_false(any(flags$genus == "Andrena" & flags$species == "baeriae"))
})

test_that("add_qc_flags marks missing fields", {
  src("clean/specimen_clean.R")
  df <- tibble::tibble(latitude = c(1, NA), longitude = c(1, 2), date = c(Sys.Date(), NA),
                       sdnhm_id = c("A", ""), ucsd_id = c("U", "V"), genus = c("Andrena", ""))
  out <- add_qc_flags(df)
  expect_equal(out$missing_latlong, c(FALSE, TRUE))
  expect_equal(out$missing_date, c(FALSE, TRUE))
  expect_equal(out$missing_sdnhm_id, c(FALSE, TRUE))
  expect_equal(out$missing_genus, c(FALSE, TRUE))
})

test_that("detect_duplicate_ids catches dup ucsd and sdnhm (ignoring 0/NA)", {
  src("clean/specimen_clean.R")
  df <- tibble::tibble(
    ucsd_id  = c(1, 1, 2, 3),
    sdnhm_id = c(10, 20, 0, 0))
  dups <- detect_duplicate_ids(df)
  expect_true(1 %in% dups$ucsd_id)          # duplicated ucsd
  expect_false(any(dups$sdnhm_id == 0))     # zeros never flagged
})

test_that("match_specimen_complex prefixes and gates on species", {
  src("clean/specimen_clean.R")
  df <- tibble::tibble(genus = c("Diadasia", "Diadasia"), species = c("australis", NA),
                       complex = NA_character_, complex_taxon_id = NA)
  lk <- tibble::tibble(genus = "diadasia", species = "australis",
                       complex_match = "Diadasia australis", complex_taxon_id_match = 42)
  out <- match_specimen_complex(df, lk)
  expect_equal(out$complex[1], "(Complex) Diadasia australis")
  expect_equal(out$complex_taxon_id[1], 42)
  expect_true(is.na(out$complex[2]))        # genus-only -> no complex
})

test_that("build_old_scientific_name handles the three blank cases", {
  src("clean/specimen_clean.R")
  df <- tibble::tibble(old_genus_name = c(NA, "Andrena", "Andrena"),
                       old_species_name = c(NA, NA, "prunorum"))
  out <- build_old_scientific_name(df)
  expect_true(is.na(out$old_scientific_name[1]))
  expect_equal(out$old_scientific_name[2], "Andrena")
  expect_equal(out$old_scientific_name[3], "Andrena prunorum")
})

test_that("resolve_flag_gate: clean / continue / stop", {
  src("clean/specimen_clean.R")
  expect_equal(resolve_flag_gate(0, interactive_ok = TRUE), "clean")
  expect_equal(resolve_flag_gate(3, interactive_ok = FALSE), "continue")   # non-interactive skips
  expect_equal(resolve_flag_gate(3, interactive_ok = TRUE, prompt_fn = function(...) "y"), "continue")
  expect_equal(resolve_flag_gate(3, interactive_ok = TRUE, prompt_fn = function(...) "n"), "stop")
})
