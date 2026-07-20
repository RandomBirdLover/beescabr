library(testthat)
library(dplyr)

test_that("standardize_specimen_names fixes casing", {
  src("specimens/specimen_clean.R")
  df <- tibble::tibble(genus = c("ANDRENA", "melissodes"),
                       species = c("Robustior", "SUBTILIOR"),
                       subspecies = c(NA, "FOO"))
  out <- standardize_specimen_names(df)
  expect_equal(out$genus, c("Andrena", "Melissodes"))
  expect_equal(out$species, c("robustior", "subtilior"))
  expect_equal(out$subspecies, c(NA, "foo"))
})

test_that("fill_specimen_taxonomy coalesces blanks from the lookup only", {
  src("specimens/specimen_clean.R")
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
  src("specimens/specimen_clean.R")
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
  src("specimens/specimen_clean.R")
  df <- tibble::tibble(latitude = c(1, NA), longitude = c(1, 2), date = c(Sys.Date(), NA),
                       sdnhm_id = c("A", ""), ucsd_id = c("U", "V"), genus = c("Andrena", ""))
  out <- add_qc_flags(df)
  expect_equal(out$missing_latlong, c(FALSE, TRUE))
  expect_equal(out$missing_date, c(FALSE, TRUE))
  expect_equal(out$missing_sdnhm_id, c(FALSE, TRUE))
  expect_equal(out$missing_genus, c(FALSE, TRUE))
})

test_that("detect_duplicate_ids catches dup ucsd and sdnhm (ignoring 0/NA)", {
  src("specimens/specimen_clean.R")
  df <- tibble::tibble(
    ucsd_id  = c(1, 1, 2, 3),
    sdnhm_id = c(10, 20, 0, 0))
  dups <- detect_duplicate_ids(df)
  expect_true(1 %in% dups$ucsd_id)          # duplicated ucsd
  expect_false(any(dups$sdnhm_id == 0))     # zeros never flagged
})

test_that("match_specimen_complex prefixes and gates on species", {
  src("specimens/specimen_clean.R")
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
  src("specimens/specimen_clean.R")
  df <- tibble::tibble(old_genus_name = c(NA, "Andrena", "Andrena"),
                       old_species_name = c(NA, NA, "prunorum"))
  out <- build_old_scientific_name(df)
  expect_true(is.na(out$old_scientific_name[1]))
  expect_equal(out$old_scientific_name[2], "Andrena")
  expect_equal(out$old_scientific_name[3], "Andrena prunorum")
})

test_that("resolve_flag_gate: clean / continue / stop", {
  src("specimens/specimen_clean.R")
  expect_equal(resolve_flag_gate(0, interactive_ok = TRUE), "clean")
  expect_equal(resolve_flag_gate(3, interactive_ok = FALSE), "continue")   # non-interactive skips
  expect_equal(resolve_flag_gate(3, interactive_ok = TRUE, prompt_fn = function(...) "y"), "continue")
  expect_equal(resolve_flag_gate(3, interactive_ok = TRUE, prompt_fn = function(...) "n"), "stop")
})

test_that("flag_raw_clutter tags non-ID'd and missing rows", {
  src("specimens/specimen_clean.R")
  df <- tibble::tibble(ucsd_id = 1:4,
                       genus = c("Andrena", "", "Bombus", NA_character_),
                       missing_specimen = c("N", "N", "Y", "Y"))
  out <- flag_raw_clutter(df)
  expect_equal(nrow(out), 3)                              # row1 (Andrena, N) is clean
  expect_equal(out$clutter_reason[out$ucsd_id == 2], "needs_id")
  expect_equal(out$clutter_reason[out$ucsd_id == 3], "missing")
  expect_equal(out$clutter_reason[out$ucsd_id == 4], "needs_id; missing")
})

test_that("transect_variant_map + match_plot_transect map plot text to TP/UPMON/BST", {
  src("specimens/specimen_clean.R")
  cw <- tibble::tibble(
    name     = c("tp", "upmon", "bst", "ot", "feeding"),
    what_for = c("transect", "transect", "transect", "transect", "behavior"),
    specimen_label_variants = c("Cabrillo NM: Tide Pool Trail; TPT1; TP2",
                                "Cabrillo NM: UpMon; Upr. Mnumnt.",
                                "Cabrillo NM: Bayside Trail; BST",
                                NA_character_, NA_character_))
  vm <- transect_variant_map(cw)
  expect_true(all(c("TP", "UPMON", "BST") %in% vm$transect))
  expect_false("OT" %in% vm$transect)            # no specimen variants -> not in the map
  plots <- c("Cabrillo NM: Upr. Mnumnt.", "Cabrillo NM: Tide Pool Trail 1",
             "Cabrillo NM: BST", "Cabrillo NM", NA_character_)
  expect_equal(match_plot_transect(plots, vm), c("UPMON", "TP", "BST", NA, NA))
})

test_that("attach_lookup_taxonomy fills taxon_id + higher ranks, keeps specimen names", {
  src("specimens/specimen_clean.R")
  df <- tibble::tibble(genus = c("Andrena", NA_character_), species = c("baeriae", NA_character_),
                       subspecies = NA_character_, subgenus = c("Callandrena", NA_character_),
                       complex = NA_character_, family = c(NA_character_, "Andrenidae"),
                       subfamily = NA_character_, tribe = NA_character_)
  lk <- tibble::tibble(
    taxon_id = c(123L, 999L), rank = c("species", "family"),
    scientific_name = c("Andrena baeriae", "Andrenidae"), common_name = NA_character_,
    kingdom = "Animalia", phylum = "Arthropoda", subphylum = "Hexapoda", class = "Insecta",
    subclass = "Pterygota", order = "Hymenoptera", suborder = "Apocrita", infraorder = "Aculeata",
    superfamily = "Apoidea", family = c("Andrenidae", "Andrenidae"), epifamily = "Anthophila",
    subfamily = c("Andreninae", NA_character_), tribe = c("Andrenini", NA_character_),
    subtribe = NA_character_, genus = c("Andrena", NA_character_),
    subgenus = c("Callandrena", NA_character_), complex = NA_character_,
    species = c("baeriae", NA_character_), subspecies = NA_character_)
  out <- attach_lookup_taxonomy(df, lk)
  expect_equal(out$taxon_id[1], 123L)
  expect_equal(out$taxon_rank[1], "species")
  expect_equal(out$scientific_name[1], "Andrena baeriae")
  expect_equal(out$kingdom[1], "Animalia")
  expect_equal(out$family[1], "Andrenidae")   # was NA -> filled from lookup
  expect_equal(out$genus[1], "Andrena")       # specimen's own kept
  expect_equal(out$species[1], "baeriae")
  # blank-genus (unidentified) specimen must NOT inherit a higher-rank id
  expect_true(is.na(out$taxon_id[2]))
  expect_true(is.na(out$scientific_name[2]))
  expect_equal(out$family[2], "Andrenidae")   # its own family column is kept
})
