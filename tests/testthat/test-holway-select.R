library(testthat)

sp <- function(rank) list(id = 1, name = "x", rank = rank)
mk <- function(id, name, rank) list(id = id, name = name, rank = rank)

test_that("exact name match auto-picks (subspecies, with ssp. normalization)", {
  src("checklists/holway_reference_build.R")
  results <- list(mk(1, "Ashmeadiella cactorum", "species"),
                  mk(313836, "Ashmeadiella cactorum basalis", "subspecies"))
  res <- select_taxon_candidate(results, described = TRUE,
                                search_term = "Ashmeadiella cactorum ssp. basalis",
                                is_subspecies = TRUE)
  expect_equal(res$action, "pick")
  expect_equal(res$index, 2L)                       # the subspecies, not the parent
})

test_that("subspecies with no exact match does NOT grab the parent species", {
  src("checklists/holway_reference_build.R")
  results <- list(mk(1, "Atoposmia copelandica", "species"))   # only the species exists
  res <- select_taxon_candidate(results, described = TRUE,
                                search_term = "Atoposmia copelandica ssp. albomarginata",
                                is_subspecies = TRUE)
  expect_equal(res$action, "skip")                  # -> routes to ITIS keep/skip
})

test_that("exact match disambiguates near-spellings (Andrena nigra vs nigrae)", {
  src("checklists/holway_reference_build.R")
  results <- list(mk(1, "Andrena nigrae", "species"), mk(2, "Andrena nigra", "species"))
  res <- select_taxon_candidate(results, described = TRUE, search_term = "Andrena nigra")
  expect_equal(res$action, "pick")
  expect_equal(res$index, 2L)
})

test_that("resolved subspecies scientific_name gets the ssp. display form", {
  src("checklists/holway_reference_build.R")
  ranks <- tibble::tibble(
    taxon_id = 313836L,
    taxon_kingdom_name = "Animalia", taxon_phylum_name = "Arthropoda",
    taxon_class_name = "Insecta", taxon_order_name = "Hymenoptera",
    taxon_superfamily_name = "Apoidea", taxon_family_name = "Megachilidae",
    taxon_subfamily_name = "Megachilinae", taxon_tribe_name = "Osmiini",
    taxon_subtribe_name = NA_character_, taxon_genus_name = "Ashmeadiella",
    taxon_species_name = "Ashmeadiella cactorum",
    taxon_subspecies_name = "Ashmeadiella cactorum basalis",
    subgenus = "Ashmeadiella", complex = NA_character_, complex_taxon_id = NA_integer_,
    rank = "subspecies")
  row <- tidy_holway_ref_row(ranks, scientific_name = "Ashmeadiella cactorum basalis",
                             common_name = NA_character_, source_sheet = "Described")
  expect_equal(row$scientific_name, "Ashmeadiella cactorum ssp. basalis")
  expect_equal(row$species, "cactorum"); expect_equal(row$subspecies, "basalis")
  expect_equal(row$taxon_id, 313836L)
})

test_that("holway_resolution_plan builds the right search per row type", {
  src("checklists/holway.R"); src("checklists/holway_reference_build.R")
  p_ss <- holway_resolution_plan("Described", "Ashmeadiella", split_holway_species("cactorum basalis"))
  expect_true(p_ss$is_subspecies)
  expect_equal(p_ss$term, "Ashmeadiella cactorum basalis")   # plain trinomial, no ssp.
  p_sp <- holway_resolution_plan("Described", "Stelis", split_holway_species("anthocopae"))
  expect_false(p_sp$is_subspecies)
  expect_equal(p_sp$term, "Stelis anthocopae")
  p_g <- holway_resolution_plan("Unpublished", "Andrena", split_holway_species("sp1"))
  expect_equal(p_g$term, "Andrena")
})

test_that("parse_slash_options splits a name pair into full-name candidates", {
  src("checklists/holway.R"); src("checklists/holway_reference_build.R")
  expect_equal(parse_slash_options("Bombus", "californicus / fervidus"),
               c("Bombus californicus", "Bombus fervidus"))
  expect_equal(parse_slash_options("Bombus", "pensylvanicus / sonorus"),
               c("Bombus pensylvanicus", "Bombus sonorus"))
})

test_that("resolve_slash_answer maps number / typed name / none", {
  src("checklists/holway_reference_build.R")
  opts <- c("Bombus californicus", "Bombus fervidus")
  expect_equal(resolve_slash_answer("1", "Bombus", opts), "Bombus californicus")
  expect_equal(resolve_slash_answer("2", "Bombus", opts), "Bombus fervidus")
  expect_equal(resolve_slash_answer("sonorus", "Bombus", opts), "Bombus sonorus")  # custom
  expect_true(is.na(resolve_slash_answer("none", "Bombus", opts)))
  expect_true(is.na(resolve_slash_answer("", "Bombus", opts)))
})

test_that("unresolved_holway_ref_row builds subspecies and species keep-rows", {
  src("checklists/holway.R"); src("checklists/holway_reference_build.R")
  ss <- tibble::tibble(source_sheet = "Described", family = "Megachilidae",
                       subfamily = "Megachilinae", tribe = "Osmiini",
                       genus = "Atoposmia", subgenus = "(Hexosmia)",
                       species_raw = "copelandica albomarginata")
  row <- unresolved_holway_ref_row(ss, itis_valid = TRUE, is_subspecies = TRUE)
  expect_equal(row$rank, "subspecies")
  expect_equal(row$species, "copelandica"); expect_equal(row$subspecies, "albomarginata")
  expect_equal(row$scientific_name, "Atoposmia copelandica ssp. albomarginata")
  expect_equal(row$subgenus, "Hexosmia")            # parens stripped
  expect_true(row$itis_valid); expect_true(is.na(row$taxon_id))

  spp <- tibble::tibble(source_sheet = "Described", family = "Megachilidae",
                        subfamily = NA_character_, tribe = NA_character_,
                        genus = "Stelis", subgenus = "", species_raw = "anthocopae")
  row2 <- unresolved_holway_ref_row(spp, itis_valid = TRUE, is_subspecies = FALSE)
  expect_equal(row2$rank, "species"); expect_equal(row2$species, "anthocopae")
  expect_true(is.na(row2$subspecies))
  expect_equal(row2$scientific_name, "Stelis anthocopae")
})

test_that("select_taxon_candidate picks the single result", {
  src("checklists/holway_reference_build.R")
  expect_equal(select_taxon_candidate(list(sp("species")), described = TRUE)$action, "pick")
})

test_that("select_taxon_candidate skips empty results", {
  src("checklists/holway_reference_build.R")
  expect_equal(select_taxon_candidate(list(), described = TRUE)$action, "skip")
})

test_that("non-Described multi-result auto-skips (genus-level search)", {
  src("checklists/holway_reference_build.R")
  res <- select_taxon_candidate(list(sp("species"), sp("genus")), described = FALSE)
  expect_equal(res$action, "skip")
})

test_that("Described with one species hit auto-picks it", {
  src("checklists/holway_reference_build.R")
  results <- list(sp("genus"), sp("species"), sp("subgenus"))
  res <- select_taxon_candidate(results, described = TRUE)
  expect_equal(res$action, "pick")
  expect_equal(res$index, 2L)
})

test_that("Described with multiple species hits needs a prompt", {
  src("checklists/holway_reference_build.R")
  res <- select_taxon_candidate(list(sp("species"), sp("species")), described = TRUE)
  expect_equal(res$action, "prompt")
})

test_that("holway_search_term uses genus+species for Described, genus otherwise", {
  src("checklists/holway_reference_build.R")
  expect_equal(holway_search_term("Described", "Andrena", "quercina"), "Andrena quercina")
  expect_equal(holway_search_term("Unpublished", "Andrena", "sp1"), "Andrena")
})

test_that("retry_empty_search returns initial results untouched when non-empty", {
  src("checklists/holway_reference_build.R")
  hit <- list(sp("species"))
  # fetch_fn must never be called if we already have results
  out <- retry_empty_search(hit, "Andrena x",
                            fetch_fn = function(t) stop("should not fetch"),
                            prompt_fn = function(...) stop("should not prompt"))
  expect_identical(out, hit)
})

test_that("retry_empty_search retries with the typed term until a hit", {
  src("checklists/holway_reference_build.R")
  # first alternate term still empty, second finds it
  fetch <- function(t) if (identical(t, "Andrena good")) list(sp("species")) else list()
  answers <- c("Andrena bad", "Andrena good")
  i <- 0
  prompt <- function(...) { i <<- i + 1; answers[i] }
  out <- retry_empty_search(list(), "Andrena typo", fetch_fn = fetch, prompt_fn = prompt)
  expect_length(out, 1)
})

test_that("tidy_holway_ref_row reshapes to the clean lookup layout", {
  src("checklists/holway_reference_build.R")
  ranks <- tibble::tibble(
    taxon_id = 42L,
    taxon_kingdom_name = "Animalia", taxon_phylum_name = "Arthropoda",
    taxon_class_name = "Insecta", taxon_order_name = "Hymenoptera",
    taxon_superfamily_name = "Apoidea", taxon_family_name = "Andrenidae",
    taxon_subfamily_name = "Andreninae", taxon_tribe_name = "Andrenini",
    taxon_subtribe_name = NA_character_, taxon_genus_name = "Andrena",
    taxon_species_name = "Andrena annectens", taxon_subspecies_name = NA_character_,
    subgenus = NA_character_, complex = NA_character_, complex_taxon_id = NA_integer_,
    rank = "species")
  row <- tidy_holway_ref_row(ranks, scientific_name = "Andrena annectens",
                             common_name = "A mining bee", source_sheet = "Tentative")
  # metadata columns first, in the lookup's order
  expect_equal(names(row)[1:4], c("taxon_id", "scientific_name", "common_name", "rank"))
  expect_equal(row$scientific_name, "Andrena annectens")  # authoritative iNat name
  expect_equal(row$species, "annectens")                  # bare epithet, not the binomial
  expect_equal(row$genus, "Andrena")
  expect_equal(row$family, "Andrenidae")
  expect_true(row$resolved)
})

test_that("retry_empty_search stops on 'skip' and when non-interactive", {
  src("checklists/holway_reference_build.R")
  # user skips -> stays empty
  out1 <- retry_empty_search(list(), "Andrena typo",
                             fetch_fn = function(t) list(sp("species")),
                             prompt_fn = function(...) "skip")
  expect_length(out1, 0)
  # non-interactive -> never prompts, stays empty
  out2 <- retry_empty_search(list(), "Andrena typo",
                             fetch_fn = function(t) stop("should not fetch"),
                             prompt_fn = function(...) stop("should not prompt"),
                             interactive_ok = FALSE)
  expect_length(out2, 0)
})
