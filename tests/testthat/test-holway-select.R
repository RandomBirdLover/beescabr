library(testthat)

sp <- function(rank) list(id = 1, name = "x", rank = rank)

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
