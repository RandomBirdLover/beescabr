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
