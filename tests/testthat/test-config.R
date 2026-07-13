library(testthat)

test_that("config defines the API + place constants", {
  src("config.R")
  expect_equal(TAXON_ANTHOPHILA, 630955L)
  expect_equal(TAXON_APIS_MELLIFERA, 47219L)
  expect_equal(STORE_CRS, 4326L)
  expect_equal(BEESCABR_PROJECT_CRS, 26946L)
})

test_that("field-id map keys the specially-handled obs-fields", {
  src("config.R")
  expect_equal(KNOWN_FIELD_IDS$flower_visited, 3126L)
  expect_equal(KNOWN_FIELD_IDS$transect_name, 19447L)
  expect_equal(KNOWN_FIELD_IDS$tags_override, 20521L)
})

test_that("flower-visited sources list the authoritative field first", {
  src("config.R")
  expect_equal(FLOWER_VISITED_SOURCES[1], "field:interaction->visited flower of")
  expect_true("field:nectar plant" %in% FLOWER_VISITED_SOURCES)
})

test_that("the export rank columns are present and ordered (incl. subtribe)", {
  src("config.R")
  expect_length(TAXON_RANK_COLUMNS, 12)
  expect_equal(TAXON_RANK_COLUMNS[1], "taxon_kingdom_name")
  expect_true("taxon_subtribe_name" %in% TAXON_RANK_COLUMNS)
  expect_equal(TAXON_RANK_COLUMNS[12], "taxon_subspecies_name")
  # config also exposes the requested 14-level taxonomic order
  expect_equal(TAXONOMY_LEVELS[1], "kingdom")
  expect_equal(TAXONOMY_LEVELS[9], "subtribe")
  expect_equal(TAXONOMY_LEVELS[14], "subspecies")
})
