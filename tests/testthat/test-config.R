library(testthat)

test_that("config defines the API + place constants", {
  src("config.R")
  expect_equal(TAXON_ANTHOPHILA, 630955L)
  expect_equal(TAXON_APIS_MELLIFERA, 47219L)
  expect_equal(PLACE_CABR_MONUMENT, 4715L)
  expect_equal(FAIR_MONTHS, 3:10)          # the standardized survey season
  # NOTE: STORE_CRS and BEESCABR_PROJECT_CRS were retired in a refactor; the
  # working CRS is now PROJECT_CRS, defined in scripts/spatial/spatial_utils.R.
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

test_that("the export rank columns are present and ordered (incl. sub-ranks)", {
  src("config.R")
  expect_length(TAXON_RANK_COLUMNS, 17)
  expect_equal(TAXON_RANK_COLUMNS[1], "taxon_kingdom_name")
  expect_true("taxon_subtribe_name" %in% TAXON_RANK_COLUMNS)
  expect_true(all(c("taxon_subphylum_name", "taxon_subclass_name", "taxon_suborder_name",
                    "taxon_infraorder_name", "taxon_epifamily_name") %in% TAXON_RANK_COLUMNS))
  expect_equal(TAXON_RANK_COLUMNS[17], "taxon_subspecies_name")
  # config also exposes the requested 19-level taxonomic order
  expect_length(TAXONOMY_LEVELS, 19)
  expect_equal(TAXONOMY_LEVELS[1], "kingdom")
  expect_equal(TAXONOMY_LEVELS[14], "subtribe")
  expect_equal(TAXONOMY_LEVELS[19], "subspecies")
})
