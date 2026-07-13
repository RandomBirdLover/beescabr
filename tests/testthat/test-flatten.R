# Tests for the PURE flatten transforms (api/inat_flatten.R).
# These run anywhere -- no network, no DuckDB.

library(testthat)
library(jsonlite)

source_here <- function(rel) {
  # locate scripts/ relative to this test file regardless of cwd
  base <- Sys.getenv("BEESCABR_ROOT", unset = normalizePath(file.path(dirname(getwd())), mustWork = FALSE))
  candidates <- c(
    file.path("scripts", rel),
    file.path("..", "..", "scripts", rel),
    file.path(base, "scripts", rel)
  )
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) stop("cannot locate scripts/", rel)
  source(hit)
}
source_here("api/inat_flatten.R")

fx <- function(name) {
  candidates <- c(
    file.path("tests/testthat/fixtures", name),
    file.path("fixtures", name),
    file.path("..", "testthat", "fixtures", name)
  )
  candidates[file.exists(candidates)][1]
}

test_that("flatten_ofvs reads taxon-datatype fields from taxon$name, not value", {
  obs <- fromJSON(fx("observation_sample.json"), simplifyVector = FALSE)
  cols <- flatten_ofvs(obs$ofvs)

  expect_equal(cols[["field:interaction->visited flower of"]], "Isocoma menziesii")
  # NOT the raw numeric id "77511"
  expect_false(identical(cols[["field:interaction->visited flower of"]], "77511"))
})

test_that("flatten_ofvs reads text-datatype fields from value directly", {
  obs <- fromJSON(fx("observation_sample.json"), simplifyVector = FALSE)
  cols <- flatten_ofvs(obs$ofvs)
  expect_equal(cols[["field:bee survey transect name"]], "TP1")
})

test_that("flatten_ofvs column names match the CSV export convention", {
  obs <- fromJSON(fx("observation_sample.json"), simplifyVector = FALSE)
  cols <- flatten_ofvs(obs$ofvs)
  # export used lowercase 'field:' prefix
  expect_true("field:interaction->visited flower of" %in% names(cols))
})

test_that("flatten_ofvs handles empty / NULL ofvs", {
  expect_equal(length(flatten_ofvs(NULL)), 0)
  expect_equal(length(flatten_ofvs(list())), 0)
})

test_that("flatten_ofvs survives malformed records (regression for atomic-vector crash)", {
  # taxon-datatype field whose `taxon` is a bare id string, not an object
  bad_taxon <- list(list(name = "Interaction->Visited flower of",
                         datatype = "taxon", value = "77511", taxon = "77511"))
  expect_silent(cols <- flatten_ofvs(bad_taxon))
  expect_true(is.na(cols[["field:interaction->visited flower of"]]))

  # an ofvs entry that isn't an object at all
  mixed <- list("junk", list(name = "Bee Survey Transect Name", datatype = "text", value = "TP1"))
  expect_silent(cols2 <- flatten_ofvs(mixed))
  expect_equal(cols2[["field:bee survey transect name"]], "TP1")
})

test_that("flatten_observation survives a malformed taxon/user/geojson", {
  obs <- list(id = 5, taxon = "not-an-object", user = 42,
              geojson = "nope", ofvs = list(list(name = "X", datatype = "taxon", taxon = "9")))
  expect_silent(row <- flatten_observation(obs))
  expect_equal(row$id, 5L)
  expect_true(is.na(row$scientific_name))
  expect_true(is.na(row$latitude))
})

test_that("flatten_observation extracts core identity + coordinates", {
  obs <- fromJSON(fx("observation_sample.json"), simplifyVector = FALSE)
  row <- flatten_observation(obs)

  expect_equal(row$id, 12345678L)
  expect_equal(row$user_login, "markkjames")
  expect_equal(row$scientific_name, "Melissodes robustior")
  expect_equal(row$taxon_id, 632955L)
  expect_equal(round(row$longitude, 3), -117.242)
  expect_equal(round(row$latitude, 3), 32.672)
  expect_false(row$captive_cultivated)
  expect_false(row$coordinates_obscured)
})

test_that("flatten_observation joins tags into a comma-separated tag_list", {
  obs <- fromJSON(fx("observation_sample.json"), simplifyVector = FALSE)
  row <- flatten_observation(obs)
  expect_equal(row$tag_list, "CabrilloBee10MinuteSurvey, Cabrillo2021BeeSurvey, UPMON")
})

test_that("flatten_observation carries obs-field columns through", {
  obs <- fromJSON(fx("observation_sample.json"), simplifyVector = FALSE)
  row <- flatten_observation(obs)
  expect_equal(row[["field:interaction->visited flower of"]], "Isocoma menziesii")
  expect_equal(row[["field:bee survey transect name"]], "TP1")
})

test_that("flatten_observation handles missing geojson", {
  obs <- fromJSON(fx("observation_sample.json"), simplifyVector = FALSE)
  obs$geojson <- NULL
  row <- flatten_observation(obs)
  expect_true(is.na(row$latitude))
  expect_true(is.na(row$longitude))
})

test_that("parse_taxon_ranks builds the full ranked hierarchy", {
  tx <- fromJSON(fx("taxon_sample.json"), simplifyVector = FALSE)
  ranks <- parse_taxon_ranks(tx)

  expect_equal(ranks$taxon_id, 632955L)
  expect_equal(ranks$taxon_kingdom_name, "Animalia")
  expect_equal(ranks$taxon_family_name, "Apidae")
  expect_equal(ranks$taxon_tribe_name, "Eucerini")
  expect_equal(ranks$taxon_genus_name, "Melissodes")
  # species column carries the full binomial, exactly like the export
  expect_equal(ranks$taxon_species_name, "Melissodes robustior")
  expect_true(is.na(ranks$taxon_subspecies_name))
  # subgenus captured from the ancestor chain
  expect_equal(ranks$subgenus, "Melissodes")
})

test_that("parse_taxon_ranks flags a taxon that is itself a complex", {
  complex_taxon <- list(
    id = 1630911, name = "Diadasia australis", rank = "complex",
    ancestors = list(
      list(id = 48460, name = "Animalia", rank = "kingdom"),
      list(id = 632790, name = "Diadasia", rank = "genus")
    )
  )
  ranks <- parse_taxon_ranks(complex_taxon)
  expect_equal(ranks$complex, "Diadasia australis")
  expect_equal(ranks$complex_taxon_id, 1630911L)
  expect_equal(ranks$taxon_genus_name, "Diadasia")
})
