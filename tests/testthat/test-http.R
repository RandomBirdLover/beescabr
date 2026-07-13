library(testthat)

test_that(".page_done detects the last (short) page", {
  src("engine/api/inat_http.R")
  expect_false(.page_done(200L, 200L))  # full page -> more to come
  expect_true(.page_done(150L, 200L))   # short page -> done
  expect_true(.page_done(0L, 200L))     # empty -> done
})

test_that(".inat_build_request assembles url + query params", {
  src("engine/api/inat_http.R")
  req <- .inat_build_request("observations",
                             query = list(place_id = 118491, taxon_id = 630955, id_above = 100))
  expect_match(req$url, "api\\.inaturalist\\.org/v1/observations")
  expect_match(req$url, "place_id=118491")
  expect_match(req$url, "taxon_id=630955")
  expect_match(req$url, "id_above=100")
})

test_that(".inat_build_request drops NULL/empty query params", {
  src("engine/api/inat_http.R")
  req <- .inat_build_request("taxa", query = list(q = "Melissodes", without_taxon_id = NULL))
  expect_match(req$url, "q=Melissodes")
  expect_no_match(req$url, "without_taxon_id")
})
