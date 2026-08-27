library(testthat)

# DB-backed tests. Run only where the duckdb R package is installed (e.g. the
# user's Mac). They also need the spatial extension for the geometry column;
# if it can't load (offline first run), the test skips rather than fails.

skip_if_no_store <- function() {
  if (!have_duckdb()) skip("duckdb R package not installed")
}

open_temp_store <- function() {
  src("config.R"); src("inat_observations/engine/db/store_conn.R"); src("inat_observations/engine/db/observations_store.R")
  src("inat_observations/engine/db/taxon_store.R"); src("inat_observations/engine/db/decision_store.R")
  path <- tempfile(fileext = ".duckdb")
  con <- tryCatch(store_connect(path), error = function(e) skip(conditionMessage(e)))
  con
}

test_that("observation upsert is idempotent and tracks the max id cursor", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)

  obs <- list(list(
    id = 12345, taxon = list(id = 632955), observed_on = "2021-04-12",
    geojson = list(type = "Point", coordinates = list(-117.24, 32.67))
  ))
  expect_equal(write_observations(con, obs), 1L)
  write_observations(con, obs)  # re-upsert same id
  expect_equal(count_observations(con), 1L)
  expect_equal(max_observation_id(con), 12345L)

  raw <- read_observations_raw(con)
  expect_equal(nrow(raw), 1L)
})

test_that("taxon cache round-trips by id and by name key", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)

  expect_null(taxon_cache_get(con, taxon_cache_key_id(1)))
  taxon <- list(id = 632955, name = "Melissodes robustior", rank = "species")
  taxon_cache_put(con, taxon_cache_key_id(632955), 632955, taxon)

  hit <- taxon_cache_get(con, taxon_cache_key_id(632955))
  expect_equal(hit$name, "Melissodes robustior")
  expect_equal(taxon_cache_count(con), 1)
})

test_that("decision store records pick and skip", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)

  expect_null(decision_get(con, "Andrena quercina"))
  decision_put(con, "Andrena quercina", "pick", 361408)
  d <- decision_get(con, "Andrena quercina")
  expect_equal(d$action, "pick")
  expect_equal(d$chosen_taxon_id, 361408L)

  decision_put(con, "Foo bar", "skip")
  expect_equal(decision_get(con, "Foo bar")$action, "skip")
})

test_that("ingest_observations pages via raw text and DuckDB-side parsing", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  src("inat_observations/engine/api/inat_http.R"); src("inat_observations/engine/api/inat_flatten.R"); src("inat_observations/engine/api/inat_cache.R")
  src("inat_observations/engine/pipelines/ingest_inat.R")

  # fake API returns RAW response strings (what inat_request_text yields)
  recs <- lapply(1:5, function(i) list(
    id = i, taxon = list(id = i * 10), observed_on = "2021-04-12",
    geojson = list(type = "Point", coordinates = list(-117.24 + i * 0.001, 32.67))
  ))
  fake_text <- function(path, query = list(), ...) {
    above <- as.numeric(query$id_above %||% 0)
    remaining <- Filter(function(r) r$id > above, recs)
    page <- head(remaining, query$per_page)
    as.character(jsonlite::toJSON(list(total_results = length(remaining), results = page),
                                  auto_unbox = TRUE, null = "null"))
  }

  # per_page = 2 forces 3 pages; commit_every = 1 commits each page
  # state_path MUST be injected: its default is repo-root-relative, so a test run
  # would write a real last_ingest.txt under tests/testthat/ and dirty the repo.
  n <- ingest_observations(con, place_id = 1, taxon_id = 1, without_taxon_id = 1,
                           incremental = FALSE, per_page = 2L, commit_every = 1L, throttle = 0,
                           state_path = file.path(tempdir(), "last_ingest_test.txt"),
                           request_text_fn = fake_text, sleep_fn = function(...) NULL, verbose = FALSE)
  expect_equal(n, 5L)
  expect_equal(count_observations(con), 5L)
  expect_equal(max_observation_id(con), 5L)
})

test_that("read_observations_export caches the flatten and invalidates on change", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  src("inat_observations/engine/api/inat_http.R"); src("inat_observations/engine/api/inat_flatten.R"); src("inat_observations/engine/api/inat_cache.R")
  src("inat_observations/engine/db/observations_store.R"); src("inat_observations/engine/pipelines/read_inat.R")

  page <- function(recs) as.character(jsonlite::toJSON(list(results = recs),
                                                       auto_unbox = TRUE, null = "null"))
  mk <- function(i) list(id = i, taxon = list(id = 100), observed_on = "2021-04-12",
                         geojson = list(type = "Point", coordinates = list(-117.2, 32.6)))
  write_observations_page(con, page(lapply(1:3, mk)))

  taxon <- jsonlite::fromJSON(fx("taxon_sample.json"), simplifyVector = FALSE)
  fake_req <- function(path, query = list(), ...) {
    ids <- as.integer(strsplit(sub("^taxa/", "", path), ",")[[1]])
    list(results = lapply(ids, function(id) { t <- taxon; t$id <- id; t }))
  }
  cache <- tempfile(fileext = ".rds")

  d1 <- read_observations_export(con, request_fn = fake_req, verbose = FALSE, cache_path = cache)
  expect_equal(nrow(d1), 3L)
  expect_true(file.exists(cache))

  # unchanged inputs -> disk cache reused, identical result
  d2 <- read_observations_export(con, request_fn = fake_req, verbose = FALSE, cache_path = cache)
  expect_equal(nrow(d2), 3L)

  # adding an observation changes the signature -> rebuild picks it up
  write_observations_page(con, page(list(mk(4))))
  d3 <- read_observations_export(con, request_fn = fake_req, verbose = FALSE, cache_path = cache)
  expect_equal(nrow(d3), 4L)
})

test_that("resolve_taxonomy caches: second call makes no API request", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  src("inat_observations/engine/api/inat_http.R"); src("inat_observations/engine/api/inat_flatten.R"); src("inat_observations/engine/api/inat_cache.R")

  taxon <- jsonlite::fromJSON(fx("taxon_sample.json"), simplifyVector = FALSE)
  calls <- 0
  fake_request <- function(path, query = list(), user_agent = NULL) {
    calls <<- calls + 1
    list(results = list(taxon))
  }

  m1 <- resolve_taxonomy(con, c(632955), request_fn = fake_request, throttle = 0, verbose = FALSE)
  expect_equal(m1$taxon_genus_name[1], "Melissodes")
  expect_equal(calls, 1)

  # second resolve hits the cache -> no new request
  m2 <- resolve_taxonomy(con, c(632955), request_fn = fake_request, throttle = 0, verbose = FALSE)
  expect_equal(calls, 1)
  expect_equal(m2$taxon_family_name[1], "Apidae")
})

test_that("resolve_taxonomy batches many ids into one request (rate-limit fix)", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  src("inat_observations/engine/api/inat_http.R"); src("inat_observations/engine/api/inat_flatten.R"); src("inat_observations/engine/api/inat_cache.R")

  base <- jsonlite::fromJSON(fx("taxon_sample.json"), simplifyVector = FALSE)
  calls <- 0
  fake_request <- function(path, query = list(), user_agent = NULL) {
    calls <<- calls + 1
    ids <- as.integer(strsplit(sub("^taxa/", "", path), ",")[[1]])
    list(results = lapply(ids, function(id) { t <- base; t$id <- id; t }))
  }

  m <- resolve_taxonomy(con, c(632955, 111, 222), request_fn = fake_request,
                        throttle = 0, verbose = FALSE)
  expect_equal(nrow(m), 3)
  expect_equal(calls, 1)                 # 3 ids resolved in ONE batched request
  expect_true(all(m$taxon_family_name == "Apidae"))
})
