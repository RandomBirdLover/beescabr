# test-offline-mode.R -- "Offline run: no iNaturalist calls at all" must be true.
#
# Menu option 3 sets BEESCABR_SKIP_INGEST=1 and promises no network. Only the
# OBSERVATION pull honoured it. The plant-common-name step and the missing-taxon-id
# resolver never checked the flag, so an "offline" run still made ~88 requests.
suppressWarnings(suppressMessages(library(testthat)))

withr_env <- function(vals, code) {
  old <- vapply(names(vals), function(n) Sys.getenv(n, unset = NA_character_), character(1))
  do.call(Sys.setenv, as.list(vals))
  on.exit({
    for (n in names(vals)) {
      v <- old[[match(n, names(old))]]
      if (is.na(v)) Sys.unsetenv(n) else do.call(Sys.setenv, setNames(list(v), n))
    }
  }, add = TRUE)
  force(code)
}

test_that("beescabr_offline reads the ingest flag", {
  src("config.R")
  withr_env(c(BEESCABR_SKIP_INGEST = "1"), expect_true(beescabr_offline()))
  withr_env(c(BEESCABR_SKIP_INGEST = "0"), expect_false(beescabr_offline()))
  withr_env(c(BEESCABR_SKIP_INGEST = ""),  expect_false(beescabr_offline()))
})

test_that("resolve_plant_common makes no request when offline", {
  src("reference/refresh/enrich_lookups.R")
  called <- 0L
  withr_env(c(BEESCABR_SKIP_INGEST = "1"), {
    suppressMessages(resolve_plant_common(
      c("Zzyzxia", "Qqqqia"), verbose = FALSE,
      fetch_fn   = function(g) { called <<- called + 1L; "made up" },
      reach_fn   = function() { called <<- called + 1L; TRUE },
      cache_path = tempfile(fileext = ".csv")))
  })
  expect_equal(called, 0L)
})

test_that("resolve_plant_common still fetches when online", {
  src("reference/refresh/enrich_lookups.R")
  called <- 0L
  withr_env(c(BEESCABR_SKIP_INGEST = "0"), {
    suppressMessages(resolve_plant_common(
      "Zzyzxia", verbose = FALSE,
      fetch_fn   = function(g) { called <<- called + 1L; "Made Up" },
      reach_fn   = function() TRUE,
      cache_path = tempfile(fileext = ".csv")))
  })
  expect_equal(called, 1L)
})

# --- and stop re-asking for a genus that simply has no common name ----------
# `have` counted only genera WITH a name, so the 70 that have none were fetched
# again on every run: "fetched 0 new", 35 s of throttled waiting, nothing saved.
test_that("a genus with no common name is remembered as checked", {
  src("reference/refresh/enrich_lookups.R")
  cp <- tempfile(fileext = ".csv")
  n1 <- 0L
  suppressMessages(resolve_plant_common("Apioideae", verbose = FALSE, cache_path = cp,
                                        reach_fn = function() TRUE,
                                        fetch_fn = function(g) { n1 <<- n1 + 1L; NA_character_ }))
  expect_equal(n1, 1L)                         # asked once
  n2 <- 0L
  suppressMessages(resolve_plant_common("Apioideae", verbose = FALSE, cache_path = cp,
                                        reach_fn = function() TRUE,
                                        fetch_fn = function(g) { n2 <<- n2 + 1L; NA_character_ }))
  expect_equal(n2, 0L)                         # never asked again
})

test_that("a genus that DOES have a common name still resolves and caches", {
  src("reference/refresh/enrich_lookups.R")
  cp <- tempfile(fileext = ".csv")
  out <- suppressMessages(resolve_plant_common("Salvia", verbose = FALSE, cache_path = cp,
                                               reach_fn = function() TRUE,
                                               fetch_fn = function(g) "sage"))
  expect_equal(unname(out[["Salvia"]]), "Sage")
  n <- 0L
  suppressMessages(resolve_plant_common("Salvia", verbose = FALSE, cache_path = cp,
                                        reach_fn = function() TRUE,
                                        fetch_fn = function(g) { n <<- n + 1L; "sage" }))
  expect_equal(n, 0L)
})

test_that("resolve_missing_taxon_ids makes no search when offline", {
  src("reference/taxonomy/resolve_missing_ids.R")
  called <- 0L
  df <- data.frame(rank = "species", scientific_name = "Stelis anthocopae",
                   family = "Megachilidae", genus = "Stelis", species = "anthocopae",
                   taxon_id = NA_integer_, stringsAsFactors = FALSE)
  withr_env(c(BEESCABR_SKIP_INGEST = "1"), {
    suppressMessages(resolve_missing_taxon_ids(df, cache_path = tempfile(fileext = ".csv"),
                                               fetch_fn = function(...) { called <<- called + 1L; list() }))
  })
  expect_equal(called, 0L)
})
