# A failed IUCN lookup used to return code "NE" -- the SAME value the API returns for a
# species it has genuinely not assessed. So an expired key, an outage, or a typo'd token
# looked exactly like a scientific finding, and would have relabelled Bombus crotchii
# (Endangered) as "Not Evaluated" on the public site. A failure must be distinguishable.

src("reference/enrich_lookups.R")

test_that("a real API answer of NE is reported as NE", {
  r <- .iucn_fetch_one("Apis mellifera", key = "k",
                       fetch_fn = function(...) list(red_list_category = list(code = "NE")))
  expect_equal(r$code, "NE")
  expect_true(r$ok)
})

test_that("a genuine assessment comes through", {
  r <- .iucn_fetch_one("Bombus crotchii", key = "k",
                       fetch_fn = function(...) list(red_list_category = list(code = "EN"),
                                                     year_published = "2024"))
  expect_equal(r$code, "EN")
  expect_true(r$ok)
})

test_that("a FAILED call is not reported as NE", {
  r <- .iucn_fetch_one("Bombus crotchii", key = "bad",
                       fetch_fn = function(...) stop("Token not valid! (HTTP 401)"))
  expect_false(r$ok)
  expect_true(is.na(r$code))          # NOT "NE"
})

test_that("the failure carries the reason, so a bad key can be named", {
  r <- .iucn_fetch_one("Bombus crotchii", key = "bad",
                       fetch_fn = function(...) stop("Token not valid! (HTTP 401)"))
  expect_match(r$error, "401")
})

test_that("an authentication failure is recognisable as one", {
  expect_true(.iucn_is_auth_error("Token not valid! (HTTP 401)"))
  expect_true(.iucn_is_auth_error("HTTP 403 Forbidden"))
  expect_false(.iucn_is_auth_error("Timeout was reached"))
  expect_false(.iucn_is_auth_error(NA_character_))
})
