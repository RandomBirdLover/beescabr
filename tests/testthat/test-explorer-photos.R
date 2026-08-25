# Representative-photo selection for the Bee Occurrence Explorer:
# license labels, credit lines, and the default-photo -> taxon_photos fallback.
# Network is always faked via request_fn (never hits the real iNaturalist API).

src("analysis/explorer_photo_helpers.R")

OPEN <- c("cc0", "pd", "cc-by", "cc-by-nc", "cc-by-sa", "cc-by-nd",
          "cc-by-nc-sa", "cc-by-nc-nd")

ph <- function(lic, url = "http://x/p.jpg", attr = "(c) Someone")
  list(license_code = lic, medium_url = url, attribution = attr)

# ---- license_label ----------------------------------------------------------

test_that("license_label formats iNat license codes for display", {
  expect_equal(license_label("cc-by"),       "CC BY")
  expect_equal(license_label("cc-by-nc"),    "CC BY-NC")
  expect_equal(license_label("cc-by-nc-sa"), "CC BY-NC-SA")
  expect_equal(license_label("cc0"),         "CC0")
  expect_equal(license_label("pd"),          "Public Domain")
})

test_that("license_label is NA for missing codes", {
  expect_true(is.na(license_label(NULL)))
  expect_true(is.na(license_label(NA)))
  expect_true(is.na(license_label("")))
})

# ---- photo_credit -----------------------------------------------------------

test_that("photo_credit appends the license when the attribution lacks it", {
  expect_equal(photo_credit("(c) Jane Doe", "cc-by-nc"),
               "(c) Jane Doe · CC BY-NC")
})

test_that("photo_credit does not duplicate a license already in the attribution", {
  a <- "(c) Jane Doe, some rights reserved (CC BY-NC)"
  expect_equal(photo_credit(a, "cc-by-nc"), a)
  expect_equal(photo_credit("(c) J, some rights reserved (cc by-nc)", "cc-by-nc"),
               "(c) J, some rights reserved (cc by-nc)")   # case-insensitive match
})

test_that("photo_credit falls back to 'iNaturalist' when attribution is missing", {
  expect_equal(photo_credit(NULL, "cc0"), "iNaturalist · CC0")
})

test_that("photo_credit leaves the attribution alone when there is no license code", {
  expect_equal(photo_credit("(c) Jane Doe", NULL), "(c) Jane Doe")
})

# ---- pick_open_photo --------------------------------------------------------

test_that("an openly-licensed default photo is picked directly", {
  t <- list(id = 1, name = "Habropoda depressa",
            default_photo = ph("cc-by", "http://x/default.jpg"))
  got <- pick_open_photo(t, "Habropoda depressa", OPEN)
  expect_equal(got$u, "http://x/default.jpg")
  expect_equal(got$l, "https://www.inaturalist.org/taxa/1")
  expect_true(grepl("CC BY", got$c, fixed = TRUE))
})

test_that("a closed default photo falls back to the first open taxon_photos entry", {
  t <- list(id = 2, name = "Habropoda depressa",
            default_photo = ph(NULL, "http://x/default.jpg"),
            taxon_photos = list(
              list(photo = ph(NULL,       "http://x/1.jpg")),
              list(photo = ph("cc-by-nc", "http://x/2.jpg",
                              "(c) Aidan, some rights reserved (CC BY-NC)")),
              list(photo = ph("cc-by",    "http://x/3.jpg"))))
  got <- pick_open_photo(t, "Habropoda depressa", OPEN)
  expect_equal(got$u, "http://x/2.jpg")                # FIRST open one, in order
  expect_true(grepl("CC BY-NC", got$c, fixed = TRUE))
})

test_that("an open-licensed photo without a medium_url is skipped", {
  t <- list(id = 3, name = "Bombus",
            taxon_photos = list(
              list(photo = ph("cc0", NULL)),
              list(photo = ph("cc-by", "http://x/ok.jpg"))))
  expect_equal(pick_open_photo(t, "Bombus", OPEN)$u, "http://x/ok.jpg")
})

test_that("pick_open_photo is NULL when nothing is openly licensed", {
  t <- list(id = 4, name = "Osmia",
            default_photo = ph(NULL),
            taxon_photos = list(list(photo = ph(NULL)), list(photo = ph("arr"))))
  expect_null(pick_open_photo(t, "Osmia", OPEN))
})

test_that("pick_open_photo rejects a name mismatch and a NULL taxon", {
  t <- list(id = 5, name = "Xenoglossodes davidsoni",
            default_photo = ph("cc0"))
  expect_null(pick_open_photo(t, "Tetraloniella davidsoni", OPEN))
  expect_null(pick_open_photo(NULL, "Osmia", OPEN))
})

test_that("the name match is case-insensitive", {
  t <- list(id = 6, name = "habropoda DEPRESSA", default_photo = ph("cc0", "http://x/d.jpg"))
  expect_equal(pick_open_photo(t, "Habropoda depressa", OPEN)$u, "http://x/d.jpg")
})

# ---- fetch_taxon_photo (faked network) --------------------------------------

fake_api <- function(search_taxon, detail_taxon = NULL) {
  calls <- character(0)
  fn <- function(url) {
    calls[[length(calls) + 1]] <<- url
    if (grepl("/taxa\\?q=", url)) {
      list(results = if (is.null(search_taxon)) list() else list(search_taxon))
    } else {
      list(results = if (is.null(detail_taxon)) list() else list(detail_taxon))
    }
  }
  list(fn = fn, calls = function() calls)
}

test_that("an open default photo needs only the search request", {
  api <- fake_api(list(id = 10, name = "Bombus",
                       default_photo = ph("cc-by", "http://x/def.jpg")))
  got <- fetch_taxon_photo("Bombus", "genus", OPEN,
                           request_fn = api$fn, sleep_fn = function() NULL)
  expect_equal(got$u, "http://x/def.jpg")
  expect_equal(length(api$calls()), 1)
})

test_that("a closed default triggers ONE detail request, picked from taxon_photos", {
  # the real search response has no taxon_photos at all; only the detail does
  api <- fake_api(
    search_taxon = list(id = 11, name = "Habropoda depressa",
                        default_photo = ph(NULL, "http://x/def.jpg")),
    detail_taxon = list(id = 11, name = "Habropoda depressa",
                        default_photo = ph(NULL, "http://x/def.jpg"),
                        taxon_photos = list(
                          list(photo = ph(NULL, "http://x/1.jpg")),
                          list(photo = ph("cc-by-nc", "http://x/2.jpg")))))
  got <- fetch_taxon_photo("Habropoda depressa", "species", OPEN,
                           request_fn = api$fn, sleep_fn = function() NULL)
  expect_equal(got$u, "http://x/2.jpg")
  expect_equal(length(api$calls()), 2)
  expect_true(grepl("/taxa/11", api$calls()[2]))
})

test_that("a name mismatch stops after the search request", {
  api <- fake_api(list(id = 12, name = "Xenoglossodes davidsoni",
                       default_photo = ph("cc0", "http://x/d.jpg")))
  expect_null(fetch_taxon_photo("Tetraloniella davidsoni", "species", OPEN,
                                request_fn = api$fn, sleep_fn = function() NULL))
  expect_equal(length(api$calls()), 1)
})

test_that("empty search results and request errors give NULL", {
  api <- fake_api(NULL)
  expect_null(fetch_taxon_photo("Nosuchbeeus", "genus", OPEN,
                                request_fn = api$fn, sleep_fn = function() NULL))
  expect_null(fetch_taxon_photo("Bombus", "genus", OPEN,
                                request_fn = function(url) NULL,
                                sleep_fn = function() NULL))
})

test_that("no open photo anywhere gives NULL after both requests", {
  api <- fake_api(
    search_taxon = list(id = 13, name = "Osmia", default_photo = ph(NULL)),
    detail_taxon = list(id = 13, name = "Osmia", default_photo = ph(NULL),
                        taxon_photos = list(list(photo = ph(NULL)))))
  expect_null(fetch_taxon_photo("Osmia", "genus", OPEN,
                                request_fn = api$fn, sleep_fn = function() NULL))
  expect_equal(length(api$calls()), 2)
})
