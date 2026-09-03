# IUCN REQUIRES a citation with the Red List version ("Full acknowledgement and citation
# needs to be given for using the API"). The version is captured at refresh time so the
# public pages never have to make a live call, and never guess.
# iNaturalist has no such requirement, so it is credited, not formally cited.

src("publish/citations.R")

test_that("the IUCN citation carries the version and the access date", {
  s <- iucn_citation("2026-1", "2026-09-02")
  expect_match(s, "IUCN 2026", fixed = TRUE)
  expect_match(s, "Version 2026-1", fixed = TRUE)
  expect_match(s, "iucnredlist.org", fixed = TRUE)
  expect_match(s, "2026-09-02", fixed = TRUE)
})

test_that("the citation year comes from the version, not from today", {
  # a page rebuilt in 2027 from a 2026 pull must still cite IUCN 2026
  s <- iucn_citation("2026-1", "2027-03-14")
  expect_match(s, "IUCN 2026", fixed = TRUE)
})

test_that("an unknown version yields NO citation rather than a wrong one", {
  expect_equal(iucn_citation("", "2026-09-02"), "")
  expect_equal(iucn_citation(NA, "2026-09-02"), "")
})

test_that("iNaturalist is credited with an access date", {
  s <- inat_citation("2026-08-21")
  expect_match(s, "iNaturalist", fixed = TRUE)
  expect_match(s, "inaturalist.org", fixed = TRUE)
  expect_match(s, "2026-08-21", fixed = TRUE)
})

test_that("the stored version round-trips through the cache file", {
  f <- tempfile()
  citation_save_version(f, "2026-1")
  expect_equal(citation_read_version(f), "2026-1")
})

test_that("a missing version file reads as empty, not as an error", {
  expect_equal(citation_read_version(tempfile()), "")
})
