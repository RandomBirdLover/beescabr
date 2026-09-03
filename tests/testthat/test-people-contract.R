# people_manual.csv / participation_generated.csv are read by five scripts. This pins what each
# one needs, so a column someone drops fails here rather than silently emptying
# the acknowledgements page or an NPS headcount.

if (!exists("PATHS")) src("config.R")
# PATHS entries are repo-root-relative; testthat runs from tests/testthat/
rp <- function(p) file.path(.beescabr_root(), p)
skip_if_no_people <- function() {
  if (is.null(PATHS$people) || !file.exists(rp(PATHS$people))) skip("people_manual.csv not present")
}

test_that("people_manual.csv carries every column its consumers read", {
  skip_if_no_people()
  p <- read.csv(rp(PATHS$people), stringsAsFactors = FALSE)
  expect_true(all(c("person_id", "first_name", "last_name", "inaturalist_username",
                    "collector_code", "determiner_code",              # museum-label forms
                    "surveyor", "identifier", "researcher",           # the three lists
                    "affiliation", "taxa_identified", "team_title") %in% names(p)))
})

test_that("every person_id is unique and non-blank", {
  skip_if_no_people()
  p <- read.csv(rp(PATHS$people), stringsAsFactors = FALSE)
  expect_equal(anyDuplicated(p$person_id), 0L)
  expect_true(all(nzchar(trimws(p$person_id))))
})

test_that("participation only ever names a person who exists", {
  skip_if_no_people()
  if (!file.exists(rp(PATHS$participation))) skip("participation_generated.csv not built yet")
  p <- read.csv(rp(PATHS$people), stringsAsFactors = FALSE)
  q <- read.csv(rp(PATHS$participation), stringsAsFactors = FALSE)
  expect_true(all(q$person_id %in% p$person_id))
  expect_true(all(q$person_id[!is.na(q$person_id)] != ""))
})

test_that("everyone with participation is flagged a surveyor", {
  skip_if_no_people()
  if (!file.exists(rp(PATHS$participation))) skip("participation_generated.csv not built yet")
  p <- read.csv(rp(PATHS$people), stringsAsFactors = FALSE)
  q <- read.csv(rp(PATHS$participation), stringsAsFactors = FALSE)
  expect_true(all(p$surveyor[match(unique(q$person_id), p$person_id)]))
})

test_that("the known headcounts still come out", {
  skip_if_no_people()
  p <- read.csv(rp(PATHS$people), stringsAsFactors = FALSE)
  expect_equal(nrow(p), 48L)
  expect_equal(sum(p$surveyor), 34L)
  expect_equal(sum(p$identifier), 16L)
  expect_equal(sum(p$researcher), 7L)
})
