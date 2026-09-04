# identification_counts_generated.csv -- how many identifications each person made
# ON CABRILLO RECORDS.
#
# id_count used to be typed into the roster by hand: a number that was true the day
# it was entered and drifted every time anyone identified a bee. It orders the
# identifier list on the acknowledgements page, and the survey team beside it is
# already ranked by a COUNTED figure, so this makes the two consistent.
#
# Scope matters more than it looks. The observation cache is county-wide (119,928
# records), and counting all of it gave John Ascher 47,596 -- true, but not about
# Cabrillo. Counting only observations inside the park boundary gives 6,256, close
# to the 6,153 someone once typed, which is the number that was always intended.

src("project_info/build_identification_counts.R")

.people <- data.frame(
  person_id            = c("p001", "p002", "p003"),
  first_name           = c("Jessica", "Patricia", "Taro"),
  last_name            = c("Mullins", "Simpson", "Katayama"),
  inaturalist_username = c("jessmullins", "patsimpson2000", ""),
  stringsAsFactors = FALSE)

.ids <- data.frame(
  login = c("jessmullins", "patsimpson2000", "patsimpson2000", "astranger", "JESSMULLINS"),
  kind  = c("bee",         "bee",            "plant",          "bee",       "bee"),
  n     = c(10L,           4L,               6L,               99L,         5L),
  stringsAsFactors = FALSE)

test_that("identifications are tallied per person, split bee and plant", {
  out <- identification_counts(.ids, .people)
  p <- out[out$person_id == "p002", ]
  expect_equal(p$n_bee, 4L); expect_equal(p$n_plant, 6L); expect_equal(p$total, 10L)
})

test_that("a handle is matched however it is capitalised", {
  # iNaturalist logins are case-insensitive; two spellings are one person
  out <- identification_counts(.ids, .people)
  expect_equal(out$n_bee[out$person_id == "p001"], 15L)
})

test_that("someone not in people.csv is ignored, never invented", {
  out <- identification_counts(.ids, .people)
  expect_false(any(out$person_id %in% c(NA, "", "astranger")))
  expect_equal(nrow(out), 3L)
})

test_that("a person with no identifications gets zero, not a missing row", {
  # they still appear in the list, just last -- dropping them would remove a credit
  out <- identification_counts(.ids, .people)
  t <- out[out$person_id == "p003", ]
  expect_equal(t$total, 0L)
})

test_that("no identifications at all yields a zero row per person, not an error", {
  out <- identification_counts(.ids[0, ], .people)
  expect_equal(nrow(out), 3L)
  expect_true(all(out$total == 0L))
})

test_that("the output carries exactly the columns the page reads", {
  expect_equal(names(identification_counts(.ids, .people)),
               c("person_id", "n_bee", "n_plant", "total"))
})
