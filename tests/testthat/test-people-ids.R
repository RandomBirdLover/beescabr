# person_id: assign once, resolve forever.
#
# The id is opaque and stable -- never derived from a name, so a surname change,
# a new iNat handle, or a differently-spelled museum label breaks no join. Same
# rule taxon_id follows (CLAUDE.md).

src("utils/people_ids.R")

.people <- data.frame(
  person_id            = c("p001", "p002", "p003", "p004"),
  first_name           = c("Sam",      "Cindy",        "John",       "Julia"),
  last_name            = c("O'Dell",   "Pencek",       "Ascher",     "Keum"),
  inaturalist_username = c("wranglebees", "carrotpeople", "johnascher", "julaliak"),
  collector_code       = c("S O'Dell", "",             "",           ""),
  determiner_code      = c("",         "",             "JS Ascher",  ""),
  stringsAsFactors = FALSE)

test_that("minting starts at p001 and pads to three digits", {
  expect_equal(person_id_mint(3), c("p001", "p002", "p003"))
})

test_that("minting continues past the highest id ever issued, never reusing one", {
  # p002 deleted from the file must NOT come back around to a different human
  expect_equal(person_id_mint(2, c("p001", "p003")), c("p004", "p005"))
  expect_equal(person_id_mint(1, c("p010", "p002")), "p011")
})

test_that("every written form of a person resolves to their id", {
  expect_equal(person_id_of("Sam O'Dell",  .people), "p001")   # full name
  expect_equal(person_id_of("S O'Dell",    .people), "p001")   # museum label
  expect_equal(person_id_of("wranglebees", .people), "p001")   # iNat handle
  expect_equal(person_id_of("Sam",         .people), "p001")   # unique first name
  expect_equal(person_id_of("JS Ascher",   .people), "p003")   # determiner code
})

test_that("resolution ignores case and stray whitespace", {
  expect_equal(person_id_of(c("  WRANGLEBEES ", "sam o'dell"), .people), c("p001", "p001"))
})

test_that("a form that matches nobody is NA, never a guess", {
  expect_true(is.na(person_id_of("Wilhelmina Sprocket", .people)))
  expect_true(is.na(person_id_of("CSBI Interns", .people)))
  expect_true(is.na(person_id_of("", .people)))
})

test_that("a surveyors-style cell yields the ids it names", {
  expect_equal(person_ids_in("Cindy Pencek, S O'Dell", .people), c("p002", "p001"))
  expect_equal(person_ids_in("A Mendez Lozano & A Geffre; CSBI Interns", .people), character(0))
})

test_that("ids stay character, so no integer coercion can mis-credit anyone", {
  # match(26L, "26") is NA while 26 == "26" is TRUE -- a numeric id fails silently
  expect_type(person_id_mint(1), "character")
  expect_true(all(grepl("^p[0-9]{3}$", person_id_mint(5))))
})
