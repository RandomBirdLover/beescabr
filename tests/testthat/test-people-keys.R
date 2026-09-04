# Turning a surveyor string into a person, for the acknowledgements credit tally.
#
# The per-survey record writes a name three ways: "First Last" (intern-log and
# inat-tag rows), "F Last" (specimen-record rows, the museum label), and -- in
# older data not yet regenerated -- a bare first name. All three must land on
# the same person, and none of them may land on the WRONG person: two people
# share the first name Julia, so a bare "Julia" is not creditable at all.

src("utils/people.R")

team <- data.frame(
  name  = c("Sam O'Dell", "Julia Keum", "Julia Showalter", "Cassidy Samovar"),
  first = c("Sam", "Julia", "Julia", "Cassidy "),
  last  = c("O'Dell", "Keum", "Showalter", "Samovar"),
  stringsAsFactors = FALSE
)
k <- person_name_keys(team$name, team$first, team$last)

test_that("person_name joins first and last and squishes stray whitespace", {
  expect_equal(person_name("Sam", "O'Dell"), "Sam O'Dell")
  expect_equal(person_name("Cassidy ", "Samovar"), "Cassidy Samovar")
  expect_true(is.na(person_name("", "")))
})

test_that("a full name resolves to the person", {
  expect_equal(unname(k[["sam o'dell"]]), "Sam O'Dell")
  expect_equal(unname(k[["julia keum"]]), "Julia Keum")
})

test_that("a museum-label name (F Last) resolves to the person", {
  # specimen-record rows stay as written on the specimen; they must still credit
  expect_equal(unname(k[["s o'dell"]]), "Sam O'Dell")
  expect_equal(unname(k[["c samovar"]]), "Cassidy Samovar")
})

test_that("a first name resolves only when exactly one person has it", {
  expect_equal(unname(k[["sam"]]), "Sam O'Dell")
  expect_false("julia" %in% names(k))   # two Julias: crediting either one is a coin flip
})

test_that("lookup keys are lowercase, so caller case does not matter", {
  expect_true(all(names(k) == tolower(names(k))))
})

test_that("a person with no last name still gets a key", {
  k2 <- person_name_keys("Min", "Min", "")
  expect_equal(unname(k2[["min"]]), "Min")
})

# --- the declared label code -------------------------------------------------
# A roster may DECLARE the string a person is written as on a specimen label,
# instead of leaving it to be inferred. Inference produces "x gaeta"; it can never
# produce "X. Gaeta" or a middle initial, so a declared code has to win.

test_that("a declared code becomes a key", {
  k <- person_name_keys("Xiomara Gaeta", "Xiomara", "Gaeta", code = "X. Gaeta")
  expect_equal(unname(k[["x. gaeta"]]), "Xiomara Gaeta")
})

test_that("declaring a code does not remove the inferred keys", {
  # the same person is still written "Xiomara" and "X Gaeta" in other places
  k <- person_name_keys("Xiomara Gaeta", "Xiomara", "Gaeta", code = "X. Gaeta")
  expect_equal(unname(k[["xiomara gaeta"]]), "Xiomara Gaeta")
  expect_equal(unname(k[["x gaeta"]]), "Xiomara Gaeta")
  expect_equal(unname(k[["xiomara"]]), "Xiomara Gaeta")
})

test_that("a blank or missing code adds no key", {
  # the match("", "") trap: an unfilled cell must never become a lookup entry
  k <- person_name_keys(c("Sam O'Dell", "Amy Geffre"), c("Sam", "Amy"),
                        c("O'Dell", "Geffre"), code = c("", NA))
  expect_false("" %in% names(k))
  expect_false(any(is.na(names(k))))
  expect_equal(unname(k[["s o'dell"]]), "Sam O'Dell")   # inference still covers them
})

test_that("omitting the code argument entirely still works", {
  k <- person_name_keys("Sam O'Dell", "Sam", "O'Dell")
  expect_equal(unname(k[["s o'dell"]]), "Sam O'Dell")
})

# people.R and people_ids.R were two files splitting one idea: who is this person.
# people.R turned the three name shapes ("Sam O'Dell" / "S O'Dell" / an iNat handle)
# into one identity; people_ids.R minted and resolved the stable person_id that
# identity is stored under. Neither is usable without the other, and a caller had
# to know which half held the function it wanted. One file, one subject.
test_that("people.R alone provides both the name keys and the person_id functions", {
  e <- new.env()
  old <- setwd(.beescabr_root()); on.exit(setwd(old), add = TRUE)
  sys.source("scripts/utils/people.R", e)
  for (f in c("person_name_keys", "person_name",
              "person_id_keys", "person_id_mint", "people_display"))
    expect_true(exists(f, envir = e), info = paste(f, "missing from people.R"))
})

test_that("the old split file is gone", {
  expect_false(file.exists(file.path(.beescabr_root(), "scripts/utils", "people_ids.R")))
  expect_true(file.exists(file.path(.beescabr_root(), "scripts/utils", "people.R")))
})
