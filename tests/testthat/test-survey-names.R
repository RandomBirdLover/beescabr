# Surveyor names in the per-survey record.
#
# The `surveyors` column mixed two conventions: intern-log and inat-tag rows
# carried a bare first name ("Sam"), specimen-record rows carried the museum
# label ("S O'Dell"). Two people share the first name Julia (Keum, a 2024
# intern; Showalter, a 2025-26 beeple), so a first name is NOT an identity --
# it only resolves inside its roster year.
#
# Rule: intern-log and inat-tag rows say "First Last". specimen-record rows are
# left exactly as written, because that string is what is on the specimen label.

src("project_info/finding_survey_dates.R")

roster <- data.frame(
  year       = c(2021, 2021, 2024, 2025, 2022),
  first_name = c("Sam", "Cassidy ", "Julia", "Julia", "Megan"),   # note the typo'd trailing space
  last_name  = c("O'Dell", "Samovar", "Keum", "Showalter", "Hewitt"),
  stringsAsFactors = FALSE
)

test_that("a bare first name is expanded to the roster's full name", {
  expect_equal(sd_full_names("Sam", 2021, roster), "Sam O'Dell")
})

test_that("every name in a multi-surveyor cell is expanded", {
  expect_equal(sd_full_names("Cassidy, Sam", 2021, roster), "Cassidy Samovar, Sam O'Dell")
})

test_that("a first name resolves within its own roster year, not across years", {
  # the bug this guards: two Julias. Expanding by first name alone credits the
  # wrong person for a whole season.
  expect_equal(sd_full_names("Julia", 2024, roster), "Julia Keum")
  expect_equal(sd_full_names("Julia", 2025, roster), "Julia Showalter")
})

test_that("a name that does not resolve is left exactly as written", {
  # never guess: an unknown token stays put so a human can see it
  expect_equal(sd_full_names("Wilhelmina Sprocket", 2021, roster), "Wilhelmina Sprocket")
  expect_equal(sd_full_names("Julia", 2021, roster), "Julia")   # no Julia on the 2021 roster
})

test_that("expansion is idempotent -- a full name survives a second pass", {
  once  <- sd_full_names("Cassidy, Sam", 2021, roster)
  expect_equal(sd_full_names(once, 2021, roster), once)
})

test_that("blank and NA cells pass through untouched", {
  expect_equal(sd_full_names(c("", NA), c(2021, 2021), roster), c("", NA))
})

test_that("it is vectorised over rows, each with its own year", {
  expect_equal(sd_full_names(c("Sam", "Julia", "Julia"), c(2021, 2024, 2025), roster),
               c("Sam O'Dell", "Julia Keum", "Julia Showalter"))
})

# --- museum-label names ------------------------------------------------------
# The specimen SPREADSHEET keeps "S O'Dell" -- that is what is written on the pin
# and it is never rewritten. But master_per_survey_info_generated.csv is a generated survey
# table, so the people in it are named in full, whichever source the row came from.

roster2 <- data.frame(
  year       = c(2021, 2021, 2021, 2021),
  first_name = c("Sam", "Amy", "Anahi", "Min"),
  last_name  = c("O'Dell", "Geffre", "Mendez Lozano", "Han"),
  stringsAsFactors = FALSE
)

test_that("a museum-label name expands to the full name", {
  expect_equal(sd_full_names("S O'Dell", 2021, roster2), "Sam O'Dell")
})

test_that("a multi-word surname on a label still resolves", {
  # "A Mendez Lozano" -- the surname is two words, which a naive last-token split loses
  expect_equal(sd_full_names("A Mendez Lozano", 2021, roster2), "Anahi Mendez Lozano")
})

test_that("semicolons and ampersands survive expansion exactly as written", {
  # the separators carry meaning on a specimen row: "&" pairs collectors, ";" splits groups
  expect_equal(sd_full_names("M Han & C Samovar", 2021, roster2),
               "Min Han & C Samovar")                       # Samovar is not on this fixture roster
  expect_equal(sd_full_names("S O'Dell; CSBI Interns", 2021, roster2),
               "Sam O'Dell; interns")
})

test_that("a group label becomes plain 'interns' in the NAME column", {
  # The label "CSBI Interns" survives verbatim in specimen_collector; this column
  # names people, and which interns netted that day was never recorded.
  expect_equal(sd_full_names("CSBI Interns", 2021, roster2), "interns")
  expect_equal(sd_full_names("csbi interns", 2021, roster2), "interns")
})

test_that("a real person is never swallowed by the group rule", {
  # matched on the whole token, not as a substring
  expect_equal(sd_full_names("Sam O'Dell", 2021, roster2), "Sam O'Dell")
})

test_that("a collector_code declared on the roster resolves the label", {
  # inference produces "x gaeta"; it can never produce the period. The roster declares it.
  r <- data.frame(year = 2021, first_name = "Xiomara", last_name = "Gaeta",
                  collector_code = "X. Gaeta", stringsAsFactors = FALSE)
  expect_equal(sd_full_names("X. Gaeta", 2021, r), "Xiomara Gaeta")
})

test_that("a roster with no collector_code column still works", {
  expect_equal(sd_full_names("S O'Dell", 2021, roster2), "Sam O'Dell")
})

# --- calendar windows name a beeple by FIRST NAME ----------------------------
# The beeple calendar writes "Julia", and the review report has to say which
# Julia. Two are on the roster (Keum, a 2024 intern; Showalter, a 2025-26
# beeple), and the derived roster spans every year, so the first name alone is
# not enough. Prefer whoever actually has a tagged survey that year, and when
# that still does not settle it, say nothing rather than name the wrong person.

.ros <- data.frame(
  year = c(2025, 2025, 2025), first_name = c("Julia", "Julia", "Cindy"),
  uname = c("julaliak", "jwanderer6", "carrotpeople"), stringsAsFactors = FALSE)

test_that("a unique first name resolves straight away", {
  expect_equal(sd_calendar_uname(2025, "Cindy", .ros, evidence = character(0)), "carrotpeople")
})

test_that("a shared first name is settled by who actually surveyed that year", {
  expect_equal(sd_calendar_uname(2025, "Julia", .ros, evidence = "jwanderer6"), "jwanderer6")
  expect_equal(sd_calendar_uname(2025, "Julia", .ros, evidence = "julaliak"),  "julaliak")
})

test_that("a shared first name with no evidence either way stays blank", {
  # naming one of them would be a coin flip; the window still surfaces for review
  expect_true(is.na(sd_calendar_uname(2025, "Julia", .ros, evidence = character(0))))
})

test_that("a shared first name where BOTH surveyed stays blank", {
  expect_true(is.na(sd_calendar_uname(2025, "Julia", .ros,
                                      evidence = c("julaliak", "jwanderer6"))))
})

test_that("a name nobody on the roster has is NA", {
  expect_true(is.na(sd_calendar_uname(2025, "Nobody", .ros, evidence = character(0))))
})

test_that("only a beeple can match a beeple calendar window", {
  # the calendar IS the beeple schedule -- an intern was never on it, so they must
  # not be offered as the answer even when they share the first name
  r <- data.frame(year = 2024, first_name = c("Julia", "Julia"),
                  uname = c("julaliak", "jwanderer6"),
                  role = c("intern", "beeple"), stringsAsFactors = FALSE)
  expect_equal(sd_calendar_uname(2024, "Julia", r, evidence = character(0)), "jwanderer6")
})

test_that("a window naming only an intern resolves to nobody", {
  r <- data.frame(year = 2024, first_name = "Grant", uname = "grant65",
                  role = "intern", stringsAsFactors = FALSE)
  expect_true(is.na(sd_calendar_uname(2024, "Grant", r, evidence = "grant65")))
})
