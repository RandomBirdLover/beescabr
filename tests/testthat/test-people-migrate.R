# Merging the three rosters into one people table.
#
# The three files re-state the same person; six people are in more than one.
# Merging must join them into ONE row without ever silently choosing between
# two different values for the same fact.

src("project_info/build_people_roster.R")

.sr <- data.frame(year = c(2021, 2022, 2021), inaturalist_username = c("wranglebees", "wranglebees", ""),
                  collector_code = c("S O'Dell", "S O'Dell", ""), first_name = c("Sam", "Sam", "Lydia"),
                  last_name = c("O'Dell", "O'Dell", "Duran"), email = c("sam@a.com", "sam@a.com", ""),
                  role = "intern", stringsAsFactors = FALSE)
.ir <- data.frame(first_name = "Sam", last_name = "O'Dell", inaturalist_username = "wranglebees",
                  determiner_code = "S O'Dell", email = "sam@a.com", stringsAsFactors = FALSE)
.tr <- data.frame(first_name = "Sam", last_name = "O'Dell", inaturalist_username = "wranglebees",
                  role = "Entomologist", email = "sam@a.com", stringsAsFactors = FALSE)

test_that("a person in all three rosters becomes exactly one row", {
  p <- people_from_rosters(.sr, .ir, .tr)
  expect_equal(nrow(p), 2)                              # Sam + Lydia
  expect_equal(sum(p$last_name == "O'Dell"), 1)
})

test_that("each roster contributes its own facts to that one row", {
  p <- people_from_rosters(.sr, .ir, .tr)
  s <- p[p$last_name == "O'Dell", ]
  expect_equal(s$collector_code, "S O'Dell")            # from the surveyor roster
  expect_equal(s$determiner_code, "S O'Dell")           # from the identifier roster
  expect_equal(s$team_title, "Entomologist")            # from the research-team roster
  expect_true(s$surveyor && s$identifier && s$researcher)
})

test_that("someone in only one roster gets only that flag", {
  p <- people_from_rosters(.sr, .ir, .tr)
  l <- p[p$last_name == "Duran", ]
  expect_true(l$surveyor)
  expect_false(l$identifier); expect_false(l$researcher)
})

test_that("ids are minted p001 upward, one per person", {
  p <- people_from_rosters(.sr, .ir, .tr)
  expect_equal(sort(p$person_id), c("p001", "p002"))
  expect_equal(anyDuplicated(p$person_id), 0L)
})

test_that("a person-year roster does not duplicate the person", {
  # Sam has two surveyor rows (2021, 2022); that must not make two people
  expect_equal(sum(people_from_rosters(.sr, .ir, .tr)$last_name == "O'Dell"), 1)
})

test_that("two different values for one fact are REPORTED, never silently picked", {
  ir2 <- .ir; ir2$email <- "sam@university.edu"
  p <- people_from_rosters(.sr, ir2, .tr)
  cf <- attr(p, "conflicts")
  expect_true(is.data.frame(cf) && nrow(cf) == 1)
  expect_equal(cf$field, "email")
  expect_true(grepl("sam@a.com", cf$values) && grepl("sam@university.edu", cf$values))
})

test_that("no conflicts reported when the rosters agree", {
  expect_equal(nrow(attr(people_from_rosters(.sr, .ir, .tr), "conflicts")), 0)
})
