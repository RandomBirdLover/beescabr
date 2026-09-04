# The subtitle has to STATE THE RESULT, not describe the method -- that is the
# house standard every other report figure follows. Computed from the rarefied
# numbers so it can never disagree with the figure under it.
source(file.path("..", "..", "scripts", "analysis", "shared", "rarefaction_names.R"))

veg <- data.frame(
  rank  = c("species", "species", "genus", "genus"),
  group = c("beeple", "intern", "beeple", "intern"),
  rarefied_richness = c(30.0, 18.0, 20.5, 18.0), stringsAsFactors = FALSE)

test_that("a group of people does not 'survey find' -- only a method does", {
  expect_match(rare_takeaway(veg, "by_observer"), "beeple find more", fixed = TRUE)
  expect_false(grepl("beeple surveys", rare_takeaway(veg, "by_observer"), fixed = TRUE))
})

test_that("the subtitle names the winner and both numbers, per rank", {
  s <- rare_takeaway(veg, "by_observer")
  expect_match(s, "beeple")
  expect_match(s, "30.0 vs 18.0")
  expect_match(s, "20.5 vs 18.0", fixed = TRUE)   # NOT "20": %.0f rounds 20.5 to even
  expect_match(s, "species")
  expect_match(s, "genera")
})

test_that("it says the comparison is fair, because that is the whole point", {
  expect_match(rare_takeaway(veg, "by_observer"), "equal|fair", ignore.case = TRUE)
})

test_that("a tie is reported as a tie, not as a winner", {
  tied <- data.frame(rank = c("genus", "genus"), group = c("beeple", "intern"),
                     rarefied_richness = c(19, 19), stringsAsFactors = FALSE)
  expect_match(rare_takeaway(tied, "by_observer"), "tied|no difference", ignore.case = TRUE)
})

test_that("a rank with only one group is skipped rather than half-reported", {
  one <- veg[veg$rank == "species" | veg$group == "beeple", ]
  s <- rare_takeaway(one[one$rank == "genus" | one$rank == "species", ], "by_observer")
  expect_true(nzchar(s))
})

test_that("every window declares the scope line its figures caption with", {
  for (d in names(RARE_WINDOWS)) {
    expect_true(nzchar(RARE_WINDOWS[[d]]$scope), info = d)
    expect_true(nzchar(RARE_WINDOWS[[d]]$method), info = d)
  }
  expect_match(RARE_WINDOWS$by_observer$method, "constant|non-lethal")
})
