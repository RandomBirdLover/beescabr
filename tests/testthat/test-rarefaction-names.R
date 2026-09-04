# Output names for the fair-window rarefaction files. "iNEXT" and "vegan" are R
# package names, not descriptions -- nobody reading the folder can tell what they
# mean, so the filenames say what the file answers instead. One helper because
# three scripts write into this folder and the names must not drift apart again.
source(file.path("..", "..", "scripts", "analysis", "rarefaction_names.R"))

# The project says "lethal" / "non-lethal" everywhere -- BEE_METHOD_COL, the scope
# captions, the efficiency figures. The rarefaction outputs say it too.
test_that("a comparison is named by what it compares, not by the grouping column", {
  expect_equal(rare_comparison("by_method"),   "lethal_vs_nonlethal")
  expect_equal(rare_comparison("method"),      "lethal_vs_nonlethal")
  expect_equal(rare_comparison("by_observer"), "beeple_vs_interns")
  expect_equal(rare_comparison("observer"),    "beeple_vs_interns")
})

test_that("report dimensions keep their existing names", {
  expect_null(rare_comparison("by_transect"))
  expect_null(rare_comparison("by_year"))
})

test_that("the figure and its table share one stem, so they sort together", {
  expect_equal(rare_out_name("method", "genus", "figure"),
               "bee_richness_lethal_vs_nonlethal_genus_rarefaction.png")
  expect_equal(rare_out_name("method", "genus", "table"),
               "bee_richness_lethal_vs_nonlethal_genus_rarefaction.csv")
})

test_that("the two supporting tables say what standardization produced them", {
  expect_equal(rare_out_name("by_observer", NULL, "estimates"),
               "bee_richness_beeple_vs_interns_effort_standardized_estimates.csv")
  expect_equal(rare_out_name("by_observer", NULL, "rarefied"),
               "bee_richness_beeple_vs_interns_rarefied_to_smallest_group.csv")
})

test_that("no output name carries an R package name", {
  nm <- c(rare_out_name("method", "genus", "figure"), rare_out_name("method", "genus", "table"),
          rare_out_name("observer", "species", "figure"), rare_out_name("observer", NULL, "estimates"),
          rare_out_name("observer", NULL, "rarefied"))
  expect_false(any(grepl("inext|vegan", nm, ignore.case = TRUE)))
})

test_that("an unknown dimension or kind is refused rather than guessed", {
  expect_error(rare_out_name("by_transect", "genus", "figure"), "report dimension")
  expect_error(rare_out_name("method", "genus", "sideways"), "kind")
})
