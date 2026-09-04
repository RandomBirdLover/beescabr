# The observer contrast (beeple vs interns) needs its own declared colors. Two
# figures sitting side by side -- one colored by method, one by observer -- must
# read as two different questions; sharing a palette is what makes a reader fuse
# them into one.
source(file.path("..", "..", "scripts", "analysis", "theme_beescabr.R"))

test_that("the observer contrast has declared colors, not inline hexes", {
  expect_true(exists("BEE_OBSERVER_COL"))
  expect_setequal(names(BEE_OBSERVER_COL), c("beeple", "intern"))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", BEE_OBSERVER_COL)))
})

test_that("observer colors are the non-urgent TEAL family, never the urgent red", {
  expect_true(all(BEE_OBSERVER_COL %in% BEE_TEAL))
})

test_that("observer colors do not collide with the method colors", {
  expect_equal(length(intersect(tolower(BEE_OBSERVER_COL), tolower(BEE_METHOD_COL))), 0)
})

test_that("the two observer colors are far enough apart to tell apart", {
  d <- grDevices::col2rgb(BEE_OBSERVER_COL)
  expect_gt(sum(abs(d[, "beeple"] - d[, "intern"])), 120)
})

test_that("observers have display labels a stranger can read", {
  expect_setequal(names(BEE_OBSERVER_LABEL), c("beeple", "intern"))
  expect_true(all(nzchar(BEE_OBSERVER_LABEL)))
})
