# The observer contrast (beeple vs interns) needs its own declared colors. Two
# figures sitting side by side -- one colored by method, one by observer -- must
# read as two different questions; sharing a palette is what makes a reader fuse
# them into one.
source(file.path("..", "..", "scripts", "analysis", "shared", "theme_beescabr.R"))

test_that("the observer contrast has declared colors, not inline hexes", {
  expect_true(exists("BEE_OBSERVER_COL"))
  expect_setequal(names(BEE_OBSERVER_COL), c("beeple", "intern"))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", BEE_OBSERVER_COL)))
})

test_that("observer colors are two DIFFERENT hues, not two stops of one ramp", {
  # two teals differing only in lightness read as washed out beside the method
  # figure's red/periwinkle. Yellow vs purple is the high-contrast pair, and it
  # survives red-green colour blindness, which green-vs-orange does not.
  h <- grDevices::rgb2hsv(grDevices::col2rgb(BEE_OBSERVER_COL))["h", ]
  expect_gt(min(abs(diff(h)), 1 - abs(diff(h))), 0.15)
})

test_that("observer colors stay clear of the RARE/urgent red family", {
  expect_equal(length(intersect(tolower(BEE_OBSERVER_COL), tolower(BEE_RARE))), 0)
})

test_that("observer colors do not collide with the method colors", {
  expect_equal(length(intersect(tolower(BEE_OBSERVER_COL), tolower(BEE_METHOD_COL))), 0)
})

test_that("the two observer colors are far enough apart to tell apart", {
  d <- grDevices::col2rgb(BEE_OBSERVER_COL)
  expect_gt(sum(abs(d[, "beeple"] - d[, "intern"])), 200)
})

test_that("observers have display labels a stranger can read", {
  expect_setequal(names(BEE_OBSERVER_LABEL), c("beeple", "intern"))
  expect_true(all(nzchar(BEE_OBSERVER_LABEL)))
})

# TRANSECT and METHOD each carry a _LT tint for the lighter half of a split bar
# (the "still unresolved" side). Observer had no tint, so an observer bar that
# needed one would have had a hex invented for it at the call site.
test_that("the observer contrast has a light tint, built like the others", {
  expect_true(exists("BEE_OBSERVER_COL_LT"))
  expect_setequal(names(BEE_OBSERVER_COL_LT), names(BEE_OBSERVER_COL))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", BEE_OBSERVER_COL_LT)))
})

test_that("each observer tint is the same hue as its full color, only lighter", {
  full <- grDevices::rgb2hsv(grDevices::col2rgb(BEE_OBSERVER_COL))
  lt   <- grDevices::rgb2hsv(grDevices::col2rgb(BEE_OBSERVER_COL_LT))
  # same hue: a tint that shifts hue reads as a different series, not a lighter one
  expect_lt(max(pmin(abs(full["h", ] - lt["h", ]), 1 - abs(full["h", ] - lt["h", ]))), 0.02)
  expect_true(all(lt["v", ] > full["v", ] | lt["s", ] < full["s", ]))
})
