# One tidy estimates table per comparison, instead of three raw iNEXT dumps per rank.
# AsyEst and the two estimateD() tables are the SAME grain -- assemblage x Hill q --
# so they belong in one long table with a `basis` column, not three files.
source(file.path("..", "..", "scripts", "analysis", "inext_estimates.R"))

asy <- data.frame(
  Assemblage = c("beeple", "beeple", "beeple", "intern", "intern", "intern"),
  Diversity  = rep(c("Species richness", "Shannon diversity", "Simpson diversity"), 2),
  Observed   = c(40, 20, 12, 30, 15, 9),
  Estimator  = c(52, 22, 13, 38, 16, 10),
  `s.e.`     = c(3, 1, 0.5, 2, 1, 0.4),
  LCL        = c(46, 20, 12, 34, 14, 9),
  UCL        = c(58, 24, 14, 42, 18, 11),
  check.names = FALSE, stringsAsFactors = FALSE)

estd <- function(sc) data.frame(
  Assemblage = c("beeple", "beeple", "beeple", "intern", "intern", "intern"),
  m = rep(500, 6), Method = "Rarefaction", Order.q = rep(0:2, 2), SC = sc,
  qD = c(35, 18, 11, 33, 16, 10), qD.LCL = c(32, 17, 10, 30, 15, 9),
  qD.UCL = c(38, 19, 12, 36, 17, 11), stringsAsFactors = FALSE)

test_that("the three iNEXT tables collapse into one long table keyed by basis", {
  out <- inext_estimates_tidy(asy, estd(0.90), estd(0.95), rank = "genus")
  expect_equal(nrow(out), 18)                       # 3 bases x 2 groups x 3 q
  expect_setequal(unique(out$basis), c("asymptotic", "equal_size", "equal_coverage"))
  expect_true(all(out$rank == "genus"))
  expect_setequal(unique(out$q), 0:2)
  expect_setequal(unique(out$group), c("beeple", "intern"))
})

test_that("the Diversity label becomes a numeric Hill q, not a sentence", {
  out <- inext_estimates_tidy(asy, estd(0.90), estd(0.95), rank = "species")
  a <- out[out$basis == "asymptotic" & out$group == "beeple", ]
  expect_equal(a$q[order(a$q)], 0:2)
  expect_equal(a$diversity[a$q == 0], 52)           # Estimator, not Observed
  expect_equal(a$observed[a$q == 0], 40)            # observed kept alongside
  expect_equal(a$se[a$q == 0], 3)
})

test_that("standardized rows carry their effort and coverage, asymptotic rows do not", {
  out <- inext_estimates_tidy(asy, estd(0.90), estd(0.95), rank = "genus")
  expect_equal(unique(out$coverage[out$basis == "equal_size"]), 0.90)
  expect_equal(unique(out$coverage[out$basis == "equal_coverage"]), 0.95)
  expect_equal(unique(out$n[out$basis == "equal_size"]), 500)
  expect_true(all(is.na(out$n[out$basis == "asymptotic"])))
  expect_true(all(is.na(out$observed[out$basis != "asymptotic"])))
})

test_that("ranks stack into one table and column order is stable", {
  both <- rbind(inext_estimates_tidy(asy, estd(0.9), estd(0.95), "species"),
                inext_estimates_tidy(asy, estd(0.9), estd(0.95), "genus"))
  expect_equal(nrow(both), 36)
  expect_equal(names(both), c("rank", "basis", "group", "q", "n", "coverage",
                              "observed", "diversity", "se", "lcl", "ucl"))
})
