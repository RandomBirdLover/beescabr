# The first time someone builds the site, tell them what GitHub Pages needs.
#
# The deploy step assumes Pages is already serving docs/ from main, and that you can
# push to origin. Neither travels with a clone, and neither is mentioned anywhere the
# person running the pipeline will look. So the pipeline says it, once, when there is
# no site yet -- not buried in a guide.
src("website/setup_notice.R")

test_that("a first build explains what Pages needs", {
  msg <- website_setup_notice(has_site = FALSE, remote = "https://github.com/someone/beescabr")
  txt <- paste(msg, collapse = "\n")
  expect_true(length(msg) > 0)
  expect_match(txt, "Settings", fixed = TRUE)
  expect_match(txt, "/docs", fixed = TRUE)
  expect_match(txt, "main", fixed = TRUE)
})

test_that("it names the repository the deploy would actually push to", {
  msg <- website_setup_notice(has_site = FALSE, remote = "https://github.com/taro/beescabr")
  expect_match(paste(msg, collapse = "\n"), "taro/beescabr", fixed = TRUE)
})

test_that("it predicts the URL the site will appear at", {
  msg <- website_setup_notice(has_site = FALSE, remote = "https://github.com/taro/beescabr")
  expect_match(paste(msg, collapse = "\n"), "taro.github.io/beescabr", fixed = TRUE)
})

test_that("an ssh remote is understood too", {
  msg <- website_setup_notice(has_site = FALSE, remote = "git@github.com:taro/beescabr.git")
  expect_match(paste(msg, collapse = "\n"), "taro.github.io/beescabr", fixed = TRUE)
})

test_that("once a site exists it says nothing", {
  expect_null(website_setup_notice(has_site = TRUE, remote = "https://github.com/taro/beescabr"))
})

test_that("no remote at all is handled without crashing", {
  msg <- website_setup_notice(has_site = FALSE, remote = "")
  expect_true(length(msg) > 0)
  expect_match(paste(msg, collapse = "\n"), "no git remote", ignore.case = TRUE)
})

test_that("it says the original repo is the ORIGINAL, and to fork it", {
  msg <- website_setup_notice(FALSE, "https://github.com/RandomBirdLover/beescabr")
  txt <- paste(msg, collapse = "\n")
  expect_match(txt, "fork", ignore.case = TRUE)
  expect_match(txt, "RandomBirdLover/beescabr", fixed = TRUE)
})

test_that("a fork is recognised as already yours, and names its upstream", {
  msg <- website_setup_notice(FALSE, "https://github.com/taro/beescabr")
  txt <- paste(msg, collapse = "\n")
  expect_match(txt, "taro/beescabr", fixed = TRUE)
  expect_match(txt, "RandomBirdLover/beescabr", fixed = TRUE)   # names where it came from
})
