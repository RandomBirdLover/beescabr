library(testthat)

# Landing-page builder lives in the R publish module. Sourcing it must NOT run
# publish_pages() (that does file I/O + network-free copies) -- it is guarded so
# build_landing_html() can be unit-tested in isolation.
src("publish/publish_pages.R")

test_that("build_landing_html renders one card per page with its fields", {
  cards <- list(
    list(out = "foo.html", title = "Foo Guide", blurb = "All about foo.", icon = "\U0001F41D", tag = "Field guide"),
    list(out = "bar.html", title = "Bar Map",   blurb = "Where the bars are.", icon = "\U0001F5FA", tag = "Map"))
  h <- build_landing_html(cards, "2026-08-13", "abc12345")
  expect_true(grepl('href="./foo.html"', h, fixed = TRUE))
  expect_true(grepl("Foo Guide", h, fixed = TRUE))
  expect_true(grepl("All about foo.", h, fixed = TRUE))
  expect_true(grepl('href="./bar.html"', h, fixed = TRUE))
  expect_true(grepl("Bar Map", h, fixed = TRUE))
  # both cards present
  expect_equal(length(gregexpr('class="card"', h, fixed = TRUE)[[1]]), 2L)
})

test_that("build_landing_html interpolates the date and hero cache-buster", {
  h <- build_landing_html(list(), "2026-08-13", "deadbeef")
  expect_true(grepl("hero.jpg?v=deadbeef", h, fixed = TRUE))
  expect_true(grepl("Based on data available as of 2026-08-13.", h, fixed = TRUE))
})

test_that("build_landing_html links the Acknowledgements page and is a full document", {
  h <- build_landing_html(list(), "x", "1")
  expect_true(grepl("./acknowledgements.html", h, fixed = TRUE))
  expect_true(startsWith(h, "<!doctype html>"))
  expect_true(grepl("</html>", h, fixed = TRUE))
})

test_that("build_landing_html credits the funders with their official links", {
  h <- build_landing_html(list(), "x", "1")
  # Southern California Research Learning Center -- major funder -> its NPS RLC page
  expect_true(grepl("Southern California Research Learning Center", h, fixed = TRUE))
  expect_true(grepl('href="https://www.nps.gov/rlc/southerncal/index.htm"', h, fixed = TRUE))
  # Conservation Legacy + Cabrillo National Monument Foundation
  expect_true(grepl("Conservation Legacy", h, fixed = TRUE))
  expect_true(grepl('href="https://conservationlegacy.org/"', h, fixed = TRUE))
  expect_true(grepl("Cabrillo National Monument Foundation", h, fixed = TRUE))
  expect_true(grepl('href="https://www.cnmf.org/"', h, fixed = TRUE))
})

test_that("group_pages_by_tag groups cards by tag, sections in first-appearance order", {
  pages <- list(
    list(out = "a.html", tag = "Field guide"),
    list(out = "b.html", tag = "Map"),
    list(out = "c.html", tag = "Explore"),
    list(out = "d.html", tag = "Map"),
    list(out = "e.html", tag = "Field guide"))
  g <- group_pages_by_tag(pages)
  expect_equal(vapply(g, `[[`, "", "out"), c("a.html", "e.html", "b.html", "d.html", "c.html"))
  expect_equal(vapply(g, `[[`, "", "tag"),
               c("Field guide", "Field guide", "Map", "Map", "Explore"))
})

test_that("group_pages_by_tag keeps within-section manifest order and handles edge cases", {
  pages <- list(
    list(out = "1.html", tag = "Map"),
    list(out = "2.html", tag = "Map"),
    list(out = "3.html", tag = "Map"))
  expect_equal(vapply(group_pages_by_tag(pages), `[[`, "", "out"),
               c("1.html", "2.html", "3.html"))         # stable within a section
  expect_equal(group_pages_by_tag(list()), list())
  one <- list(list(out = "x.html", tag = "Summary"))
  expect_equal(group_pages_by_tag(one), one)
})
