
# --- NEEDS YOU rollup: full paths, without running off the page -------------
# Every item now carries a full folder path so the operator can paste it. The
# longest is 78 characters, which is wider than the banner, so a line that does
# not fit puts its path on its own indented line instead of sprawling.
test_that("need_lines keeps a short item on one line", {
  ln <- need_lines(list(c(what = "3 duplicate IDs", where = "data/x.csv")), width = 62L)
  expect_length(ln, 1L)
  expect_match(ln, "3 duplicate IDs")
  expect_match(ln, "data/x.csv")
})

test_that("need_lines wraps a long path onto its own indented line", {
  long <- "data/specimens/specimens_clean/review/qc_review_specimen_cleanup_worklist_generated.csv"
  ln <- need_lines(list(c(what = "186 specimens need identifying", where = long)), width = 62L)
  expect_length(ln, 2L)
  expect_false(grepl(long, ln[1], fixed = TRUE))       # path is not on the what line
  expect_match(ln[2], long, fixed = TRUE)              # but it is still printed in full
  expect_match(ln[2], "^ +")                           # indented under its item
  expect_true(all(nchar(ln) <= 62L + nchar(long)))     # only the path itself may exceed
})

test_that("need_lines aligns the short items with each other", {
  ln <- need_lines(list(c(what = "a",  where = "data/one.csv"),
                        c(what = "bb", where = "data/two.csv")), width = 62L)
  expect_equal(regexpr("data/one.csv", ln[1], fixed = TRUE),
               regexpr("data/two.csv", ln[2], fixed = TRUE), ignore_attr = TRUE)
})

test_that("need_lines handles an item with no path", {
  ln <- need_lines(list(c(what = "look at the map", where = "")), width = 62L)
  expect_length(ln, 1L)
  expect_equal(trimws(ln), "▸ look at the map")
})
