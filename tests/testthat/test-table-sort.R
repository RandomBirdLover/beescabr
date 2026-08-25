# Sorted-column highlighting shared by every sortable table on the site
# (both field guides + least-sampled bees). The marker JS and the CSS both live
# in theme_beescabr.R so the three pages cannot drift apart.

src("analysis/theme_beescabr.R")

test_that("bee_sort_mark_js defines the marker and is safe inside a single-quoted R string", {
  js <- bee_sort_mark_js()
  expect_true(grepl("function beeMarkSort", js, fixed = TRUE))
  # these files embed their JS in single-quoted R strings, so an apostrophe would
  # terminate the literal and break the build
  expect_false(grepl("'", js, fixed = TRUE))
})

test_that("the marker highlights the clicked header and tags its column", {
  js <- bee_sort_mark_js()
  expect_true(grepl('classList.add("sorted")', js, fixed = TRUE))
  expect_true(grepl('classList.add("sortcol")', js, fixed = TRUE))
})

test_that("the marker clears any previous highlight before applying the new one", {
  js <- bee_sort_mark_js()
  expect_true(grepl('classList.remove("sorted")', js, fixed = TRUE))
  expect_true(grepl('classList.remove("sortcol")', js, fixed = TRUE))
  # clearing must happen before adding, or clicking a new column leaves two highlighted
  expect_true(regexpr('classList.remove("sorted")', js, fixed = TRUE) <
              regexpr('classList.add("sorted")',    js, fixed = TRUE))
})

test_that("the marker records direction as aria-sort for screen readers and the arrow", {
  js <- bee_sort_mark_js()
  expect_true(grepl("aria-sort", js, fixed = TRUE))
  expect_true(grepl("ascending", js, fixed = TRUE))
  expect_true(grepl("descending", js, fixed = TRUE))
})

test_that("bee_table_css styles the sorted header, its arrow, and the sorted column", {
  css <- bee_table_css()
  expect_true(grepl("th.sorted", css, fixed = TRUE))
  expect_true(grepl('th.sorted:after', css, fixed = TRUE))                 # direction arrow
  expect_true(grepl('[aria-sort="descending"]', css, fixed = TRUE))        # flipped arrow
  expect_true(grepl("td.sortcol", css, fixed = TRUE))                      # column tint
})

test_that("the sorted-column tint outranks the zebra striping it sits on top of", {
  css <- bee_table_css()
  # zebra rule is "tbody tr:nth-child(even)"; the tint must be more specific AND later
  zebra <- regexpr("tbody tr:nth-child(even)", css, fixed = TRUE)
  tint  <- regexpr("td.sortcol", css, fixed = TRUE)
  expect_true(zebra > 0 && tint > zebra)
  expect_true(grepl("tbody tr td.sortcol", css, fixed = TRUE))   # 3 elements + 1 class beats 1 pseudo-class
})
