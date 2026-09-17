library(testthat)

# The end-of-run rollup listed the 2 duplicate specimen IDs but not the 186 specimens
# with no identification at all -- by far the biggest job outstanding, and the only one
# that needs someone at a microscope. It was mentioned once mid-run, in a note that
# said "the raw .xlsx" (there are 19) and pointed at a TODO in a code comment.
#
# It is its own line, not folded in with the iNaturalist ids: those are checklist names
# with no iNat number, which is a different job in a different place.
src("utils/console.R")

test_that("specimens needing identification are counted separately from anything else", {
  expect_match(needs_specimen_ids(186L)[["what"]], "186 specimens need identifying", fixed = TRUE)
  expect_match(needs_specimen_ids(186L)[["where"]],
               "qc_review_specimen_cleanup_worklist_generated.csv", fixed = TRUE)
})

test_that("one reads as one", {
  expect_match(needs_specimen_ids(1L)[["what"]], "1 specimen needs identifying", fixed = TRUE)
})

test_that("nothing outstanding queues nothing", {
  expect_null(needs_specimen_ids(0L))
})

test_that("the rollup keeps the four jobs apart", {
  bx_need_reset()
  bx_need("186 specimens need identifying", "a.csv")
  bx_need("17 bee names need an iNat id", "b.csv")
  bx_need("2 duplicate specimen IDs", "c.csv")
  bx_need("Send 9 surveyors their maps", "d/")
  said <- character(0)
  withCallingHandlers(bx_need_print(), message = function(m) {
    said <<- c(said, conditionMessage(m)); invokeRestart("muffleMessage") })
  txt <- paste(said, collapse = "")
  for (x in c("186 specimens need identifying", "17 bee names need an iNat id",
              "2 duplicate specimen IDs", "Send 9 surveyors their maps"))
    expect_match(txt, x, fixed = TRUE, info = x)
})
