# DATA_ACCESS.md is the doc a newcomer reads while setting up, and it lists the
# run-mode menu the cleaning pipeline shows. It had drifted: the doc still told
# people to set BEESCABR_SKIP_INGEST by hand, which ingest_mode.R treats as
# "operator knows what they are doing" and silently SKIPS the menu for. Following
# the instructions turned off the thing written to help the reader.
#
# So the menu and the doc are checked against each other.
src("utils/ingest_mode.R")

.doc <- function()
  paste(readLines(file.path("..", "..", "dev-docs", "DATA_ACCESS.md"), warn = FALSE),
        collapse = "\n")

test_that("every run mode is listed in DATA_ACCESS.md, by number and label", {
  doc <- .doc()
  for (k in names(INGEST_MODES)) {
    lab <- INGEST_MODES[[k]]$label
    expect_match(doc, paste0("\\| ", k, " \\| \\*{0,2}", lab), perl = TRUE,
                 info = paste("run mode", k, "-", lab, "- not in the doc"))
  }
})

test_that("the doc lists no run mode the pipeline does not offer", {
  rows <- regmatches(.doc(), gregexpr("^\\| ([0-9]+) \\|", .doc(), perl = TRUE))[[1]]
  nums <- gsub("[^0-9]", "", rows)
  expect_true(all(nums %in% names(INGEST_MODES)),
              info = paste("doc lists mode(s) that do not exist:",
                           paste(setdiff(nums, names(INGEST_MODES)), collapse = ", ")))
})

test_that("the doc does not tell a reader to set an ingest flag by hand", {
  # Setting one is what makes ingest_mode.R skip the menu without saying so.
  body <- sub("(?s)^.*## Running it yourself", "", .doc(), perl = TRUE)
  body <- sub("(?s)## Never make public.*$", "", body, perl = TRUE)
  for (flag in INGEST_MODE_FLAGS)
    expect_false(grepl(paste0("Sys.setenv\\s*\\(\\s*", flag), body, perl = TRUE),
                 info = paste("doc still instructs setting", flag))
})
