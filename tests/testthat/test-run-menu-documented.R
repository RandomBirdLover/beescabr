# PIPELINE_GUIDE.md documents the run-mode menu the cleaning pipeline shows. Both
# it and DATA_ACCESS.md had drifted from the real menu -- the guide listed modes
# by names that no longer existed ("Skip ingest", "Full re-walk", "Refresh"), and
# DATA_ACCESS told people to set BEESCABR_SKIP_INGEST by hand, which
# ingest_mode.R reads as "the operator knows what they are doing" and silently
# SKIPS the menu for. Following the instructions turned the menu off.
#
# So the menu and the doc are checked against each other.
src("utils/ingest_mode.R")

.doc <- function()
  paste(readLines(file.path("..", "..", "dev-docs", "PIPELINE_GUIDE.md"), warn = FALSE),
        collapse = "\n")

test_that("every run mode is listed in DATA_ACCESS.md, by number and label", {
  doc <- .doc()
  for (k in names(INGEST_MODES)) {
    lab <- INGEST_MODES[[k]]$label
    expect_match(doc, paste0("\\| ", k, " \\| ", lab), perl = TRUE,
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
  for (f in c("dev-docs/PIPELINE_GUIDE.md", "dev-docs/DATA_ACCESS.md", "CLAUDE.md")) {
    body <- paste(readLines(file.path("..", "..", f), warn = FALSE),
                  collapse = "\n")
    for (flag in INGEST_MODE_FLAGS)
      expect_false(grepl(paste0("Sys.setenv\\s*\\(\\s*", flag), body, perl = TRUE),
                   info = paste(f, "still instructs setting", flag))
  }
})
