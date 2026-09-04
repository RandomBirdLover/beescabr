# The three stages must be run in order: each reads what the one before it wrote,
# and publishing refuses to ship a page older than the data. Every doc that shows
# how to run the pipeline has to show the same order, and name the runners that
# actually exist -- a doc naming a renamed script sends a new person hunting.
.root <- .beescabr_root()

.STAGES <- c("run_data_cleaning_pipeline.R",
             "run_all_analysis_pipeline.R",
             "run_publishing_materials_pipeline.R")

.DOCS <- c("README.md", "dev-docs/PIPELINE_GUIDE.md", "dev-docs/DATA_ACCESS.md")

test_that("the runners the docs name all exist", {
  for (s in .STAGES)
    expect_true(file.exists(file.path(.root, "scripts", s)), info = s)
})

test_that("every run doc lists the three stages, in order", {
  for (d in .DOCS) {
    txt <- paste(readLines(file.path(.root, d), warn = FALSE), collapse = "\n")
    at <- vapply(.STAGES, function(s) {
      m <- regexpr(s, txt, fixed = TRUE); if (m < 0) NA_integer_ else as.integer(m)
    }, 0L)
    expect_false(anyNA(at),
                 info = paste(d, "does not name", paste(.STAGES[is.na(at)], collapse = ", ")))
    if (!anyNA(at))
      expect_equal(order(at), seq_along(.STAGES),
                   info = paste(d, "lists the stages out of order"))
  }
})

test_that("no run doc tells a reader to source config.R as a step", {
  # Every script sources config.R itself. Listing it as a command to type invents
  # a fourth step that does nothing on its own.
  for (d in .DOCS) {
    txt <- paste(readLines(file.path(.root, d), warn = FALSE), collapse = "\n")
    expect_false(grepl('source\\("scripts/config\\.R"\\)', txt, perl = TRUE),
                 info = paste(d, "tells the reader to run config.R"))
  }
})
