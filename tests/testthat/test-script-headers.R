# Every script opens the same way: a rule, then its own path relative to scripts/.
# The path line is the point -- a script gets opened from a stack trace or a grep
# hit with no folder context, and "which file am I in" should not need a guess.
# This is the same kind of check as test-analysis-modules.R: a convention nobody
# remembers stays true only if something enforces it.
.root <- file.path("..", "..")

.script_files <- function()
  list.files(file.path(.root, "scripts"), pattern = "[.]R$", recursive = TRUE)

test_that("every script opens with a rule comment", {
  bad <- Filter(function(f) {
    h <- readLines(file.path(.root, "scripts", f), warn = FALSE, n = 1)
    !length(h) || !grepl("^# ={5,}", h[1])
  }, .script_files())
  expect_equal(bad, character(0),
               info = paste("no opening rule:", paste(bad, collapse = ", ")))
})

test_that("every script names its own path in the header", {
  bad <- Filter(function(f) {
    h <- readLines(file.path(.root, "scripts", f), warn = FALSE, n = 6)
    !any(grepl(f, h, fixed = TRUE))
  }, .script_files())
  expect_equal(bad, character(0),
               info = paste("header does not name the file's own path:",
                            paste(bad, collapse = ", ")))
})
