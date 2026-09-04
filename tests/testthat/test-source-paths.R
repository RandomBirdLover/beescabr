# Every path a script names must point at a file that exists.
#
# This exists because moving files around broke three DIFFERENT path forms in one
# night, each found only by running a pipeline:
#   source("scripts/reference/holway.R")             repo-root relative
#   need("load_holway", "reference/holway.R")        scripts/ relative
#   source(file.path("scripts", "reference/x.R"))    assembled
# A broken path is invisible until the line runs, and some of those lines only run
# in an interactive prompt or a once-a-season stage.
.root <- file.path("..", "..")

test_that("every .R path named in a script points at a real file", {
  old <- setwd(.root); on.exit(setwd(old), add = TRUE)
  fs  <- list.files("scripts", pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
  bad <- character(0)
  for (f in fs) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    for (m in unlist(regmatches(txt, gregexpr('"[A-Za-z0-9_/]+\\.R"', txt)))) {
      r <- gsub('"', '', m)
      if (grepl("^scripts/", r)) {
        if (!file.exists(r)) bad <- c(bad, paste(f, "->", r))
      } else if (grepl("/", r)) {              # the need() convention: relative to scripts/
        if (!file.exists(file.path("scripts", r))) bad <- c(bad, paste(f, "->", r))
      }
    }
  }
  expect_equal(unique(bad), character(0),
               info = paste("broken source paths:", paste(unique(bad), collapse = " | ")))
})
