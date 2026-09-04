# Every PATHS$key a script names must actually exist in config.R.
#
# This exists because one did not. PATHS$checklist_sd_county_inat was renamed to
# checklist_sd_inat and one reference was missed, so file.exists(NULL) threw
# "invalid 'file' argument", the whole specimen cleaning stage aborted inside its
# tryCatch, and the run printed a one-line note and carried on. The cleaned
# specimen table silently went two weeks stale.
#
# A typo'd key is invisible until something reads it, so check them all up front.
.root <- file.path("..", "..")

test_that("every PATHS key referenced in scripts/ is defined in config.R", {
  old <- setwd(.root); on.exit(setwd(old), add = TRUE)
  source("scripts/config.R")
  fs  <- list.files("scripts", pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
  txt <- paste(unlist(lapply(fs, readLines, warn = FALSE)), collapse = "\n")
  used <- unique(gsub("PATHS\\$", "", unlist(regmatches(txt,
            gregexpr("PATHS\\$[A-Za-z0-9_.]+", txt)))))
  missing <- sort(setdiff(used, names(PATHS)))
  expect_equal(missing, character(0),
               info = paste("named in a script but not in PATHS:",
                            paste(missing, collapse = ", ")))
})
