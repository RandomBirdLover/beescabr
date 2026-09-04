# run_all_analysis_pipeline.R keeps `.modules`: analysis files that are HELPERS
# (other scripts source them) rather than analyses, so the auto-loop must not run
# them as if they produced figures.
#
# It is hand-maintained, and it had already drifted: transect_years.R,
# explorer_photo_helpers.R and inat_taxon_links.R were all being run as analyses.
# Harmless -- they only define functions -- but they inflated the "ran N scripts"
# tally and the next helper might not be harmless.
#
# This derives the truth (a helper is a file another script sources) and fails when
# the list disagrees, which is the same reason config.R owns one dependency list.

.root <- .beescabr_root()

.declared_modules <- function() {
  src <- readLines(file.path(.root, "scripts/run_all_analysis_pipeline.R"), warn = FALSE)
  i <- grep("^\\.modules <- c\\(", src)
  if (!length(i)) return(character(0))
  j <- i; while (!grepl("\\)", src[j])) j <- j + 1L
  unlist(regmatches(paste(src[i:j], collapse = " "),
                    gregexpr('"[^"]+\\.R"', paste(src[i:j], collapse = " "))))  |>
    gsub(pattern = '"', replacement = "", fixed = TRUE)
}

.sourced_helpers <- function() {
  files <- list.files(file.path(.root, "scripts"), pattern = "\\.R$",
                      recursive = TRUE, full.names = TRUE)
  txt <- unlist(lapply(files, function(f) readLines(f, warn = FALSE)))
  hits <- unlist(regmatches(txt, gregexpr('source\\("scripts/analysis/[^"]+\\.R"\\)', txt)))
  sort(unique(basename(gsub('source\\("|"\\)', "", hits))))
}

test_that("every analysis helper is excluded from the auto-run loop", {
  missing <- setdiff(.sourced_helpers(), .declared_modules())
  expect_equal(missing, character(0),
               info = paste("these are sourced as helpers but the loop will run them:",
                            paste(missing, collapse = ", ")))
})

test_that("the exclusion list does not name files that no longer exist", {
  gone <- setdiff(.declared_modules(),
                  basename(list.files(file.path(.root, "scripts/analysis"),
                                      pattern = "\\.R$", recursive = TRUE)))
  expect_equal(gone, character(0),
               info = paste("listed but absent:", paste(gone, collapse = ", ")))
})
