# =============================================================
# scripts/run_all_analysis.R
# Regenerate EVERY analysis figure + table with the current house palette,
# WITHOUT re-running ingest/cleaning (which is the slow part). It just reads the
# already-cleaned tables and rewrites the outputs in data/analysis/.
#
# Run from the repo ROOT:
#   source("scripts/run_all_analysis.R")     # in an R console
#   Rscript scripts/run_all_analysis.R        # or from a terminal
#
# Any single script that errors is reported and skipped -- the rest still run,
# and the failures are listed at the end.
# =============================================================

# shared modules first: paths, palette, helpers (the figure scripts also
# self-source these, but loading them once up front keeps things tidy + fast).
source("scripts/config.R")
source("scripts/analysis/theme_beescabr.R")
source("scripts/analysis/utils_analysis.R")
source("scripts/analysis/not_on_holway.R")

.modules <- c("theme_beescabr.R", "utils_analysis.R", "not_on_holway.R")
.scripts <- setdiff(sort(list.files("scripts/analysis", pattern = "\\.R$")), .modules)

# source each in the global env; a formal-arg closure keeps `nm` safe even if a
# sourced script reuses common variable names.
.ok <- lapply(.scripts, function(nm) {
  message("\n===== ", nm, " =====")
  tryCatch({ source(file.path("scripts/analysis", nm)); TRUE },
           error = function(e) { message("  !! FAILED: ", conditionMessage(e)); FALSE })
})
.failed <- .scripts[!unlist(.ok)]

message("\n---------------------------------------------")
message(sprintf("Ran %d analysis scripts; %d failed.", length(.scripts), length(.failed)))
if (length(.failed)) message("Failed: ", paste(.failed, collapse = ", "))
message("Figures + tables are in data/analysis/")
