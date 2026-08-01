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
source("scripts/analysis/conservation_status.R")   # shared IUCN / conservation lookups (one source)

# Refresh IUCN Red List statuses FIRST, so the field guide and the rare-species figure
# use current listings and any newly threatened species is caught. Best-effort: it needs
# internet + an API token; if it can't reach IUCN (offline, no token, API down) it keeps
# the last cached data/checklists/iucn/iucn_status.csv and the rest of the run proceeds.
# (refresh_iucn_status.R lives in scripts/, not scripts/analysis/, so it is NOT in the
# auto-discovered loop below -- sourcing it here is the single place it runs.)
RUNNING_ALL <- TRUE
message("\n===== refresh_iucn_status.R (IUCN Red List) =====")
tryCatch(source("scripts/refresh_iucn_status.R"),
         error = function(e) message("  !! IUCN refresh skipped: ", conditionMessage(e),
                                     "\n     (using the last cached iucn_status.csv; run stays offline-safe)"))

.modules <- c("theme_beescabr.R", "utils_analysis.R", "not_on_holway.R", "conservation_status.R")
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
