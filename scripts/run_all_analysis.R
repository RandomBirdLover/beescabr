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
source("scripts/analysis/plant_names.R")           # shared plant-genus common-name labels (one source)
source("scripts/analysis/forage_selectivity.R")    # shared bee-genus forage selectivity (one source)

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

# Refresh PLANT-GENUS COMMON NAMES next, same best-effort contract: it fills genera the
# local plant-taxonomy files don't name by asking the public iNaturalist taxa API (no
# token needed). Offline/blocked -> it keeps the local-seed + previously fetched cache,
# and figures fall back to the Latin genus for anything still unnamed. Lives in scripts/
# (not scripts/analysis/), so it is NOT in the auto-discovered loop below.
message("\n===== refresh_plant_common_names.R (plant genus common names) =====")
tryCatch(source("scripts/refresh_plant_common_names.R"),
         error = function(e) message("  !! plant common-name refresh skipped: ", conditionMessage(e),
                                     "\n     (using the last cached plant_genus_common.csv; run stays offline-safe)"))

.modules <- c("theme_beescabr.R", "utils_analysis.R", "not_on_holway.R",
              "conservation_status.R", "plant_names.R", "forage_selectivity.R",
              "findings_summaries.R")   # runs LAST (after every analysis) -- excluded from the auto-loop
.scripts <- setdiff(sort(list.files("scripts/analysis", pattern = "\\.R$")), .modules)

# source each in the global env; a formal-arg closure keeps `nm` safe even if a
# sourced script reuses common variable names.
.ok <- lapply(.scripts, function(nm) {
  message("\n===== ", nm, " =====")
  tryCatch({ source(file.path("scripts/analysis", nm)); TRUE },
           error = function(e) { message("  !! FAILED: ", conditionMessage(e)); FALSE })
})
.failed <- .scripts[!unlist(.ok)]

# LAST: roll up every analysis into plain-language <name>_findings.csv tables +
# a master findings_index.csv (data/analysis/findings/). Runs after the loop so it
# can read the fresh per-analysis outputs it summarises. Best-effort like the rest.
message("\n===== findings_summaries.R (plain-language finding rollups) =====")
tryCatch(source("scripts/analysis/findings_summaries.R"),
         error = function(e) message("  !! findings rollup skipped: ", conditionMessage(e)))

message("\n---------------------------------------------")
message(sprintf("Ran %d analysis scripts; %d failed.", length(.scripts), length(.failed)))
if (length(.failed)) message("Failed: ", paste(.failed, collapse = ", "))
message("Figures + tables are in data/analysis/")
