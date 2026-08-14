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

# IUCN Red List status (per bee species) and plant-genus common names are no longer refreshed
# here -- they are now BAKED INTO THE CLEANED TABLES + CHECKLISTS at data-cleaning time
# (scripts/reference/enrich_lookups.R, called by the *_clean.R scripts + cabr_bee_checklist.R,
# fetched once and cached). This run stays fully OFFLINE and just reads those columns/caches.
# To force-refresh existing IUCN assessments or common names, run the pipeline with the refresh
# flag (BEESCABR_REFRESH=1 Rscript scripts/run_pipeline.R) -- or the two tools directly:
#   Rscript scripts/reference/refresh_iucn_status.R  |  Rscript scripts/reference/refresh_plant_common_names.R
RUNNING_ALL <- TRUE

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

# ---- PUBLISH (last stage): sync the public GitHub Pages site with the fresh HTML ----
# Publishing is PART OF THE PIPELINE so it's never a forgotten manual step: this copies the
# public report pages (field guides, summary, least-sampled, the maps, the occurrence explorer)
# into docs/ and rebuilds the landing page. By default it only updates docs/ locally; set
# BEESCABR_DEPLOY=1 to ALSO commit + push docs/ (GitHub Pages then redeploys automatically).
message("\n===== publish public site (docs/) =====")
pub <- tryCatch(system2("bash", "scripts/publish_pages.sh", stdout = TRUE, stderr = TRUE),
                error = function(e) conditionMessage(e))
message(paste(utils::tail(pub, 4), collapse = "\n"))
if (identical(Sys.getenv("BEESCABR_DEPLOY"), "1")) {
  message("BEESCABR_DEPLOY=1 -> committing + pushing docs/ ...")
  system2("git", c("add", "docs/"))
  if (system2("git", c("diff", "--cached", "--quiet")) != 0L) {   # non-zero exit = there ARE staged changes
    system2("git", c("commit", "-m", "Rebuild published site (docs/)"))
    system2("git", c("push", "origin", "main"))
    message("  deployed -- GitHub Pages will update in ~1 minute.")
  } else message("  no site changes to deploy.")
} else {
  message("Site updated in docs/. Commit + push to deploy, or set BEESCABR_DEPLOY=1 to auto-deploy.")
}
