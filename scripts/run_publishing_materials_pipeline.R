# =============================================================
# run_publishing_materials_pipeline.R
# beescabr pipeline -- STAGE 3: PUBLISHING MATERIALS (the public GitHub Pages site)
#
# The three pipeline stages, each its own runner:
#   1. run_data_cleaning_pipeline.R      ingest iNat -> DuckDB cache -> cleaned tables
#   2. run_all_analysis_pipeline.R       every figure + table + report/journal HTML
#   3. run_publishing_materials_pipeline.R   <-- THIS: build + publish the public site
#
# This stage re-renders ONLY the pages that go PUBLIC (field guides, park summary,
# least-sampled, the transect + bee-bounty maps, and the occurrence explorer), then
# copies them into docs/ and rebuilds the landing page (via scripts/publish/). The
# publish modules live in scripts/publish/, the way cleaning lives in scripts/clean/
# and analysis in scripts/analysis/.
#
#   Rscript scripts/run_publishing_materials_pipeline.R                     # rebuild docs/ (then git push to deploy)
#   BEESCABR_DEPLOY=1 Rscript scripts/run_publishing_materials_pipeline.R   # also commit + push docs/ (auto-deploy)
#
# Assumes the analysis data is current -- run stages 1 + 2 first.
# =============================================================

# shared modules the public HTML scripts rely on (they also self-source these; loading
# once up front keeps things tidy + fast).
source("scripts/config.R")
source("scripts/analysis/theme_beescabr.R")
source("scripts/analysis/utils_analysis.R")
source("scripts/analysis/plant_names.R")
source("scripts/analysis/conservation_status.R")
source("scripts/analysis/forage_selectivity.R")
source("scripts/analysis/not_on_holway.R")

# the analysis scripts that emit a PUBLIC html page. ADD NEW PUBLIC PAGES HERE
# (and add the matching row to PUBLISH_PAGES in scripts/publish/publish_pages.R).
PUBLIC_PAGES <- c(
  "bee_field_guide.R", "bee_field_guide_genus.R", "nps_summary_tables.R",
  "least_sampled_bees.R", "bee_bounties.R", "transect_map.R",
  "bee_occurrence_explorer.R")

# refresh basemap: clear the cached tiles so the static transect map redraws
# with CURRENT tiles on every publish (interactive Leaflet maps are already live)
unlink("data/spatial/basemap_cache", recursive = TRUE)

message("==> STAGE 3: regenerating public report HTML")
ok <- vapply(PUBLIC_PAGES, function(nm) {
  message("    ", nm)
  tryCatch({ source(file.path("scripts/analysis", nm)); TRUE },
           error = function(e) { message("      !! FAILED: ", conditionMessage(e)); FALSE })
}, logical(1))
if (any(!ok)) message("  (failed: ", paste(PUBLIC_PAGES[!ok], collapse = ", "), ")")

message("\n==> Publishing into docs/")
pub <- tryCatch(system2("Rscript", "scripts/publish/publish_pages.R", stdout = TRUE, stderr = TRUE),
                error = function(e) conditionMessage(e))
message(paste(pub, collapse = "\n"))

if (identical(Sys.getenv("BEESCABR_DEPLOY"), "1")) {
  message("\n==> Deploying (BEESCABR_DEPLOY=1)")
  system2("git", c("add", "docs/"))
  if (system2("git", c("diff", "--cached", "--quiet")) != 0L) {   # non-zero = there ARE staged changes
    system2("git", c("commit", "-m", "Rebuild published site (docs/)"))
    system2("git", c("push", "origin", "main"))
    message("    deployed -- GitHub Pages will update in ~1 minute.")
  } else message("    no site changes to deploy.")
} else {
  message("\nSite rebuilt in docs/. To deploy:")
  message("    git add docs/ && git commit -m 'Rebuild site' && git push")
  message("  (or re-run with BEESCABR_DEPLOY=1 to do that automatically).")
}
