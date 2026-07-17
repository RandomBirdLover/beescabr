# =============================================================
# tools/build_plant_export.R
# beescabr -- memory-isolated (re)build of export_flat_plant.rds
# Created: 2026-07-17 (recovery after an in-pipeline OOM on the first plant export)
#
# The plant OBSERVATION cache (data/cache/inat_cache_plant.duckdb) is already
# durable -- the API pull committed 40k+ obs before the crash. This script only
# rebuilds the flattened + taxonomy-resolved export the brain reads. It exists so
# that build can run ALONE, in a FRESH R session, with neither the 77k-bee export
# frame nor the bee DB connection resident -- that co-residency is what ran the
# in-pipeline build out of memory at taxonomy batch 5/62.
#
# HOW TO RUN
#   1. Session > Restart R   (or click "Start New Session" on the crash dialog)
#   2. In the fresh console, with the working dir at the repo root:
#        source("scripts/build_plant_export.R")
#   3. When it prints "Plant export built: N rows", run the pipeline normally --
#      step 2b will find export_flat_plant.rds already built (cache hit) and skip
#      the heavy flatten, sailing into the brain.
#
# Resumable: the plant taxon_cache persists, so if the taxonomy fetch is
# interrupted again, just source this file again -- it picks up where it left off.
# =============================================================

BEESCABR_SOURCED_BY_RUNNER <- TRUE   # keep the sourced modules from auto-running

source("scripts/config.R")
source("scripts/engine/db/store_conn.R")
source("scripts/engine/db/observations_store.R")
source("scripts/engine/db/taxon_store.R")
source("scripts/engine/api/inat_http.R")
source("scripts/engine/api/inat_flatten.R")
source("scripts/engine/api/inat_cache.R")
source("scripts/engine/pipelines/read_inat.R")

build_plant_export <- function() {
  con <- store_connect(DB_CACHE_PATH_PLANT)
  on.exit(store_disconnect(con), add = TRUE)

  n <- count_observations(con)
  message("Plant cache holds ", n, " observations.")
  if (n == 0L) {
    message("Nothing to export -- the plant cache is empty. Run the plant ingest first.")
    return(invisible(NULL))
  }

  message("Building export_flat_plant.rds (flatten + taxonomy resolve). This is the ",
          "step that OOM'd inside the pipeline; here it runs alone, so it has room.")
  x <- read_observations_export(con, cache_path = EXPORT_FLAT_PLANT_CACHE, verbose = TRUE)
  gc()
  message("\nPlant export built: ", nrow(x), " rows, ", ncol(x), " cols -> ", EXPORT_FLAT_PLANT_CACHE)
  message("Now run the pipeline as usual; step 2b will cache-hit this file.")
  invisible(x)
}

build_plant_export()
