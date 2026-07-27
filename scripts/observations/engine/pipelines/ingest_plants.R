# =============================================================
# pipelines/ingest_plants.R
# beescabr pipeline -- populate the PLANT observation cache from the iNat API
# Created: 2026-07-17
#
# The plant counterpart to ingest_inat.R. It does NOT reimplement anything --
# it points the SAME ingest_observations() + read_observations_export() at a
# SEPARATE plant DuckDB cache (config: DB_CACHE_PATH_PLANT), so the plant pull
# has its own incremental id-cursor and updated_since state and never touches
# the bee cache (see config.R "PLANT cache" note for why separate).
#
# Scope (config.R):
#   taxon_id = TAXON_TRACHEOPHYTA  (vascular plants -- gymnosperms + angiosperms
#              both bear pollen; ferns ride along and drop out downstream)
#   place_id = PLACE_POINT_LOMA    (fully contains cabr_survey_box; the brain
#              re-filters to the exact box by point-in-polygon)
#   NO without_taxon_id -- we want every vascular plant (unlike bees, which
#   exclude Apis mellifera).
#
# Output: data/observations/cache/export_flat_plant.rds -- the second export the brain reads
# (FPI_EXPORTS kind="plant"), giving plant obs the same survey-membership +
# survey-date treatment as bees, plus the raw plant data for flower-resource
# analysis ("what plants do the bees visit / what's in bloom on survey days").
#
# Depends on: db/store_conn.R, db/observations_store.R, pipelines/ingest_inat.R,
# pipelines/read_inat.R, config.R.
# =============================================================

if (!exists("store_connect"))              source("scripts/observations/engine/db/store_conn.R")
if (!exists("ingest_observations"))        source("scripts/observations/engine/pipelines/ingest_inat.R")
if (!exists("read_observations_export"))   source("scripts/observations/engine/pipelines/read_inat.R")
if (!exists("bx_kv") && file.exists("scripts/utils/console.R")) source("scripts/utils/console.R")
if (!exists("TAXON_TRACHEOPHYTA"))         source("scripts/config.R")

# ------------------------------------------------------------
# ingest_plants(): pull vascular plants into the plant cache, then (re)build
# export_flat_plant.rds. Opens + closes its OWN connection to the plant DB.
#   do_ingest = FALSE  -> skip the API pull, just rebuild the export from the
#                         cache already on disk (mirrors run_pipeline's
#                         BEESCABR_SKIP_INGEST behavior for bees).
# Returns (invisibly) the number of observation rows written by the pull.
# ------------------------------------------------------------
ingest_plants <- function(place_id = PLACE_POINT_LOMA,
                          taxon_id = TAXON_TRACHEOPHYTA,
                          incremental = TRUE,
                          do_ingest = TRUE,
                          verbose = TRUE) {
  con <- store_connect(DB_CACHE_PATH_PLANT)
  on.exit(store_disconnect(con), add = TRUE)

  n_written <- 0L
  if (do_ingest) {
    n_written <- ingest_observations(
      con,
      place_id         = place_id,
      taxon_id         = taxon_id,
      without_taxon_id = NULL,               # keep every vascular plant
      incremental      = incremental,
      state_path       = INGEST_STATE_PATH_PLANT,
      verbose          = verbose
    )
  } else if (verbose) {
    bx_cont("plants: reusing the existing cache (API pull skipped)")
  }

  # Build the export the brain reads. Guard the empty-cache case (first run with
  # do_ingest = FALSE, or a pull that returned nothing) so we don't warn noisily.
  if (count_observations(con) > 0) {
    read_observations_export(con, cache_path = EXPORT_FLAT_PLANT_CACHE, verbose = verbose)
    if (verbose) bx_out(basename(EXPORT_FLAT_PLANT_CACHE))
  } else if (verbose) {
    bx_note("plant cache is empty -- nothing to export yet (run with do_ingest = TRUE first).")
  }
  invisible(n_written)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  message('Sourced. Run: ingest_plants()')
