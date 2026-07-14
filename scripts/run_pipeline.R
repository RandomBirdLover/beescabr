# =============================================================
# run_pipeline.R
# beescabr pipeline -- ONE COMMAND: ingest + clean + export
# Created: 2026-07-13
#
# The single entrypoint that runs the whole pipeline end to end:
#   1.  INGEST        iNat API -> DuckDB cache (once, incremental)
#   1b. HOLWAY REF    load raw Holway CSV -> resolve each species to an iNat
#                     taxon -> write holway_sd_bee_reference_table.csv
#   2.  EXPORT        Tier 1 + Tier 2 checklists + sd_bee_taxonomy_lookup.csv
#                     (the lookup combines the enriched Holway ref + iNat)
#   3.  CLEAN         cabr_inat_bee_clean.csv (triage + obs-fields + date recovery)
#
# Ingest runs exactly once here; the build and clean stages both read the
# same freshly-filled cache (they do NOT re-fetch). This is why the two
# stage scripts were refactored into build_all_checklists() / clean_inat_bees().
#
# Run:
#   Rscript scripts/run_pipeline.R      (or Source in RStudio)
#
# Flags (env vars):
#   BEESCABR_SKIP_INGEST=1      skip the API pull, use the existing cache
#   BEESCABR_FULL_INGEST=1      re-walk the whole place (not incremental)
#   BEESCABR_SKIP_HOLWAY_REF=1  skip rebuilding the Holway reference table
#   BEESCABR_NONINTERACTIVE=1   auto-skip ambiguous Holway names (no prompts)
#
# NOTE: step 1b resolves Holway species to iNat taxa. On the FIRST run it hits
# the API once per species and may prompt you to disambiguate a few names;
# every choice is cached in DuckDB, so later runs are fast and prompt-free.
# =============================================================

# Prevent the stage scripts' own standalone entrypoints from firing when we
# source them -- this runner owns the ingest and the connection.
BEESCABR_SOURCED_BY_RUNNER <- TRUE

source("scripts/config.R")
source("scripts/utils/utils.R")
source("scripts/engine/db/store_conn.R")
source("scripts/engine/db/observations_store.R")
source("scripts/engine/db/taxon_store.R")
source("scripts/engine/db/decision_store.R")
source("scripts/engine/api/inat_http.R")
source("scripts/engine/api/inat_flatten.R")
source("scripts/engine/api/inat_cache.R")
source("scripts/engine/pipelines/ingest_inat.R")
source("scripts/engine/pipelines/read_inat.R")
source("scripts/clean/triage.R")
source("scripts/spatial/spatial_utils.R")          # boundaries, PROJECT_CRS (once)
source("scripts/checklists/holway.R")
source("scripts/checklists/holway_reference_build.R") # builds holway_sd_bee_reference_table.csv
source("scripts/checklists/checklist_tiers.R")
source("scripts/checklists/taxonomy_reference.R")
source("scripts/checklists/tier2_merge.R")
source("scripts/checklists/native_bee_checklist.R") # defines build_all_checklists()
source("scripts/clean/inat_bee_clean.R")            # defines clean_inat_bees()

main <- function() {
  t0 <- Sys.time()
  con <- store_connect()
  on.exit(store_disconnect(con), add = TRUE)

  # ---- 1. INGEST (once) ----
  if (Sys.getenv("BEESCABR_SKIP_INGEST", "0") == "1") {
    message("== [1/3] INGEST skipped (BEESCABR_SKIP_INGEST=1) -- cache holds ",
            count_observations(con), " obs ==")
  } else {
    message("== [1/3] INGEST: iNat API -> DuckDB cache ==")
    ingest_observations(con, incremental = Sys.getenv("BEESCABR_FULL_INGEST", "0") != "1")
  }

  # ---- 1b. BUILD HOLWAY REFERENCE: raw Holway CSV -> iNat-resolved table ----
  if (Sys.getenv("BEESCABR_SKIP_HOLWAY_REF", "0") == "1") {
    message("\n== [1b/3] HOLWAY REFERENCE skipped (BEESCABR_SKIP_HOLWAY_REF=1) ==")
  } else {
    message("\n== [1b/3] BUILD HOLWAY REFERENCE: resolve Holway species -> iNat taxon_ids ==")
    holway_raw <- load_holway(PATHS$holway_combined)
    interactive_ok <- interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"
    holway_ref <- build_holway_reference(con, holway_raw, interactive_ok = interactive_ok)
    write.csv(holway_ref, PATHS$holway_reference, row.names = FALSE, na = "")
    message("  Holway reference: ", sum(holway_ref$resolved), " resolved / ",
            nrow(holway_ref), " rows -> ", PATHS$holway_reference)
  }

  # ---- 2. EXPORT: checklists + lookup ----
  message("\n== [2/3] EXPORT: Tier 1 + Tier 2 checklists + taxonomy lookup ==")
  build_summary <- build_all_checklists(con)

  # ---- 3. CLEAN: triage output ----
  message("\n== [3/3] CLEAN: cabr_inat_bee_clean.csv ==")
  clean_df <- clean_inat_bees(con)

  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  message("\n========================================")
  message("PIPELINE COMPLETE in ", dt, " min")
  message("  Cache observations : ", count_observations(con))
  message("  Tier 1 (CABR/PL/SD): ", paste(build_summary$tier1[c("cabr","pl","sd")], collapse = " / "))
  message("  Tier 2 (CABR/PL/SD): ", paste(build_summary$tier2[c("cabr","pl","sd")], collapse = " / "))
  message("  Taxonomy lookup rows: ", build_summary$lookup)
  message("  Clean obs rows      : ", nrow(clean_df))
  message("Outputs under data/outputs/. Done.")
}

main()
