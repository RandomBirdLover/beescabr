# =============================================================
# run_pipeline.R
# beescabr pipeline -- ONE COMMAND: ingest + clean + export
# Created: 2026-07-13
#
# The single entrypoint that runs the whole pipeline end to end:
#   1.  INGEST        iNat API -> DuckDB cache (once, incremental)
#   1b. HOLWAY REF    load raw Holway CSV -> resolve each species to an iNat
#                     taxon -> write holway_sd_bee_reference_table_v3.csv (ONCE;
#                     reused on later runs)
#   2.  LOOKUP        sd_bee_taxonomy_lookup.csv (the enriched Holway ref + iNat).
#                     Checklists are PARKED: the old Tier 1/2 writer moved to
#                     legacy_checklists.R (off) while the new per-source checklist
#                     stage is built to run LAST in the pipeline.
#   3.  CLEAN         cabr_inat_bee_clean.csv (triage + obs-fields + date recovery)
#
# Ingest runs exactly once here; the build and clean stages both read the
# same freshly-filled cache (they do NOT re-fetch). This is why the two
# stage scripts were refactored into build_taxonomy_lookup() / clean_inat_bees().
#
# Run:
#   Rscript scripts/run_pipeline.R      (or Source in RStudio)
#
# Flags (env vars):
#   BEESCABR_SKIP_INGEST=1         skip the API pull, use the existing cache
#   BEESCABR_FULL_INGEST=1         re-walk the whole place (not incremental)
#   BEESCABR_REBUILD_HOLWAY_REF=1  force-rebuild the Holway reference table
#   BEESCABR_NONINTERACTIVE=1      auto-skip ambiguous Holway names (no prompts)
#
# NOTE: step 1b builds the Holway reference table ONCE (resolving 700+ names to
# iNat taxa is slow + interactive). The result is saved to a versioned file and
# REUSED every run thereafter -- no re-resolution, no prompts. Rebuild only when
# Holway ships a new checklist version, via BEESCABR_REBUILD_HOLWAY_REF=1.
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
source("scripts/clean/verify.R")                   # flag_new_taxa/holway_name_sets -- sourced
                                                   # UNCONDITIONALLY so edits reload on a re-run
                                                   # (taxonomy_reference.R only loads it if absent)
source("scripts/spatial/spatial_utils.R")          # boundaries, PROJECT_CRS (once)
source("scripts/checklists/holway.R")
source("scripts/checklists/holway_reference_build.R") # builds holway_sd_bee_reference_table.csv
source("scripts/checklists/checklist_tiers.R")
source("scripts/checklists/taxonomy_reference.R")
source("scripts/checklists/tier2_merge.R")
source("scripts/checklists/taxonomy_lookup_build.R") # defines build_taxonomy_lookup()
# CHECKLISTS PARKED (2026-07-15): the old Tier 1/Tier 2 writer moved to
# legacy_checklists.R and is deliberately NOT sourced -- the checklist stage is
# being rebuilt into the new per-source architecture (cabr_inat / cabr_specimen /
# cabr_official / pl_raw_inat / sd_holway / sd_raw_inat / sd_holway_and_raw_inat)
# which runs LAST. Run the old writer by hand if you still need those outputs.
# source("scripts/checklists/legacy_checklists.R")   # defines build_legacy_checklists()
# CLEAN stage pulled from the pipeline (2026-07-14): inat_bee_clean.R still reads
# the old crosswalk columns and needs its rewrite. Run it by hand for now; do NOT
# source it here. Re-add this line once it's updated.
# source("scripts/clean/inat_bee_clean.R")          # defines clean_inat_bees()

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

  # ---- 1b. HOLWAY REFERENCE: built ONCE, then reused ----
  # Holway's v3 checklist rarely changes, and resolving its 700+ names to iNat
  # taxa is slow + interactive -- so we do it once, save the versioned table,
  # and just reuse it on every later run. Force a rebuild (e.g. when Holway ships
  # a new checklist version) with BEESCABR_REBUILD_HOLWAY_REF=1.
  if (file.exists(PATHS$holway_reference) &&
      Sys.getenv("BEESCABR_REBUILD_HOLWAY_REF", "0") != "1") {
    message("\n== [1b/3] HOLWAY REFERENCE: reusing ", basename(PATHS$holway_reference),
            " (set BEESCABR_REBUILD_HOLWAY_REF=1 to rebuild) ==")
  } else {
    message("\n== [1b/3] BUILD HOLWAY REFERENCE: resolve Holway species -> iNat taxon_ids ==")
    holway_raw <- load_holway(PATHS$holway_combined)
    interactive_ok <- interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"
    holway_ref <- build_holway_reference(con, holway_raw, interactive_ok = interactive_ok)
    write.csv(holway_ref, PATHS$holway_reference, row.names = FALSE, na = "")
    message("  Holway reference: ", sum(holway_ref$resolved), " resolved / ",
            nrow(holway_ref), " rows -> ", PATHS$holway_reference)
  }

  # Always stamp the "(Complex)" display prefix onto the reference file, whether
  # it was just built or reused from an earlier run. decorate_complex is
  # idempotent (it never double-prefixes), so this is safe to run every time --
  # it just guarantees the reused file carries the label like the checklists do.
  if (file.exists(PATHS$holway_reference)) {
    hr <- readr::read_csv(PATHS$holway_reference, show_col_types = FALSE)
    write.csv(decorate_complex(hr), PATHS$holway_reference, row.names = FALSE, na = "")
  }

  # ---- 2. LOOKUP: taxonomy lookup (checklists parked -- see note near sources) ----
  message("\n== [2/3] LOOKUP: sd_bee_taxonomy_lookup (checklists parked) ==")
  build_summary <- build_taxonomy_lookup(con)

  # ---- 3. CLEAN: temporarily removed (see note near the source lines) ----
  message("\n== CLEAN stage skipped -- inat_bee_clean.R pending its crosswalk rewrite; run it manually ==")

  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  message("\n========================================")
  message("PIPELINE COMPLETE in ", dt, " min")
  message("  Cache observations : ", count_observations(con))
  message("  Taxonomy lookup rows: ", build_summary$lookup)
  message("  (Checklists parked -- legacy_checklists.R off; new per-source stage pending.)")
  message("Outputs under data/outputs/. Done.")
}

main()
