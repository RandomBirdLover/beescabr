# =============================================================
# run_pipeline.R
# beescabr pipeline -- ONE COMMAND: ingest + clean + export
# Created: 2026-07-13
#
# The single entrypoint that runs the whole pipeline end to end. Core data first,
# ALL checklist/taxonomy work LAST:
#   1.  INGEST   iNat API -> DuckDB cache (once, incremental)
#   2.  EXPORT   refresh data/cache/export_flat.rds (the brain's input)
#   3.  BRAIN    finding_project_info(): survey membership from crosswalk_master ->
#                project_unclean + unknown tags/fields/notes -> survey_dates.csv
#   3b. REVIEW   walk through the unknown tags + fields (interactive), file them
#                into crosswalk_master, then re-run the brain to apply. Notes have
#                no reviewer yet. Auto-skipped when non-interactive.
#   4.  CLEAN    cabr_inat_bee_clean.csv (pending its rewrite -- run by hand)
#   5.  CHECKLIST STUFF (LAST): Holway reference -> taxonomy lookup -> the new
#                per-source checklists (parked until built)
#
# Ingest runs exactly once here; every stage reads the same freshly-filled cache
# (no re-fetch).
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
source("scripts/clean/finding_project_info.R")     # THE brain: provenance + unknowns + survey_dates
source("scripts/clean/review_crosswalk.R")         # interactive review of unknown tags + fields
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
    message("== [1] INGEST skipped (BEESCABR_SKIP_INGEST=1) -- cache holds ",
            count_observations(con), " obs ==")
  } else {
    message("== [1] INGEST: iNat API -> DuckDB cache ==")
    ingest_observations(con, incremental = Sys.getenv("BEESCABR_FULL_INGEST", "0") != "1")
  }

  # ---- 2. EXPORT: refresh export_flat.rds (the brain reads it directly) ----
  message("\n== [2] EXPORT: refresh data/cache/export_flat.rds ==")
  invisible(read_observations_export(con))

  # ---- 3. BRAIN: provenance + unknown tags/fields/notes + survey_dates ----
  # finding_project_info() decides survey membership from crosswalk_master, writes
  # project_unclean_bee_observations.csv, the THREE unknown reports (review them by
  # hand in order -> update crosswalk_master -> re-run), and builds survey_dates.csv
  # (+ the beeple review queue). Taxonomy-blind, so it needs no Holway/lookup.
  message("\n== [3] BRAIN: finding_project_info (membership -> unknown tags/fields/notes -> survey_dates) ==")
  finding_project_info()

  # ---- 3b. REVIEW: sort unknown tags + fields into crosswalk_master (interactive) ----
  # The brain just wrote the unknown reports; walk through tags then fields (the
  # same review you'd run by hand), then re-run the brain so survey membership +
  # survey_dates reflect what you filed. Auto-skips in non-interactive / scheduled
  # runs (or force-skip in RStudio with BEESCABR_NONINTERACTIVE=1).
  .n_rows <- function(p) if (file.exists(p)) nrow(readr::read_csv(p, show_col_types = FALSE)) else 0L
  if (interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1") {
    n_tags <- .n_rows(FPI_UNKNOWN_TAGS); n_fields <- .n_rows(FPI_UNKNOWN_FIELDS)
    message("\n== [3b] REVIEW unknowns: ", n_tags, " tags, ", n_fields, " fields to sort ==")
    if (n_tags   > 0) review_unknowns("tags")
    if (n_fields > 0) review_unknowns("fields")
    if (n_tags > 0 || n_fields > 0) {
      message("\n== [3c] Re-running the brain to apply your crosswalk edits ==")
      finding_project_info()
    }
    n_notes <- .n_rows(FPI_UNKNOWN_NOTES)
    if (n_notes > 0)
      message("  NOTE: ", n_notes, " unknown NOTES remain -- no notes reviewer yet.")
  } else {
    message("\n== [3b] REVIEW skipped (non-interactive) -- run review_crosswalk.R by hand ==")
  }

  # ---- 4. CLEAN: temporarily removed (inat_bee_clean.R pending its rewrite) ----
  message("\n== [4] CLEAN skipped -- inat_bee_clean.R pending its crosswalk rewrite; run it manually ==")

  # ---- 5. CHECKLIST STUFF (LAST) : Holway reference -> taxonomy lookup -> checklists ----
  # Everything checklist-related runs at the very END, after the core data pipeline.
  # The Holway reference is built ONCE then reused (BEESCABR_REBUILD_HOLWAY_REF=1 to
  # rebuild); the lookup reads it. The new per-source checklist stage slots in here
  # too when built (parked for now).
  message("\n== [5a] HOLWAY REFERENCE ==")
  if (file.exists(PATHS$holway_reference) &&
      Sys.getenv("BEESCABR_REBUILD_HOLWAY_REF", "0") != "1") {
    message("  reusing ", basename(PATHS$holway_reference),
            " (set BEESCABR_REBUILD_HOLWAY_REF=1 to rebuild)")
  } else {
    message("  building: resolve Holway species -> iNat taxon_ids")
    holway_raw <- load_holway(PATHS$holway_combined)
    interactive_ok <- interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"
    holway_ref <- build_holway_reference(con, holway_raw, interactive_ok = interactive_ok)
    write.csv(holway_ref, PATHS$holway_reference, row.names = FALSE, na = "")
    message("  Holway reference: ", sum(holway_ref$resolved), " resolved / ",
            nrow(holway_ref), " rows -> ", PATHS$holway_reference)
  }
  # stamp the "(Complex)" display prefix (idempotent) whether built or reused
  if (file.exists(PATHS$holway_reference)) {
    hr <- readr::read_csv(PATHS$holway_reference, show_col_types = FALSE)
    write.csv(decorate_complex(hr), PATHS$holway_reference, row.names = FALSE, na = "")
  }

  message("\n== [5b] LOOKUP: sd_bee_taxonomy_lookup (checklists parked) ==")
  build_summary <- build_taxonomy_lookup(con)

  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  message("\n========================================")
  message("PIPELINE COMPLETE in ", dt, " min")
  message("  Cache observations  : ", count_observations(con))
  message("  Taxonomy lookup rows: ", build_summary$lookup)
  message("  (Checklists parked -- the new per-source stage runs here, LAST, when built.)")
  message("Outputs under data/. Done.")
}

main()
