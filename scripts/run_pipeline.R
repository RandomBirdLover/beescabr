# =============================================================
# run_pipeline.R
# beescabr pipeline -- ONE COMMAND: ingest + clean + export
# Created: 2026-07-13
#
# The single entrypoint that runs the whole pipeline end to end. Core data first,
# ALL checklist/taxonomy work LAST:
#   1.  INGEST   iNat API -> DuckDB cache (once, incremental)
#   2.  EXPORT   refresh data/cache/export_flat.rds (the brain's input)
#   2b. PLANTS   pull vascular plants -> SEPARATE plant cache -> export_flat_plant.rds
#                (the brain's second input; survey-day confirm + flower resources)
#   3.  BRAIN    finding_project_info(): survey membership from crosswalk_master ->
#                project_unclean + unknown tags/fields/notes -> per_survey_information.csv
#   3b. REVIEW   walk unknown tags + fields (interactive) -> crosswalk_master ->
#                re-run brain. Then [3d] eyeball the survey-date windows with no tagged
#                survey nearby (heads-up only, no re-run) and [3e] rule any equal-split
#                transect ties. Notes reviewer is standalone. Skipped non-interactive.
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
#   BEESCABR_SKIP_INGEST=1         skip the API pull (bees AND plants), use caches
#   BEESCABR_SKIP_PLANTS=1         skip the plant step entirely (bees only)
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
source("scripts/engine/pipelines/ingest_plants.R")  # plant pull -> separate cache -> export_flat_plant.rds
source("scripts/clean/triage.R")
source("scripts/clean/verify.R")                   # flag_new_taxa/holway_name_sets -- sourced
                                                   # UNCONDITIONALLY so edits reload on a re-run
                                                   # (taxonomy_reference.R only loads it if absent)
source("scripts/spatial/spatial_utils.R")          # boundaries, PROJECT_CRS (once)
source("scripts/clean/finding_project_info.R")     # THE brain: provenance + unknowns + survey_dates
source("scripts/clean/finding_specimen_dates.R")   # newest specimen .xlsx -> inputs/specimen_dates.csv
source("scripts/clean/review_crosswalk.R")         # interactive review of unknown tags + fields
source("scripts/clean/review_windows.R")           # interactive review of survey-date windows
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

  # ---- 2b. PLANTS: ingest vascular plants -> export_flat_plant.rds (separate cache) ----
  # Pulls Point Loma vascular plants into their OWN DuckDB cache and refreshes
  # export_flat_plant.rds, which the brain reads ALONGSIDE the bee export. Serves
  # BOTH survey-day confirmation (a surveyor's plant obs on a plant-only day) and
  # flower-resource analysis. Own connection; the bee `con` above is untouched.
  # Gated by the same ingest flags as bees (+ BEESCABR_SKIP_PLANTS to skip only plants).
  if (Sys.getenv("BEESCABR_SKIP_PLANTS", "0") == "1") {
    message("\n== [2b] PLANTS skipped (BEESCABR_SKIP_PLANTS=1) ==")
  } else {
    message("\n== [2b] PLANTS: iNat -> plant cache -> export_flat_plant.rds ==")
    # Free the bee export frame first: building the plant export (flatten + taxonomy
    # of 40k obs) while the 77k-bee frame was still resident OOM'd R on the first
    # plant run (2026-07-17). The on-disk export_flat.rds is untouched; the later
    # checklist stage re-reads it from disk (cache hit) if it needs it.
    clear_export_cache()
    ingest_plants(
      incremental = Sys.getenv("BEESCABR_FULL_INGEST", "0") != "1",
      do_ingest   = Sys.getenv("BEESCABR_SKIP_INGEST", "0") != "1"
    )
  }

  # ---- 2c. SPECIMENS: rebuild specimen_dates.csv from the newest specimen .xlsx ----
  # Aggregates the newest cabr_bee_specimens_record_V*.xlsx -> inputs/specimen_dates.csv,
  # which the brain reads to stamp n_speci on lethal days + add an intern row for any
  # netting day not in the intern log. Skips quietly if no specimen .xlsx is present.
  message("\n== [2c] SPECIMENS: newest specimen record -> inputs/specimen_dates.csv ==")
  finding_specimen_dates()

  # ---- 3. BRAIN: provenance + unknown tags/fields/notes + survey_dates ----
  # finding_project_info() decides survey membership from crosswalk_master, writes
  # per_observation_raw_info.csv, the THREE unknown reports (review them by
  # hand in order -> update crosswalk_master -> re-run), and builds per_survey_information.csv
  # (+ the beeple review queue). Taxonomy-blind, so it needs no Holway/lookup.
  message("\n== [3] BRAIN: finding_project_info (membership -> unknown tags/fields/notes -> survey_dates) ==")
  finding_project_info()

  # ---- 3b. REVIEW: sort unknown tags + fields into crosswalk_master (interactive) ----
  # The brain just wrote the unknown reports; walk through tags then fields (the
  # same review you'd run by hand), then re-run the brain so survey membership +
  # survey_dates reflect what you filed. Auto-skips in non-interactive / scheduled
  # runs (or force-skip in RStudio with BEESCABR_NONINTERACTIVE=1).
  .n_rows <- function(p) if (file.exists(p)) nrow(readr::read_csv(p, show_col_types = FALSE)) else 0L
  .n_windows <- function(p) {
    if (!file.exists(p)) return(0L)
    d <- readr::read_csv(p, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
    if (!"decision" %in% names(d)) return(nrow(d))
    sum(is.na(d$decision) | trimws(d$decision) == "" | tolower(trimws(d$decision)) == "unsure")
  }
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
      message("  NOTE: ", n_notes, " unknown NOTES remain -- run review_notes.R by hand (standalone).")

    # [3d] SURVEY-DATE WINDOWS -- heads-up queue of planned windows with no tagged
    # survey nearby (possible missed surveys). Runs LAST so it sees every tag + field.
    # Ruling a window is for YOUR records only -- it never adds a survey date (nothing
    # is hand-added; no tag = not a survey day), so there is NO brain re-run after it.
    n_win <- .n_windows(FPI_REVIEW)
    if (n_win > 0) {
      message("\n== [3d] REVIEW survey windows: ", n_win, " to rule (heads-up only) ==")
      review_windows()
    }

    # [3e] TRANSECT TIES -- equal-split survey days the resolver couldn't call by
    # majority (a beeple's obs tagged evenly across two transects). Rule which transect
    # the day really was, or "both". A ruling re-stamps that day on the NEXT pipeline run.
    n_ties <- .n_windows(FPI_TIES)   # reuse the blank/unsure "still to rule" counter
    if (n_ties > 0) {
      message("\n== [3e] REVIEW transect ties: ", n_ties, " to rule ==")
      review_transect_ties()
    } else {
      message("\n== [3e] REVIEW transect ties: nothing to rule ==")
    }
  } else {
    message("\n== [3b] REVIEW skipped (non-interactive) -- run review_crosswalk.R / review_windows.R by hand ==")
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
