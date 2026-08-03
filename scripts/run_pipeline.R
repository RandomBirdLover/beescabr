# =============================================================
# run_pipeline.R
# beescabr pipeline -- ONE COMMAND: ingest + clean + export
# Created: 2026-07-13
#
# The single entrypoint that runs the whole pipeline end to end. Core data first,
# ALL checklist/taxonomy work LAST:
#   1.  INGEST   iNat API -> DuckDB cache (once, incremental)
#   2.  EXPORT   refresh data/observations/cache/export_flat.rds (the brain's input)
#   2b. PLANTS   pull vascular plants -> SEPARATE plant cache -> export_flat_plant.rds
#                (the brain's second input; survey-day confirm + flower resources)
#   3.  BRAIN    finding_project_info(): survey membership from crosswalk_master ->
#                project_unclean + unknown tags/fields/notes -> master_per_survey_info.csv
#   3b. REVIEW   walk unknown tags + fields (interactive) -> crosswalk_master ->
#                re-run brain. Then [3d] eyeball the survey-date windows with no tagged
#                survey nearby (heads-up only, no re-run) and [3e] rule any equal-split
#                transect ties. Notes reviewer is standalone. Skipped non-interactive.
#   4.  CLEAN    cabr_inat_bee_clean.csv (labeled CABR bee table; walk-in re-marked not-survey)
#   5.  CHECKLIST STUFF (LAST): Holway reference -> taxonomy lookup -> the new
#                per-source checklists (parked until built)
#
# Ingest runs exactly once here; every stage reads the same freshly-filled cache
# (no re-fetch).
#
# ------------------------------------------------------------------------------
# HOW TO RUN (RStudio, most common first)
# ------------------------------------------------------------------------------
#   Normal run -- pull only NEW/edited observations since last time (fast, seconds):
#     source("scripts/run_pipeline.R")            # no flags needed; this is the default
#
#   Everyday run -- reuse the caches, don't touch iNat at all:
#     Sys.setenv(BEESCABR_SKIP_INGEST = "1")
#     source("scripts/run_pipeline.R")
#
# ------------------------------------------------------------------------------
# FLAGS = on/off switches. How they work (READ THIS -- it bit us once):
# ------------------------------------------------------------------------------
#   * A flag is just an env var stored in your R SESSION's memory.
#   * Turn a flag ON :  Sys.setenv(FLAG_NAME = "1")
#   * Turn a flag OFF:  Sys.unsetenv("FLAG_NAME")
#   * Check a flag   :  Sys.getenv("FLAG_NAME")   # ""  = off,  "1" = on
#   * A flag you set STAYS ON for the whole R session (every source() re-uses it)
#     until you Sys.unsetenv() it or restart RStudio. That is how a leftover
#     BEESCABR_FULL_INGEST=1 can silently make every run do a slow full rebuild.
#   * Terminal equivalent: prefix it, e.g.  BEESCABR_SKIP_INGEST=1 Rscript scripts/run_pipeline.R
#
# ------------------------------------------------------------------------------
# THE FLAGS
# ------------------------------------------------------------------------------
#   BEESCABR_SKIP_INGEST=1  -> don't call iNat at all; use whatever is already cached.
#                              Use when you just want to re-clean/re-export existing data.
#
#   BEESCABR_SKIP_PLANTS=1  -> pull bees only, skip the (slower) plant pull.
#
#   BEESCABR_FULL_INGEST=1  -> !! SLOW, RARELY NEEDED !! Wipe the ENTIRE cache and
#                              re-download every observation from scratch (~40+ min for
#                              bees + plants). You'll see "Full rebuild: cleared N cached
#                              observations; re-downloading everything." in the console --
#                              if you see that and didn't mean it, this flag is stuck ON:
#                              stop, run Sys.unsetenv("BEESCABR_FULL_INGEST"), re-run.
#                              Default (flag OFF) is INCREMENTAL: keeps the cache and only
#                              fetches records newer than the newest one you already have.
#                              Only turn ON if the cache looks wrong/corrupt or the iNat
#                              query params (place/taxon filters) changed.
#
#   BEESCABR_REBUILD_HOLWAY_REF=1  force-rebuild the Holway reference table (see NOTE below).
#   BEESCABR_NONINTERACTIVE=1      auto-skip ambiguous Holway names (no prompts).
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
source("scripts/utils/console.R")                # phase banners + key/value reporter + NEEDS-YOU rollup
source("scripts/observations/engine/db/store_conn.R")
source("scripts/observations/engine/db/observations_store.R")
source("scripts/observations/engine/db/taxon_store.R")
source("scripts/observations/engine/db/decision_store.R")
source("scripts/observations/engine/api/inat_http.R")
source("scripts/observations/engine/api/inat_flatten.R")
source("scripts/observations/engine/api/inat_cache.R")
source("scripts/observations/engine/pipelines/ingest_inat.R")
source("scripts/observations/engine/pipelines/read_inat.R")
source("scripts/observations/engine/pipelines/ingest_plants.R")  # plant pull -> separate cache -> export_flat_plant.rds
source("scripts/observations/build_field_id_map.R")             # defines build_field_id_map() -- stage 2c
# reference/ (holway.R, taxonomy_reference.R, verify.R) is pulled in by taxonomy_lookup_build.R (stage 5).
source("scripts/spatial/spatial_utils.R")          # boundaries, PROJECT_CRS (once)
source("scripts/project_info/finding_beeple_calendar.R")  # defines finding_beeple_calendar() -- stage 2d
source("scripts/project_info/finding_project_info.R")     # THE brain: provenance + unknowns + survey_dates
source("scripts/project_info/review_crosswalk.R")         # interactive review of unknown tags + fields
source("scripts/project_info/review_windows.R")           # interactive review of survey-date windows
source("scripts/observations/inat_bee_clean.R")           # defines inat_bee_clean() -- stage 7 (clean, taxonomy-filled)
source("scripts/observations/inat_plant_clean.R")         # defines inat_plant_clean() -- stage 8 (surveyors' plant table)
source("scripts/observations/build_location_review_maps.R") # defines build_location_review_maps() -- stage 7d (per-observer maps)
source("scripts/observations/bee_forage.R")               # defines write_bee_forage() -- stage 5b2 (bee-obs forage plants)
source("scripts/observations/qc/inat_misid_qc.R")         # defines inat_misid_qc() -- stage 10 (misID review queue)
# ---- TAXONOMY + SPECIMENS + CHECKLISTS ----
# Both the interactive Holway->iNat resolver AND the non-interactive lookup builder run in
# the pipeline now (stages 4 + 5); they pull their own deps (holway.R, taxonomy_reference.R,
# verify.R, checklist_build.R) via need().
source("scripts/reference/manual_overrides.R")        # apply_manual_overrides / write_review_worklist (name-change fixes)
source("scripts/reference/holway_reference_build.R")  # defines build_holway_reference() -- stage 4 (interactive)
source("scripts/reference/taxonomy_lookup_build.R")   # defines build_taxonomy_lookup() -- stage 5
source("scripts/reference/verify_prompt.R")           # defines prompt_verify_taxa() -- pass-2 verification
source("scripts/reference/plant_lookup_join.R")           # attach_flower_ids() -- flower taxon_id + in_park
source("scripts/reference/plant_taxonomy_lookup_build.R") # defines build_plant_taxonomy_lookup() -- stage 5c
source("scripts/project_info/collect_plant_names.R")      # defines review_plant_names() -- stage 7b
source("scripts/specimens/specimen_clean_helpers.R")          # pure specimen-cleaning helpers
source("scripts/specimens/specimen_bee_clean.R")      # defines clean_specimens() -- stage 6b
source("scripts/specimens/specimen_raw_worklist.R")      # defines tidy_raw_specimens() -- stage 6a raw worklist
source("scripts/reference/specimen_id_prompt.R")      # defines resolve_specimen_taxa() -- stage 6c interactive taxon-id resolve
# CHECKLISTS (stage 9): normalized-tree builder + the 3 per-scope orchestrators. Sourced here so
# the whole pipeline runs end-to-end with NO manual steps (cabr_inat / cabr_specimen / cabr_official /
# pl_raw_inat / sd_holway / sd_raw_inat / sd_holway_and_raw_inat -- parent taxa as their own rows).
source("scripts/checklists/checklist_build.R")        # spatial_split / lookup_subtree / combine_checklists
source("scripts/checklists/cabr_bee_checklist.R")     # defines build_cabr_bee_checklists()
source("scripts/checklists/pl_bee_checklist.R")       # defines build_pl_bee_checklists()
source("scripts/checklists/sd_bee_checklist.R")       # defines build_sd_bee_checklists()
source("scripts/analysis/not_on_holway.R")            # not_on_holway_bees() + format_new_bees() -- stage 11

main <- function() {
  t0 <- Sys.time()
  con <- store_connect()
  on.exit(store_disconnect(con), add = TRUE)

  bx_need_reset()
  bx_phase(1, "SETUP & FETCH")

  # ---- 1. INGEST (once) ----
  if (Sys.getenv("BEESCABR_SKIP_INGEST", "0") == "1") {
    bx_kv("Fetch bees", "cache: ", count_observations(con), " observations (API pull skipped)")
  } else {
    bx_kv("Fetch bees", "pulling new + edited observations from iNaturalist…")
    ingest_observations(con, incremental = Sys.getenv("BEESCABR_FULL_INGEST", "0") != "1")
  }

  # ---- 2. EXPORT: refresh export_flat.rds (the brain reads it directly) ----
  invisible(read_observations_export(con))

  # ---- 2b. PLANTS: ingest vascular plants -> export_flat_plant.rds (separate cache) ----
  # Pulls Point Loma vascular plants into their OWN DuckDB cache and refreshes
  # export_flat_plant.rds, which the brain reads ALONGSIDE the bee export. Serves
  # BOTH survey-day confirmation (a surveyor's plant obs on a plant-only day) and
  # flower-resource analysis. Own connection; the bee `con` above is untouched.
  # Gated by the same ingest flags as bees (+ BEESCABR_SKIP_PLANTS to skip only plants).
  if (Sys.getenv("BEESCABR_SKIP_PLANTS", "0") == "1") {
    bx_kv("Fetch plants", "skipped (BEESCABR_SKIP_PLANTS=1)")
  } else {
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

  # ---- 2c. FIELD MAP: refresh the obs-field name -> iNat id map from the cache ----
  # Keeps data/observations/reference/inat_field_id_map.csv (the crosswalk's stable-id
  # reference) in step with freshly-ingested fields. Cheap DuckDB query; skipped when ingest
  # was skipped and the map already exists. Reuses the bee `con` (2b used its own plant con).
  FIELD_MAP_PATH <- "data/observations/reference/inat_field_id_map.csv"
  if (Sys.getenv("BEESCABR_SKIP_INGEST", "0") != "1" || !file.exists(FIELD_MAP_PATH)) {
    build_field_id_map(con = con)
  }  # else: silently reused (ingest skipped and the map already exists)

  # ---- 2d. BEEPLE CALENDARS: (re)build beeple_calendar_windows.csv from the PDFs ----
  # Re-parses every "YYYY Cabrillo Bee Survey Calendar.pdf" in
  # data/project_info/sources/beeple_calendar_windows/ each run, so a newly-added year
  # (e.g. 2027) is picked up automatically. The brain reads the resulting windows CSV.
  # Wrapped so a missing pdftools / malformed PDF warns and keeps the existing CSV
  # rather than killing the run.
  tryCatch(finding_beeple_calendar(), error = function(e)
    bx_note("calendar parse failed (", conditionMessage(e),
            ") — kept the existing beeple_calendar_windows.csv."))

  # ---- 3. BRAIN: provenance + unknown tags/fields/notes + survey_dates ----
  # finding_project_info() decides survey membership from master_crosswalk.csv, writes
  # cabr_inat_raw.csv, the THREE unknown reports (review them by
  # hand in order -> update crosswalk_master -> re-run), and builds master_per_survey_info.csv
  # (+ the beeple review queue). Taxonomy-blind, so it needs no Holway/lookup.
  bx_phase(2, "SURVEY BRAIN — who surveyed, when, which transect")
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
    bx_kv("Review", n_tags, " unknown tags · ", n_fields, " unknown fields to sort")
    if (n_tags   > 0) review_unknowns("tags")
    if (n_fields > 0) review_unknowns("fields")
    if (n_tags > 0 || n_fields > 0) {
      bx_cont("re-running the brain to apply your crosswalk edits…")
      finding_project_info()
    }
    # [3b-notes] Free-text notes are OPTIONAL -- ask whether to review them this run.
    # 'y' sources the reviewer from project_info/ and runs it; anything
    # else proceeds without notes (the reviewer is never even sourced). Interactive-only.
    n_notes <- .n_rows(FPI_UNKNOWN_NOTES)
    if (n_notes > 0) {
      ans <- tolower(trimws(readline(sprintf(
        "\n[3b-notes] %d free-text note(s) flagged.  Review them now?  [y/N]: ",
        n_notes))))
      if (ans %in% c("y", "yes")) {
        source("scripts/project_info/review_notes.R")
        review_notes()
      } else {
        bx_cont("proceeding without notes (reviewer not run)")
      }
    }

    # [3d] SURVEY-DATE WINDOWS -- heads-up queue of planned windows with no tagged
    # survey nearby (possible missed surveys). Runs LAST so it sees every tag + field.
    # Ruling a window is for YOUR records only -- it never adds a survey date (nothing
    # is hand-added; no tag = not a survey day), so there is NO brain re-run after it.
    n_win <- .n_windows(FPI_REVIEW)
    if (n_win > 0) {
      bx_kv("Windows", n_win, " planned window(s) with no tagged survey nearby — rule them")
      review_windows()
    }

    # [3e] TRANSECT TIES -- equal-split survey days the resolver couldn't call by
    # majority (a beeple's obs tagged evenly across two transects). Rule which transect
    # the day really was, or "both". If you rule any, the brain re-runs right below so the
    # chosen transect lands in master_per_survey_info.csv THIS run (your master truth).
    n_ties <- .n_windows(FPI_TIES)   # reuse the blank/unsure "still to rule" counter
    if (n_ties > 0) {
      bx_kv("Ties", n_ties, " equal-split day(s) to rule")
      review_transect_ties()
      if (.n_windows(FPI_TIES) < n_ties) {   # a tie was ruled -> apply it now, not next run
        bx_cont("re-running the brain to apply your tie ruling(s)…")
        finding_project_info()
      }
    } else {
      bx_kv("Ties", "none to rule")
    }
  } else {
    bx_kv("Review", "skipped (non-interactive) — run review_crosswalk.R / review_windows.R by hand")
  }

  # ---- 4. HOLWAY REFERENCE (interactive: resolves Holway -> iNat; prompts as needed) ----
  # Rebuilds holway_sd_bee_reference_table_v3.csv EVERY run so any Holway change is caught.
  # Decisions are cached (holway_decisions) so a normal run mostly replays them -- you're only
  # prompted for the unresolved-"Described" second pass + anything new. Wrapped so an abort or
  # failure keeps the existing table and never kills the run.
  bx_phase(3, "TAXONOMY")
  bx_kv("Holway table", "matching the SD bee checklist names to iNaturalist…")
  tryCatch({
    .hdf <- load_holway(PATHS$holway_combined)
    .ref <- build_holway_reference(con, .hdf,
              interactive_ok = interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1")
    write.csv(.ref, PATHS$holway_reference, row.names = FALSE, na = "")
    bx_out(basename(PATHS$holway_reference), " (", nrow(.ref), " rows)")
  }, error = function(e) bx_note("Holway reference failed (", conditionMessage(e),
                                 ") — kept the existing table."))

  # ---- 5. TAXONOMY LOOKUP ----
  # Reads the Holway reference table (step 4) + the cache, writes sd_bee_taxonomy_lookup.csv
  # (+ the internal complex map). Wrapped so a taxonomy failure never kills the run.
  if (file.exists(PATHS$holway_reference)) {
    tryCatch(build_taxonomy_lookup(con),
             error = function(e) bx_note("taxonomy lookup failed: ", conditionMessage(e)))
    # PASS 2 -- VERIFY new-to-Holway taxa: prompt to confirm each is a real ID (not a misID) and
    # record it in verified_taxa.csv so it stops being flagged. Interactive only; never kills the run.
    tryCatch({
      if (interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1" && file.exists(PATHS$taxonomy_lookup)) {
        .lkv <- suppressMessages(utils::read.csv(PATHS$taxonomy_lookup, stringsAsFactors = FALSE, check.names = FALSE))
        prompt_verify_taxa(.lkv, interactive_ok = TRUE)
      }
    }, error = function(e) bx_note("verification prompt failed: ", conditionMessage(e)))
  } else {
    bx_kv("Bee lookup", "skipped — no Holway reference table")
  }

  # ---- 5b. PLANT CLEAN (moved up so the plant lookup + flower-id joins below can use it) ----
  bx_phase(4, "CLEAN TABLES")
  tryCatch(inat_plant_clean(),
           error = function(e) bx_note("plant clean failed: ", conditionMessage(e)))

  # ---- 5b2. BEE FORAGE (plants bees were recorded on in-park -> in-park truth for the lookup) ----
  tryCatch(write_bee_forage(),
           error = function(e) bx_note("bee forage failed: ", conditionMessage(e)))

  # ---- 5c. PLANT TAXONOMY LOOKUP (genus+species tree: crosswalk canonicals + broad obs + bee forage) ----
  tryCatch(build_plant_taxonomy_lookup(verbose = TRUE),
           error = function(e) bx_note("plant lookup failed: ", conditionMessage(e)))

  # ---- 6. SPECIMENS (lethal-survey record) ----
  # 6a. Raw hygiene worklist (non-ID'd / missing / duplicate rows to fix by hand).
  # 6b. Clean -- taxon_id + taxonomy from the lookup (step 5), transect, visited plant ->
  #     cabr_specimen_bee_clean.csv (mirrors the iNat bee schema).
  tryCatch(tidy_raw_specimens(),
           error = function(e) bx_note("raw specimen worklist failed: ", conditionMessage(e)))
  tryCatch(clean_specimens(interactive_ok = interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"),
           error = function(e) bx_note("specimen clean failed: ", conditionMessage(e)))

  # 6c. RESOLVE unknown specimen taxa -- for each bee flagged as "not in the taxonomy lookup",
  #     prompt for its iNaturalist id (with a suggested match) and fold it into specimen_additions.csv
  #     WITH full parent lineage, so it enters the lookup on the NEXT build and stops re-flagging.
  #     Interactive only; wrapped so it never kills the run.
  tryCatch({
    if (interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1") {
      .sprec <- suppressMessages(readxl::read_excel(read_latest(SBC_RECORDS_DIR, SBC_RECORDS_PATTERN)))
      resolve_specimen_taxa(.sprec, interactive_ok = TRUE)
    }
  }, error = function(e) bx_note("specimen taxon-id resolve failed: ", conditionMessage(e)))

  # ---- 7. CLEAN: labeled iNat BEE table (taxonomy filled from the lookup) ----
  # Reads the brain's cabr_inat_raw.csv, joins coords + taxon_id, fills taxonomy from the
  # lookup (step 5) by taxon_id, re-marks Humphreys Rd walk-ins. AFTER the lookup so its
  # taxonomy columns are populated.
  tryCatch(inat_bee_clean(),
           error = function(e) bx_note("bee clean failed: ", conditionMessage(e)))

  # ---- 7b. PLANT NAMES: review any NEW plant name not yet in the crosswalk ----
  # After the cleaners so specimen flower labels exist; files your decisions into master_crosswalk
  # for the next run (non-interactive runs just drop a worklist).
  tryCatch(review_plant_names(interactive_ok = interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"),
           error = function(e) bx_note("plant-name review failed: ", conditionMessage(e)))

  # ---- 7c. OBSERVATION REVIEW: prompt for the iNat obs that need fixing ON iNaturalist ----
  # cabr_inat_bee_fix_behavior.csv (wrong/missing flower field) + review_mistagged_transects.csv
  # (stray transect tag) + the bee/plant location_review files (survey pins far from any transect).
  # Each row carries the observation's url, so you open it and fix it there; the next iNat pull picks
  # up your fix. Non-blocking: surfaces + prompts, then continues.
  # (The off-transect LOCATION pins are NOT listed here -- they are the whole subject of
  # stage 7d below, which surfaces them AND builds the per-surveyor maps in one place, so
  # they aren't split across two prompts.)
  tryCatch({
    obs_rev   <- "data/observations/review"
    obs_items <- data.frame(
      label = c("bee behavior to fix (survey)", "bee flowers to add (non-survey)", "stray transect tags"),
      count = c(.n_rows(file.path(obs_rev, "cabr_inat_bee_fix_behavior_survey.csv")),
                .n_rows(file.path(obs_rev, "cabr_inat_bee_fix_behavior_nonsurvey.csv")),
                .n_rows(file.path(obs_rev, "review_mistagged_transects.csv"))),
      file  = c("cabr_inat_bee_fix_behavior_survey.csv", "cabr_inat_bee_fix_behavior_nonsurvey.csv", "review_mistagged_transects.csv"),
      stringsAsFactors = FALSE)
    resolve_review_gate(obs_items, obs_rev,
                        interactive_ok = interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1",
                        fix_hint = "iNaturalist", blocking = FALSE)
  }, error = function(e) message("  [7c] observation review FAILED (non-fatal): ", conditionMessage(e)))

  # ---- 7d. LOCATION REVIEW: the off-transect survey pins (bee + plant) that sit >50 m from
  # any transect. This is the SINGLE place they surface: it reports the counts, builds one
  # self-contained iNaturalist "pins to fix" map per surveyor (next to the two location_review
  # CSVs + the shared instruction page, under review_location/), and prompts you to send them.
  # The per-pin survey-log annotation is computed from the master, so tag-only intern days
  # (e.g. 2024-05-05) label correctly. ----
  bx_phase(5, "SURVEYOR MAPS")
  tryCatch(build_location_review_maps(),
           error = function(e) bx_note("location maps failed: ", conditionMessage(e)))

  # ---- 9. CHECKLISTS: cabr / pl / sd native-bee checklists (normalized tree from the lookup) ----
  # Each checklist carries parent taxa as their own rows (taxon_id/taxon_rank/names/taxonomy from the
  # lookup), like Holway. iNat lists clip the RAW bee export to each boundary; the specimen + Holway
  # subtrees come from cabr_specimen_bee_clean.csv + the Holway reference. Runs LAST (needs stages 5-8).
  bx_phase(6, "CHECKLISTS & QC")
  tryCatch({
    .lk <- suppressMessages(readr::read_csv(PATHS$taxonomy_lookup, show_col_types = FALSE)) |>
             dplyr::mutate(taxon_id = as.character(taxon_id))
    .bees_sf <- readRDS(EXPORT_FLAT_CACHE) |>
             dplyr::filter(!is.na(latitude), !is.na(longitude)) |>
             sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
             sf::st_transform(PROJECT_CRS)
    .spec <- if (file.exists(PATHS$specimen_clean))   suppressMessages(readr::read_csv(PATHS$specimen_clean,   show_col_types = FALSE)) else NULL
    .href <- if (file.exists(PATHS$holway_reference)) suppressMessages(readr::read_csv(PATHS$holway_reference, show_col_types = FALSE)) else NULL
    .hsub <- if (!is.null(.href)) lookup_subtree(.lk, .href, "SD Holway") else NULL   # built once, shared
    build_cabr_bee_checklists(.bees_sf, .lk, specimens = .spec, holway_sub = .hsub)
    build_pl_bee_checklists(.bees_sf, .lk)
    build_sd_bee_checklists(.bees_sf, .lk, holway_sub = .hsub)
    bx_out("data/checklists/{cabr, point_loma, sd_county}/  (7 files)")
  }, error = function(e) bx_note("checklists failed: ", conditionMessage(e)))

  # ---- 10. MISID QC: flag likely-misidentified iNat bee obs for review (advisory) ----
  # Species-level iNat IDs that are non-research-grade, unvouchered by a specimen, AND
  # absent from Holway -> a review queue for a human to verify on iNaturalist. Needs the
  # Holway reference + specimen table (built above), so it runs LAST. Changes nothing else.
  tryCatch(inat_misid_qc(),
           error = function(e) bx_note("misID QC failed: ", conditionMessage(e)))

  # ---- 11. NEW BEES NOT ON HOLWAY: review prompt (any rank) ----
  # CABR official-checklist taxa with holway == FALSE that ALSO have iNat records -> "new" bees
  # (genuine park/county additions OR misIDs). Advisory: a human opens each on iNaturalist to confirm
  # a trusted scientist ID'd it. Unlike stage 10 (species-rank only), this catches complex/genus-rank
  # finds too. Same set the analysis script reports to the park; here it's the "double-check" prompt.
  tryCatch({
    .chk_cabr <- "data/checklists/cabr/cabr_official_native_bee_checklist.csv"
    if (file.exists(.chk_cabr) && file.exists(PATHS$specimen_clean) && file.exists(PATHS$inat_clean)) {
      .spec_nb <- utils::read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
      .inat_nb <- utils::read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
      .noh <- not_on_holway_bees(.chk_cabr, .spec_nb, .inat_nb, holway_path = PATHS$holway_reference)
      .newbees <- .noh[.noh$group %in% c("inat_only", "inat_and_collected"), , drop = FALSE]
      if (nrow(.newbees)) {
        writeLines(format_new_bees(.noh, mode = "review"))
        dir.create("data/observations/review", showWarnings = FALSE, recursive = TRUE)
        write.csv(.newbees[, c("scientific_name", "taxon_rank", "group", "n_inat_records",
                               "n_inat_research_grade", "n_specimen_records", "taxon_id")],
                  "data/observations/review/cabr_new_bees_not_on_holway.csv", row.names = FALSE, na = "")
        bx_out("data/observations/review/cabr_new_bees_not_on_holway.csv")
      } else bx_kv("New bees", "none — every CABR bee with iNat records is on Holway")
    } else bx_kv("New bees", "skipped (need the CABR checklist + cleaned specimen/iNat tables)")
  }, error = function(e) bx_note("new-bees review failed: ", conditionMessage(e)))

  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  # ---- NEEDS-YOU rollup: collect the run's action items from the review artifacts on disk,
  # so a returning user sees everything that wants their attention in ONE place at the end. ----
  .maps_dir <- "data/observations/review/review_location/by_surveyors"
  .n_maps <- if (dir.exists(.maps_dir)) length(list.files(.maps_dir, pattern = "^cabr_pins_to_fix_.*\\.html$")) else 0L
  if (.n_maps > 0) bx_need(sprintf("Send %d surveyors their maps", .n_maps), "review_location/by_surveyors/")
  .n_tax <- .n_rows("data/reference/generated/cabr_taxon_ids_needs_review.csv")
  if (.n_tax > 0) bx_need(sprintf("%d bee names need an iNat id", .n_tax), "cabr_taxon_ids_needs_review.csv")
  .n_dupe <- .n_rows("data/specimens/specimens_clean/review/cabr_specimen_bee_duplicates.csv")
  if (.n_dupe > 0) bx_need(sprintf("%d duplicate specimen IDs", .n_dupe), "cabr_specimen_bee_duplicates.csv")
  .n_notes <- .n_rows(FPI_UNKNOWN_NOTES)
  if (.n_notes > 0) bx_need(sprintf("%d obs notes (optional review)", .n_notes), "run review_notes()")

  message("")
  bx_rule()
  message(sprintf("  ✓ DONE in %s min      cache: %s observations", dt, count_observations(con)))
  message("")
  bx_need_print()
  bx_analysis_files()
  message("\n  Everything else is written under  data/")
  bx_rule()
}

# ---- analysis-ready file manifest (printed at the end of every run) -----------
# The curated set of outputs meant for downstream analysis, grouped by kind.
# Any that aren't on disk are flagged (not found), so a failed/skipped stage
# shows up at a glance. Paths are the config.R / stage constants, verbatim.
bx_analysis_files <- function() {
  grp  <- function(label) message("    ", label, ":")
  item <- function(path) message("      · ", path, if (!file.exists(path)) "   (not found)" else "")
  message("")
  message("  FILES FOR ANALYSIS")
  grp("checklists")
  item("data/checklists/cabr/cabr_official_native_bee_checklist.csv")
  item("data/checklists/cabr/cabr_raw_inat_native_bee_checklist.csv")
  item("data/checklists/cabr/cabr_specimen_native_bee_checklist.csv")
  grp("cleaned observations")
  item("data/observations/inat_clean/cabr_inat_bee_clean.csv")
  item("data/observations/inat_clean/cabr_inat_plant_clean.csv")
  grp("survey record")
  item("data/project_info/master_per_survey_info.csv")
  item("data/project_info/surveyor_roster.csv")
  grp("reference / lookups")
  item("data/reference/sd_bee_taxonomy_lookup.csv")
  item("data/reference/cabr_plant_taxonomy_lookup.csv")
  item("data/reference/holway_sd_bee_reference_table_v3.csv")
  grp("specimens")
  item("data/specimens/specimens_clean/cabr_specimen_bee_clean.csv")
  grp("spatial / boundaries")
  item("data/spatial/boundaries/cabr/cabr_survey_box.shp")
  item("data/spatial/boundaries/cabr/nps_official/cabr_boundary_nps_official.shp")
  item("data/spatial/boundaries/point_loma/point_loma_boundary.shp")
  item("data/spatial/boundaries/san_diego_county/sd_county_boundary.shp")
  item("data/spatial/transects/cabr_bee_transects.shp")
  item("data/spatial/access_routes_to_transects/cabr_survey_access_routes.shp")
}

main()
