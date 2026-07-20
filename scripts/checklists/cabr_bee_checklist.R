# =============================================================
# checklists/cabr_bee_checklist.R  --  ROUGH DRAFT (rebuild pending, 2026-07-20)
#
# CABR native-bee checklists. Runs LAST in the pipeline; NOT sourced by
# run_pipeline.R until the inputs (clean stubs + taxonomy rebuild) are finished.
#
# INPUTS -> OUTPUTS (data/checklists/cabr/):
#   1. cabr_raw_inat_native_bee_checklist.csv  <- iNaturalist observations of bees within the
#        CABR boundary-box shapefile (the RAW iNat data clipped to CABR -- NOT inat_bee_clean.R,
#        which is a separate ANALYSIS clean)
#   2. cabr_specimen_native_bee_checklist.csv  <- cleaned specimen records (specimen_bee_clean.R)
#   3. cabr_official_native_bee_checklist.csv  <- union of #1 + #2 (+ potential Dorey / GBIF),
#        one row per taxon with boolean in_inat / in_specimen / in_holway columns
#
# Built on the shared helpers in checklist_build.R (spatial_split / build_checklist /
# finalize_checklist / build_specimen_checklist / combine_checklists).
# TODO(rebuild): wire real inputs -- the RAW iNat bee export (clipped to CABR here), the
#   specimen_bee_clean.R output, the Holway reference table, and the rebuilt taxonomy lookup.
# =============================================================
suppressWarnings(suppressMessages({library(dplyr); library(readr)}))
if (!exists("build_checklist")) source("scripts/checklists/checklist_build.R")
if (!exists("cabr_survey_box")) source("scripts/spatial/spatial_utils.R")

CABR_OUT_DIR <- "data/checklists/cabr"

# bees_sf         : RAW iNat bee observations as an sf POINT layer in PROJECT_CRS, carrying
#                   taxon_id / scientific_name / genus / species / subgenus / complex / ...
#                   (the ingested iNat data -- NOT inat_bee_clean.R). Clipped to CABR below.
# specimens       : cleaned specimen records (from specimens/specimen_bee_clean.R -- PENDING).
# holway_reference: Holway's SD-county reference checklist (reference/, PENDING) -- used ONLY
#                   to set in_holway on the official list; NULL until the rebuild lands.
# taxonomy_lookup : subgenus/complex map (defaults to taxonomy_lookup_from_bees(bees)).
build_cabr_bee_checklists <- function(bees_sf, specimens = NULL,
                                      holway_reference = NULL, taxonomy_lookup = NULL) {
  dir.create(CABR_OUT_DIR, showWarnings = FALSE, recursive = TRUE)
  if (is.null(taxonomy_lookup))
    taxonomy_lookup <- taxonomy_lookup_from_bees(sf::st_drop_geometry(bees_sf))

  # 1. iNat checklist: clip to the CABR survey box -> unique genus+ taxa -> finalize.
  cabr_inat <- spatial_split(bees_sf, cabr_survey_box, "CABR iNat") |>
    build_checklist("CABR iNat") |>
    finalize_checklist(taxonomy_lookup)
  write_csv(cabr_inat, file.path(CABR_OUT_DIR, "cabr_raw_inat_native_bee_checklist.csv"), na = "")
  run_qc(cabr_inat, "CABR iNat")

  # 2. specimen checklist: unique taxa straight from the specimen sheet.
  cabr_specimen <- if (!is.null(specimens)) build_specimen_checklist(specimens) else NULL
  if (!is.null(cabr_specimen))
    write_csv(cabr_specimen, file.path(CABR_OUT_DIR, "cabr_specimen_native_bee_checklist.csv"), na = "")

  # 3. official CABR list: union iNat + specimen (+ Holway cross-check) into one row per taxon
  #    with boolean in_inat / in_specimen / in_holway columns. in_specimen appears ONLY because
  #    specimens exist for CABR; in_holway marks whether the taxon is on Holway's SD list. A
  #    column is emitted only for sources actually provided (no misleading all-FALSE flags).
  #    TODO(rebuild): add in_dorey / in_gbif the same way.
  cabr_official <- combine_checklists(list(inat = cabr_inat, specimen = cabr_specimen,
                                           holway = holway_reference))
  write_csv(cabr_official, file.path(CABR_OUT_DIR, "cabr_official_native_bee_checklist.csv"), na = "")

  invisible(list(inat = cabr_inat, specimen = cabr_specimen, official = cabr_official))
}

# Standalone once inputs exist:
#   source("scripts/checklists/cabr_bee_checklist.R")
#   build_cabr_bee_checklists(bees_sf, specimens, holway_reference, taxonomy_lookup)
