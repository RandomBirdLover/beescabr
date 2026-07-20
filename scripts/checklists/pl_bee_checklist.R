# =============================================================
# checklists/pl_bee_checklist.R  --  ROUGH DRAFT (rebuild pending, 2026-07-20)
#
# Point Loma native-bee checklist. Runs LAST; NOT sourced by run_pipeline.R yet.
#
# INPUTS -> OUTPUTS (data/checklists/point_loma/):
#   4. pl_raw_inat_native_bee_checklist.csv  <- iNaturalist observations of bees within the
#      Point Loma boundary-box shapefile (RAW iNat data clipped to Point Loma -- NOT
#      inat_bee_clean.R). The ONLY source -- no Dorey / GBIF / SDNHM data for Point Loma.
#
# Built on checklist_build.R (spatial_split / build_checklist / finalize_checklist).
# TODO(rebuild): wire the RAW iNat bee export (clipped here) + the rebuilt taxonomy lookup.
# =============================================================
suppressWarnings(suppressMessages({library(dplyr); library(readr)}))
if (!exists("build_checklist"))     source("scripts/checklists/checklist_build.R")
if (!exists("point_loma_boundary")) source("scripts/spatial/spatial_utils.R")

PL_OUT_DIR <- "data/checklists/point_loma"

build_pl_bee_checklists <- function(bees_sf, taxonomy_lookup = NULL) {
  dir.create(PL_OUT_DIR, showWarnings = FALSE, recursive = TRUE)
  if (is.null(taxonomy_lookup))
    taxonomy_lookup <- taxonomy_lookup_from_bees(sf::st_drop_geometry(bees_sf))

  # Point Loma has iNat only: clip to the Point Loma boundary -> unique taxa -> finalize.
  pl_inat <- spatial_split(bees_sf, point_loma_boundary, "Point Loma") |>
    build_checklist("Point Loma") |>
    finalize_checklist(taxonomy_lookup)
  write_csv(pl_inat, file.path(PL_OUT_DIR, "pl_raw_inat_native_bee_checklist.csv"), na = "")
  run_qc(pl_inat, "Point Loma")

  invisible(pl_inat)
}

# Standalone once inputs exist:
#   source("scripts/checklists/pl_bee_checklist.R")
#   build_pl_bee_checklists(bees_sf, taxonomy_lookup)
