# =============================================================
# checklists/sd_bee_checklist.R  --  ROUGH DRAFT (rebuild pending, 2026-07-20)
#
# San Diego County native-bee checklists. Runs LAST; NOT sourced by run_pipeline.R yet.
#
# INPUTS -> OUTPUTS (data/checklists/sd_county/):
#   5. sd_holway_native_bee_checklist.csv              <- Holway reference table, reformatted
#   6. sd_raw_inat_native_bee_checklist.csv            <- iNaturalist observations of bees within
#        the SD county boundary-box shapefile (RAW iNat data -- NOT inat_bee_clean.R)
#   7. sd_holway_and_raw_inat_native_bee_checklist.csv <- union of #5 + #6, one row per taxon
#        with boolean in_holway / in_inat columns  (NO in_specimen -- specimens are CABR-only)
#
# Built on checklist_build.R (spatial_split / build_checklist / finalize_checklist /
# combine_checklists -> boolean in_holway / in_inat columns).
# TODO(rebuild): #5 depends on the Holway reference-table rebuild (reference/, in _to_delete/);
#   #6 on the RAW iNat bee export (clipped to SD county here) + the taxonomy lookup.
# =============================================================
suppressWarnings(suppressMessages({library(dplyr); library(readr)}))
if (!exists("build_checklist"))    source("scripts/checklists/checklist_build.R")
if (!exists("sd_county_boundary")) source("scripts/spatial/spatial_utils.R")

SD_OUT_DIR <- "data/checklists/sd_county"

# holway_reference: the rebuilt Holway reference table (reference/, PENDING) already in the
#                   common checklist format; NULL until it exists.
build_sd_bee_checklists <- function(bees_sf, holway_reference = NULL, taxonomy_lookup = NULL) {
  dir.create(SD_OUT_DIR, showWarnings = FALSE, recursive = TRUE)
  if (is.null(taxonomy_lookup))
    taxonomy_lookup <- taxonomy_lookup_from_bees(sf::st_drop_geometry(bees_sf))

  # 6. raw iNat: clip to the SD county box -> unique taxa -> finalize.
  sd_inat <- spatial_split(bees_sf, sd_county_boundary, "SD County") |>
    build_checklist("SD County iNat") |>
    finalize_checklist(taxonomy_lookup)
  write_csv(sd_inat, file.path(SD_OUT_DIR, "sd_raw_inat_native_bee_checklist.csv"), na = "")
  run_qc(sd_inat, "SD County iNat")

  # 5. Holway: reformat the rebuilt Holway reference table into the common shape.
  # TODO(rebuild): map holway_sd_bee_reference_table_v#.csv columns -> checklist format.
  sd_holway <- holway_reference
  if (!is.null(sd_holway))
    write_csv(sd_holway, file.path(SD_OUT_DIR, "sd_holway_native_bee_checklist.csv"), na = "")

  # 7. Holway + raw iNat combined: union the two taxon sets into one row per taxon with boolean
  #    in_holway / in_inat columns. in_holway only appears once the Holway reference is wired
  #    (NULL until then -> no misleading all-FALSE column). NO in_specimen -- specimens are CABR-only.
  sd_combined <- combine_checklists(list(holway = sd_holway, inat = sd_inat))
  write_csv(sd_combined, file.path(SD_OUT_DIR, "sd_holway_and_raw_inat_native_bee_checklist.csv"), na = "")

  invisible(list(inat = sd_inat, holway = sd_holway, combined = sd_combined))
}

# Standalone once inputs exist:
#   source("scripts/checklists/sd_bee_checklist.R")
#   build_sd_bee_checklists(bees_sf, holway_reference, taxonomy_lookup)
