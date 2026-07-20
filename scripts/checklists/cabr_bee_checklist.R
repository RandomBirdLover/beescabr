# =============================================================
# checklists/cabr_bee_checklist.R
# CABR native-bee checklists (normalized tree -- parent taxa as their own rows, from the lookup).
#
# INPUTS -> OUTPUTS (data/checklists/cabr/):
#   1. cabr_raw_inat_native_bee_checklist.csv  <- RAW iNat bee obs clipped to the CABR survey box,
#        expanded to the lookup subtree (leaves + ancestor rows). NOT inat_bee_clean.R.
#   2. cabr_specimen_native_bee_checklist.csv  <- the cleaned specimens (cabr_specimen_bee_clean.csv),
#        expanded to the lookup subtree (specimen-only leaves kept with blank taxon_id).
#   3. cabr_official_native_bee_checklist.csv  <- union of #1 + #2, one row per taxon, with boolean
#        specimen / inat / holway columns (holway = cross-check flag, does NOT add taxa).
#
# Every row: taxon_id, taxon_rank, scientific_name, common_name + taxonomy, all from the lookup.
# Built on checklist_build.R (spatial_split / lookup_subtree / combine_checklists).
# =============================================================
suppressWarnings(suppressMessages({library(dplyr); library(readr)}))
if (!exists("lookup_subtree"))   source("scripts/checklists/checklist_build.R")
if (!exists("cabr_survey_box"))  source("scripts/spatial/spatial_utils.R")

CABR_OUT_DIR <- "data/checklists/cabr"

# bees_sf   : RAW iNat bee observations as an sf POINT layer in PROJECT_CRS (taxon_id carried).
# lookup    : the taxonomy lookup (sd_bee_taxonomy_lookup.csv) -- the normalized tree source.
# specimens : cleaned specimen records (cabr_specimen_bee_clean.csv) -- NULL if not built yet.
# holway_sub: the Holway subtree (lookup_subtree of the Holway reference) -- for the cross-check
#             flag only; NULL -> no holway column emitted.
build_cabr_bee_checklists <- function(bees_sf, lookup, specimens = NULL, holway_sub = NULL) {
  dir.create(CABR_OUT_DIR, showWarnings = FALSE, recursive = TRUE)
  lookup <- lookup |> mutate(taxon_id = as.character(taxon_id))

  # 1. iNat: clip raw obs to the CABR box -> observed taxon_ids -> lookup subtree (parents included)
  obs <- spatial_split(bees_sf, cabr_survey_box, "CABR iNat")
  ids <- unique(as.character(obs$taxon_id)); ids <- ids[!is.na(ids) & ids != ""]
  cabr_inat <- lookup_subtree(lookup, lookup |> filter(taxon_id %in% ids), "CABR iNat")
  write_csv(cabr_inat, file.path(CABR_OUT_DIR, "cabr_raw_inat_native_bee_checklist.csv"), na = "")

  # 2. specimen: lookup subtree of the specimen taxa (specimen-only leaves kept, blank taxon_id)
  cabr_specimen <- if (!is.null(specimens)) lookup_subtree(lookup, specimens, "CABR specimen") else NULL
  if (!is.null(cabr_specimen))
    write_csv(cabr_specimen, file.path(CABR_OUT_DIR, "cabr_specimen_native_bee_checklist.csv"), na = "")

  # 3. official CABR list: union iNat + specimen, one row per taxon with specimen / inat flags.
  #    Holway is a CROSS-CHECK flag (does NOT add taxa): TRUE where the taxon is on Holway's list.
  cabr_official <- combine_checklists(list(specimen = cabr_specimen, inat = cabr_inat))
  if (!is.null(cabr_official) && !is.null(holway_sub))
    cabr_official$holway <- .cl_own_key(cabr_official) %in% .cl_own_key(holway_sub)
  write_csv(cabr_official, file.path(CABR_OUT_DIR, "cabr_official_native_bee_checklist.csv"), na = "")

  invisible(list(inat = cabr_inat, specimen = cabr_specimen, official = cabr_official))
}
