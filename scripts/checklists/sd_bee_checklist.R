# =============================================================
# checklists/sd_bee_checklist.R
# San Diego County native-bee checklists (normalized tree -- parent taxa as their own rows).
#
# INPUTS -> OUTPUTS (data/checklists/sd_county/):
#   5. sd_holway_native_bee_checklist.csv              <- Holway reference table -> lookup subtree
#   6. sd_raw_inat_native_bee_checklist.csv            <- RAW iNat bee obs clipped to the SD county
#        boundary -> lookup subtree (leaves + ancestor rows). NOT inat_bee_clean.R.
#   7. sd_holway_and_raw_inat_native_bee_checklist.csv <- union of #5 + #6, one row per taxon with
#        boolean holway / inat columns (NO specimen -- specimens are CABR-only).
#
# Every row: taxon_id, taxon_rank, scientific_name, common_name + taxonomy, all from the lookup.
# =============================================================
suppressWarnings(suppressMessages({library(dplyr); library(readr)}))
if (!exists("lookup_subtree"))     source("scripts/checklists/checklist_build.R")
if (!exists("sd_county_boundary")) source("scripts/spatial/spatial_utils.R")

SD_OUT_DIR <- "data/checklists/sd_county"

# holway_sub: the Holway subtree (lookup_subtree of the Holway reference), built once by the caller
#             and shared with the CABR cross-check. If NULL it's built here from holway_reference.
build_sd_bee_checklists <- function(bees_sf, lookup, holway_sub = NULL, holway_reference = NULL) {
  dir.create(SD_OUT_DIR, showWarnings = FALSE, recursive = TRUE)
  lookup <- lookup |> mutate(taxon_id = as.character(taxon_id))
  if (is.null(holway_sub) && !is.null(holway_reference))
    holway_sub <- lookup_subtree(lookup, holway_reference, "SD Holway")

  # 6. raw iNat: clip to the SD county box -> observed taxon_ids -> lookup subtree.
  obs <- spatial_split(bees_sf, sd_county_boundary, "SD County")
  ids <- unique(as.character(obs$taxon_id)); ids <- ids[!is.na(ids) & ids != ""]
  sd_inat <- lookup_subtree(lookup, lookup |> filter(taxon_id %in% ids), "SD County iNat")
  write_csv(sd_inat, file.path(SD_OUT_DIR, "sd_raw_inat_native_bee_checklist.csv"), na = "")

  # 5. Holway: the Holway subtree, in the shared format.
  if (!is.null(holway_sub))
    write_csv(holway_sub, file.path(SD_OUT_DIR, "sd_holway_native_bee_checklist.csv"), na = "")

  # 7. Holway + raw iNat combined: union the two taxon sets, one row per taxon with holway / inat
  #    flags. NO specimen column -- specimens are CABR-only.
  sd_combined <- combine_checklists(list(holway = holway_sub, inat = sd_inat))
  write_csv(sd_combined, file.path(SD_OUT_DIR, "sd_holway_and_raw_inat_native_bee_checklist.csv"), na = "")

  invisible(list(inat = sd_inat, holway = holway_sub, combined = sd_combined))
}
