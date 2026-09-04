# =============================================================
# checklists/pl_bee_checklist.R
# Point Loma native-bee checklist (normalized tree -- parent taxa as their own rows, from lookup).
#
# INPUTS -> OUTPUTS (data/checklists/point_loma/):
#   4. pl_inat_native_bee_checklist_generated.csv  <- RAW iNat bee obs clipped to the Point Loma boundary,
#      expanded to the lookup subtree (leaves + ancestor rows). The ONLY source for Point Loma
#      (no Dorey / GBIF / SDNHM). NOT inat_bee_clean.R.
#
# Every row: taxon_id, taxon_rank, scientific_name, common_name + taxonomy, all from the lookup.
# =============================================================
suppressWarnings(suppressMessages({library(dplyr); library(readr)}))
if (!exists("lookup_subtree"))      source("scripts/checklists/checklist_build.R")
if (!exists("point_loma_boundary")) source("scripts/spatial/spatial_utils.R")

PL_OUT_DIR <- "data/checklists/point_loma"

#' Build the Point Loma tier of checklists and write them out
#'
#' @param bees_sf Cleaned bee observations as an `sf` point layer.
#' @param lookup The bee taxonomy lookup.
#' @return Invisibly, the checklists written.
build_pl_bee_checklists <- function(bees_sf, lookup) {
  dir.create(PL_OUT_DIR, showWarnings = FALSE, recursive = TRUE)
  lookup <- lookup |> mutate(taxon_id = as.character(taxon_id))

  obs <- spatial_split(bees_sf, point_loma_boundary, "Point Loma")
  ids <- unique(as.character(obs$taxon_id)); ids <- ids[!is.na(ids) & ids != ""]
  pl_inat <- lookup_subtree(lookup, lookup |> filter(taxon_id %in% ids), "Point Loma iNat")
  write_csv(pl_inat, file.path(PL_OUT_DIR, "pl_inat_native_bee_checklist_generated.csv"), na = "")

  invisible(pl_inat)
}
