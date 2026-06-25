# =============================================================
# SD County / Point Loma / CABR Native Bee Checklists (iNat-only)
# Created: June 11, 2026
# Updated: June 23, 2026 — split into three geographic tiers
#          (SD County, Point Loma, CABR) via spatial filtering
#          against the project boundary shapefiles. Replaces the
#          single county-wide inat_bee_checklist object/file.
# Author: Brandi Sanchez
# Data: iNaturalist export, San Diego County 25 Mile Buffer, all
#       years, all quality grades, Anthophila (excl. Apis mellifera)
# Description: Builds THREE checklists of unique native bee taxa --
#              one for San Diego County (the full export extent),
#              one for Point Loma, and one for CABR -- each derived
#              by spatially filtering the same underlying iNat
#              observations before deduplicating to unique taxa.
#              Subgenus, complex name, and complex taxon_id are
#              pulled from the iNaturalist API ONCE for the full set
#              of taxa across all three tiers (not once per tier),
#              since the API cost is identical either way and a
#              single county-wide pass is far cheaper than three.
#              taxon_id is the stable key throughout.
#
# These are TIER 1 (iNat-only) building blocks -- see README
# "checklist architecture" TODO. TIER 2 (merged, with specimen +
# Dorey data folded in) is NOT built here; that's the
# `*_native_bee_checklist` (no "inat") naming, still to come.
#
# Geographic tier definitions (see spatial_utils.R for full
# provenance/history on each boundary):
#   SD County : sd_county_boundary -- as of 2026-06-24 this is a
#               dissolved Union of the original county boundary +
#               point_loma_boundary, NOT the raw county extent alone.
#               See spatial_utils.R provenance notes before assuming
#               this tier is "no spatial filter."
#   Point Loma: point_loma_boundary -- unmodified City of San Diego
#               "PENINSULA" district (CPCODE 30), re-downloaded fresh
#               2026-06-24. No buffer applied (an earlier version used
#               a 5m seam buffer; that approach was dropped -- see
#               spatial_utils.R provenance notes).
#   CABR      : cabr_survey_box (NOT cabr_boundary -- the survey box
#               is the actual CABR-tier inclusion geometry; see
#               spatial_utils.R header notes)
#
# Column naming convention:
#   taxon_*_name columns follow iNat rank naming throughout.
#   scientific_name is the ONLY column with full "Genus species" format.
#   All other rank columns contain the epithet or rank name only.
#
# Complex handling:
#   - taxon_complex_name : name of the complex (e.g. "Andrena osmioides")
#   - taxon_complex_id   : iNat taxon_id of the complex itself
#   - Complexes are NOT excluded from richness counts
#   - taxon_complex_name is the join key for matching against specimen data
#   - A taxon IS a complex when taxon_id == taxon_complex_id
# =============================================================

# Run once to install, then leave commented:
# install.packages(c("tidyverse", "httr2", "stringr", "sf"))
library(tidyverse)
library(httr2)
library(stringr)
library(sf)

source("scripts/utils.R")        # read_latest()
source("scripts/spatial_utils.R") # cabr_survey_box, point_loma_boundary, sd_county_boundary

# ------------------------------------------------------------
# STEP 1: Load iNaturalist export (auto-detects newest file)
# ------------------------------------------------------------
bees_path <- read_latest(
  "data/reference_exports/native_bees",
  "^inat_native_bees_sdcounty"
)
cat("Loading:", basename(bees_path), "\n")
bees <- read.csv(bees_path)
cat("Loaded", nrow(bees), "observations\n")

# ------------------------------------------------------------
# STEP 2: Spatially split observations into the three tiers.
#
# Uses public latitude/longitude (not private_*) -- same fields the
# rest of the pipeline treats as authoritative. Rows with missing
# coordinates can't be spatially placed and are dropped from ALL
# three tiers (reported below), not silently kept in one.
# ------------------------------------------------------------
n_missing_coords <- sum(is.na(bees$latitude) | is.na(bees$longitude))
if (n_missing_coords > 0) {
  cat(sprintf("\nNOTE: %d observation(s) missing lat/long -- excluded from all spatial tiers.\n",
              n_missing_coords))
}

bees_sf <- bees %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(PROJECT_CRS)  # PROJECT_CRS (26946) comes from spatial_utils.R

filter_to_boundary <- function(points_sf, boundary, label) {
  inside <- st_within(points_sf, boundary, sparse = FALSE)[, 1]
  result <- points_sf %>% filter(inside) %>% st_drop_geometry()
  cat(sprintf("%-12s: %d of %d observations fall inside the boundary\n",
              label, nrow(result), nrow(points_sf)))
  result
}

cat("\n--- Spatial split ---\n")
bees_sd_county  <- filter_to_boundary(bees_sf, sd_county_boundary,  "SD County")
bees_point_loma <- filter_to_boundary(bees_sf, point_loma_boundary, "Point Loma")
bees_cabr       <- filter_to_boundary(bees_sf, cabr_survey_box,     "CABR")

# ------------------------------------------------------------
# STEP 3: Build initial checklist of unique taxa (by taxon_id),
# generically, so the same logic runs once per tier.
# ------------------------------------------------------------
build_checklist <- function(obs_df) {
  obs_df %>%
    select(
      taxon_id,
      scientific_name,
      common_name,
      taxon_kingdom_name,
      taxon_phylum_name,
      taxon_class_name,
      taxon_order_name,
      taxon_superfamily_name,
      taxon_family_name,
      taxon_subfamily_name,
      taxon_tribe_name,
      taxon_subtribe_name,
      taxon_genus_name,
      taxon_species_name,
      taxon_subspecies_name
    ) %>%
    distinct(taxon_id, .keep_all = TRUE) %>%
    filter(!is.na(taxon_genus_name) | !is.na(taxon_species_name)) %>%
    arrange(taxon_family_name, taxon_genus_name, taxon_species_name)
}

checklist_sd_county  <- build_checklist(bees_sd_county)
checklist_point_loma <- build_checklist(bees_point_loma)
checklist_cabr       <- build_checklist(bees_cabr)

cat("\n--- Unique taxa per tier ---\n")
cat("SD County :", nrow(checklist_sd_county), "\n")
cat("Point Loma:", nrow(checklist_point_loma), "\n")
cat("CABR      :", nrow(checklist_cabr), "\n")

# ------------------------------------------------------------
# STEP 4: Pull taxon_subgenus_name, taxon_complex_name, and
#         taxon_complex_id from the iNaturalist API -- ONCE, for
#         the union of taxon_ids across all three tiers. Point Loma
#         and CABR taxa are subsets of the SD County taxon set (since
#         both are geographic subsets of the county), so in practice
#         this just means SD County's unique IDs, but the union is
#         computed explicitly in case that assumption ever breaks
#         (e.g. boundary edits, future county boundary changes).
#
# Uses /v1/taxa/{id} — returns full ranked ancestors.
# Complex handling:
#   - If the taxon ITSELF has rank "complex", it is flagged directly
#     (taxon_id == taxon_complex_id)
#   - If a complex appears in ancestors, it is captured there
#   - subgenus and complex are kept in separate columns
# ------------------------------------------------------------
get_subgenus_and_complex <- function(taxon_id, max_retries = 3) {
  for (attempt in 1:max_retries) {
    Sys.sleep(0.5)  # be polite to the API

    result <- tryCatch({
      resp <- request(paste0("https://api.inaturalist.org/v1/taxa/", taxon_id)) %>%
        req_timeout(10) %>%  # if the API doesn't respond within 10s, fail and retry
        req_perform() %>%
        resp_body_json()

      taxon     <- resp$results[[1]]
      ancestors <- taxon$ancestors

      taxon_subgenus_name <- NA_character_
      taxon_complex_name  <- NA_character_
      taxon_complex_id    <- NA_integer_

      # Check if the taxon ITSELF is a complex rank
      if (!is.null(taxon$rank) && taxon$rank == "complex") {
        taxon_complex_name <- taxon$name
        taxon_complex_id   <- as.integer(taxon$id)
      }

      # Walk ancestors for subgenus and complex
      for (a in ancestors) {
        if (!is.null(a$rank)) {
          if (a$rank == "subgenus" && is.na(taxon_subgenus_name)) {
            taxon_subgenus_name <- a$name
          }
          if (a$rank == "complex" && is.na(taxon_complex_name)) {
            taxon_complex_name <- a$name
            taxon_complex_id   <- as.integer(a$id)
          }
        }
      }

      tibble(
        taxon_id            = taxon_id,
        taxon_subgenus_name = taxon_subgenus_name,
        taxon_complex_name  = taxon_complex_name,
        taxon_complex_id    = taxon_complex_id,
        fetch_failed        = FALSE
      )

    }, error = function(e) NULL)  # NULL signals failure, triggers retry

    if (!is.null(result)) return(result)

    if (attempt < max_retries) {
      Sys.sleep(2)  # back off longer before retrying
    }
  }

  # All retries exhausted — log this taxon_id as a real failure
  cat(sprintf("\n  WARNING: taxon_id %s failed after %d attempts\n", taxon_id, max_retries))
  tibble(
    taxon_id            = taxon_id,
    taxon_subgenus_name = NA_character_,
    taxon_complex_name  = NA_character_,
    taxon_complex_id    = NA_integer_,
    fetch_failed        = TRUE
  )
}

unique_ids <- unique(c(
  checklist_sd_county$taxon_id,
  checklist_point_loma$taxon_id,
  checklist_cabr$taxon_id
))
unique_ids <- unique_ids[!is.na(unique_ids)]

cat("\nFetching subgenus and complex from iNaturalist API for",
    length(unique_ids), "taxa (union across all three tiers)...\n")
cat("Estimated time:", round(length(unique_ids) * 0.5 / 60, 1), "minutes\n\n")

# Run with progress indicator
taxonomy_lookup <- map_dfr(
  seq_along(unique_ids),
  function(i) {
    cat(sprintf("\r  Progress: %d / %d taxa (%.0f%%)",
                i, length(unique_ids),
                i / length(unique_ids) * 100))
    flush.console()
    get_subgenus_and_complex(unique_ids[[i]])
  }
)

cat("\n\nDone fetching taxonomy data!\n")

failed_fetches <- taxonomy_lookup %>% filter(fetch_failed == TRUE)
if (nrow(failed_fetches) > 0) {
  cat("\n*** WARNING:", nrow(failed_fetches),
      "taxa failed to fetch from the API after retries. ***\n")
  cat("*** Results below are INCOMPLETE for these taxa — rerun or investigate. ***\n")
  print(failed_fetches %>% select(taxon_id))
} else {
  cat("\nAll", nrow(taxonomy_lookup), "taxa fetched successfully — no API failures.\n")
}

# ------------------------------------------------------------
# STEP 5: Join taxonomy_lookup onto each tier, parse species and
#         subspecies to epithet only, and finalize column order.
#         Same logic per tier, applied generically.
# ------------------------------------------------------------
finalize_checklist <- function(checklist) {
  checklist %>%
    left_join(taxonomy_lookup, by = "taxon_id") %>%
    mutate(
      taxon_species_name    = word(taxon_species_name, -1),
      taxon_subspecies_name = word(taxon_subspecies_name, -1)
    ) %>%
    select(
      taxon_id,
      scientific_name,
      common_name,
      taxon_kingdom_name,
      taxon_phylum_name,
      taxon_class_name,
      taxon_order_name,
      taxon_superfamily_name,
      taxon_family_name,
      taxon_subfamily_name,
      taxon_tribe_name,
      taxon_subtribe_name,
      taxon_genus_name,
      taxon_subgenus_name,
      taxon_complex_name,   # join key for specimen matching
      taxon_complex_id,     # stable iNat reference for the complex
      taxon_species_name,
      taxon_subspecies_name
    )
}

checklist_sd_county  <- finalize_checklist(checklist_sd_county)
checklist_point_loma <- finalize_checklist(checklist_point_loma)
checklist_cabr       <- finalize_checklist(checklist_cabr)

# ------------------------------------------------------------
# STEP 6: Quality control checks (run per tier)
# ------------------------------------------------------------
run_qc <- function(checklist, label) {
  cat(sprintf("\n--- QUALITY CONTROL: %s ---\n", label))

  families <- checklist %>%
    filter(!is.na(taxon_family_name), taxon_family_name != "") %>%
    distinct(taxon_family_name) %>%
    arrange(taxon_family_name)

  cat("Families found:", nrow(families), "\n")
  print(families$taxon_family_name)

  complex_taxa <- checklist %>%
    filter(!is.na(taxon_complex_id) & taxon_id == taxon_complex_id)

  distinct_complexes <- checklist %>%
    filter(!is.na(taxon_complex_name)) %>%
    distinct(taxon_complex_name, taxon_complex_id)

  cat("Total unique taxa:             ", nrow(checklist), "\n")
  cat("Taxa with taxon_subgenus_name: ", sum(!is.na(checklist$taxon_subgenus_name)), "\n")
  cat("Taxa with taxon_complex_name:  ", sum(!is.na(checklist$taxon_complex_name)), "\n")
  cat("Distinct complexes represented:", nrow(distinct_complexes), "\n")
  cat("Genera represented:            ", n_distinct(checklist$taxon_genus_name), "\n")
}

run_qc(checklist_sd_county,  "SD County")
run_qc(checklist_point_loma, "Point Loma")
run_qc(checklist_cabr,       "CABR")

# Note on the SD County family-count check that used to live here:
# the old script asserted "expected 6 for San Diego." That assumption
# no longer auto-checks itself here since the SD County 25 Mile
# Buffer place filter (see README iNat export instructions) can in
# principle catch additional families just outside the literal county
# line. Eyeball the printed family list above instead of relying on a
# hardcoded count.

# ------------------------------------------------------------
# STEP 7: Save all three checklists as CSV.
#
# These are TIER 1 (iNat-only) outputs. The old single-file
# inat_bee_checklist / SD_inat_bee_checklist.csv from before
# 2026-06-23 no longer exists -- anything downstream referencing
# that name needs to point at one of the three files below instead.
# ------------------------------------------------------------
write.csv(checklist_sd_county,
          "data/outputs/SD_county_inat_native_bee_checklist.csv",
          row.names = FALSE)
write.csv(checklist_point_loma,
          "data/outputs/PL_inat_native_bee_checklist.csv",
          row.names = FALSE)
write.csv(checklist_cabr,
          "data/outputs/CABR_inat_native_bee_checklist.csv",
          row.names = FALSE)

cat("\nThree checklists saved to data/outputs/:\n")
cat("  SD_county_inat_native_bee_checklist.csv\n")
cat("  PL_inat_native_bee_checklist.csv\n")
cat("  CABR_inat_native_bee_checklist.csv\n")
