# =============================================================
# SD County / Point Loma / CABR Bee Reference / Master Pivot Tables
# Created: June 11, 2026
# Updated: June 21, 2026 — fixed column names after taxon_*_name
#          rename in native_bee_checklist.R; added complex handling.
# Updated: June 23, 2026 — split into three geographic tiers
#          (SD County, Point Loma, CABR), matching the three-tier
#          split in native_bee_checklist.R. Builds one reference
#          table per tier instead of a single county-wide table.
# Author: Brandi Sanchez
# Description: Builds master reference tables from each of the three
#              tiered native bee checklists. For each tier, creates
#              three counting keys so the counting method can be
#              switched with one line:
#                key_full          = genus_subgenus_complex_species (TAXON RICHNESS)
#                key_genus_species = genus_species
#                key_species_only  = species
#              Counting philosophy: "conservative inclusion" -- each
#              unique genus_subgenus_complex_species is one diversity
#              unit. Genus-only records DO count as their own unit.
#
# Complex handling:
#   complex is now part of key_full. Without it, a
#   complex-rank taxon (e.g. "Andrena osmioides complex", which has
#   no species name) would produce the same key as a plain genus-only
#   record ("Andrena_NA_NA") and incorrectly collapse into it during
#   deduping. Adding complex as its own segment keeps these as
#   distinct diversity units: genus-only, complex-only, and
#   species-within-a-complex all get separate keys.
# =============================================================

# Run once to install, then leave commented:
# install.packages("tidyverse")
library(tidyverse)

source("scripts/utils.R")  # read_latest(), require_columns()

# ------------------------------------------------------------
# STEP 1: Load all three checklists (re-run native_bee_checklist.R
#         first if any are missing, to avoid the ~4 min API call)
# ------------------------------------------------------------
checklist_paths <- c(
  sd_county  = "data/outputs/SD_county_inat_native_bee_checklist.csv",
  point_loma = "data/outputs/PL_inat_native_bee_checklist.csv",
  # RENAMED 2026-06-24 (was CABR_inat_native_bee_checklist.csv) -- see
  # native_bee_checklist.R PART B header for the full CABR naming set.
  cabr       = "data/outputs/cabr_inat_bee_checklist_clean.csv"
)

if (any(!file.exists(checklist_paths))) {
  message("One or more tiered checklists not found -- running native_bee_checklist.R to build them...")
  source("scripts/native_bee_checklist.R")
}

# ------------------------------------------------------------
# STEP 2-6 wrapped into one function, applied to each tier so the
# same logic runs three times with no duplicated code.
# ------------------------------------------------------------
build_reference_table <- function(checklist_path, label) {
  checklist <- read.csv(checklist_path)
  cat(sprintf("\n=== %s: loaded checklist with %d taxa ===\n", label, nrow(checklist)))

  # Determine the rank of each record. Ordered most-resolved to
  # least-resolved. "complex" sits between species and subgenus: a
  # taxon IS complex-rank when its own taxon_id equals its
  # complex_taxon_id (see native_bee_checklist.R).
  bee_reference <- checklist %>%
    mutate(
      across(where(is.character), ~na_if(., "")),
      rank = case_when(
        !is.na(subspecies)                           ~ "subspecies",
        !is.na(species)                              ~ "species",
        !is.na(complex_taxon_id) & taxon_id == complex_taxon_id  ~ "complex",
        !is.na(subgenus)                              ~ "subgenus",
        !is.na(genus)                                 ~ "genus",
        TRUE                                                     ~ "higher"
      )
    )

  # Build the three counting keys
  bee_reference <- bee_reference %>%
    mutate(
      key_full = paste(
        coalesce(genus, "NA"),
        coalesce(subgenus, "NA"),
        coalesce(complex, "NA"),
        coalesce(species, "NA"),
        sep = "_"
      ),
      key_genus_species = paste(
        coalesce(genus, "NA"),
        coalesce(species, "NA"),
        sep = "_"
      ),
      key_species_only = coalesce(species, "NA")
    )

  # Dedupe on the full key and drop empty records
  bee_reference <- bee_reference %>%
    filter(key_full != "NA_NA_NA_NA") %>%
    distinct(key_full, .keep_all = TRUE) %>%
    arrange(family, genus, subgenus, species)

  # Diversity counts under each method
  cat("--- DIVERSITY COUNTS ---\n")
  cat("Taxon richness (genus_subgenus_complex_species):", n_distinct(bee_reference$key_full), "\n")
  cat("Genus + species:                                ", n_distinct(bee_reference$key_genus_species), "\n")
  cat("Strict species only:                            ",
      n_distinct(bee_reference$key_species_only[bee_reference$key_species_only != "NA"]), "\n")

  # Rank breakdown
  cat("\n--- RANK BREAKDOWN ---\n")
  print(bee_reference %>% count(rank, sort = TRUE))

  bee_reference
}

cat("\nNOTE: 'Strict species only' collapses species epithets across\n")
cat("different genera (e.g. Diadasia australis and Dufourea australis\n")
cat("would count as ONE unit, since both have species_name 'australis').\n")
cat("This is a known limitation of that counting method — use key_full\n")
cat("or key_genus_species for accurate richness.\n")

bee_reference_sd_county  <- build_reference_table(checklist_paths["sd_county"],  "SD County")
bee_reference_point_loma <- build_reference_table(checklist_paths["point_loma"], "Point Loma")
bee_reference_cabr       <- build_reference_table(checklist_paths["cabr"],       "CABR")

# ------------------------------------------------------------
# STEP 7: Save all three reference tables.
#
# These are TIER 1 (iNat-only) reference tables. The old single
# SD_bee_reference_table.csv from before 2026-06-23 was built from
# the now-removed county-wide-only inat_bee_checklist -- the
# SD County file below replaces it directly (same logic, same name);
# Point Loma and CABR reference tables are new.
# ------------------------------------------------------------
write.csv(bee_reference_sd_county,
          "data/outputs/SD_bee_reference_table.csv",
          row.names = FALSE)
write.csv(bee_reference_point_loma,
          "data/outputs/PL_bee_reference_table.csv",
          row.names = FALSE)
write.csv(bee_reference_cabr,
          "data/outputs/CABR_bee_reference_table.csv",
          row.names = FALSE)

cat("\nThree reference tables saved to data/outputs/:\n")
cat("  SD_bee_reference_table.csv\n")
cat("  PL_bee_reference_table.csv\n")
cat("  CABR_bee_reference_table.csv\n")
