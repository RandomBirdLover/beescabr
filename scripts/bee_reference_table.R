# =============================================================
# SD Bee Reference / Master Pivot Table
# Created: June 11, 2026
# Author: Brandi Sanchez
# Description: Builds a master reference table from the native
#              bee checklist. Creates three counting keys so the
#              counting method can be switched with one line:
#                key_full          = genus_subgenus_species  (TAXON RICHNESS)
#                key_genus_species = genus_species
#                key_species_only  = species
#              Counting philosophy: "conservative inclusion" -- each
#              unique genus_subgenus_species is one diversity unit.
#              Genus-only records DO count as their own unit.
# =============================================================

# Run once to install, then leave commented:
# install.packages("tidyverse")
library(tidyverse)

# ------------------------------------------------------------
# STEP 1: Load checklist (re-run native_bee_checklist.R first
#         if the file is missing, to avoid the ~3 min API call)
# ------------------------------------------------------------
checklist_path <- "data/outputs/SD_native_bee_checklist.csv"

if (!file.exists(checklist_path)) {
  message("Checklist not found -- running native_bee_checklist.R to build it...")
  source("scripts/native_bee_checklist.R")
}

bee_checklist <- read.csv(checklist_path)

cat("Loaded checklist with", nrow(bee_checklist), "taxa\n")

# ------------------------------------------------------------
# STEP 2: Determine the rank of each record
# ------------------------------------------------------------
bee_reference <- bee_checklist %>%
  mutate(
    # Clean up blanks to NA
    across(where(is.character), ~na_if(., "")),
    rank = case_when(
      !is.na(taxon_subspecies_name) ~ "subspecies",
      !is.na(taxon_species_name)    ~ "species",
      !is.na(subgenus)              ~ "subgenus",
      !is.na(taxon_genus_name)      ~ "genus",
      TRUE                          ~ "higher"
    )
  )

# ------------------------------------------------------------
# STEP 3: Build the three counting keys
# ------------------------------------------------------------
bee_reference <- bee_reference %>%
  mutate(
    key_full = paste(
      coalesce(taxon_genus_name, "NA"),
      coalesce(subgenus, "NA"),
      coalesce(taxon_species_name, "NA"),
      sep = "_"
    ),
    key_genus_species = paste(
      coalesce(taxon_genus_name, "NA"),
      coalesce(taxon_species_name, "NA"),
      sep = "_"
    ),
    key_species_only = coalesce(taxon_species_name, "NA")
  )

# ------------------------------------------------------------
# STEP 4: Dedupe on the full key and drop empty records
# ------------------------------------------------------------
bee_reference <- bee_reference %>%
  filter(key_full != "NA_NA_NA") %>%
  distinct(key_full, .keep_all = TRUE) %>%
  arrange(taxon_family_name, taxon_genus_name, subgenus, taxon_species_name)

# ------------------------------------------------------------
# STEP 5: Diversity counts under each method
# ------------------------------------------------------------
cat("\n--- DIVERSITY COUNTS ---\n")
cat("Taxon richness (genus_subgenus_species):", n_distinct(bee_reference$key_full), "\n")
cat("Genus + species:                        ", n_distinct(bee_reference$key_genus_species), "\n")
cat("Strict species only:                    ",
    n_distinct(bee_reference$key_species_only[bee_reference$key_species_only != "NA"]), "\n")

# ------------------------------------------------------------
# STEP 6: Rank breakdown
# ------------------------------------------------------------
cat("\n--- RANK BREAKDOWN ---\n")
rank_summary <- bee_reference %>%
  count(rank, sort = TRUE)
print(rank_summary)

# ------------------------------------------------------------
# STEP 7: Save reference table
# ------------------------------------------------------------
write.csv(bee_reference,
          "data/outputs/SD_bee_reference_table.csv",
          row.names = FALSE)

cat("\nReference table saved to data/outputs/SD_bee_reference_table.csv\n")
