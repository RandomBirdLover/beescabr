# =============================================================
# SD Bee Master Reference Table
# Created: June 11, 2026
# Author: Brandi Sanchez
# Purpose: Build a master reference/pivot table of all unique
#          bee taxa for diversity (taxon richness) analyses.
#          This table is the JOIN KEY for all future CABR /
#          Point Loma / San Diego County comparison scripts.
#
# Counting philosophy: each unique genus_subgenus_species
#          combination = one diversity unit (taxon richness).
#          Genus-only and subgenus-only records are retained
#          as their own units (conservative inclusion), since
#          hard-to-ID genera (e.g. Colletes, Lasioglossum)
#          are often only resolvable to genus/subgenus.
#
# Requires: bee_checklist (from native_bee_checklist.R) must
#           already exist in the environment, OR load the
#           saved checklist CSV below.
# =============================================================

library(tidyverse)

# ------------------------------------------------------------
# STEP 1: Load the checklist (with subgenus already pulled)
# ------------------------------------------------------------
# If bee_checklist isn't in your environment, load from CSV:
if (!exists("bee_checklist")) {
  bee_checklist <- read.csv("data/SD_native_bee_checklist.csv")
}

cat("Starting with", nrow(bee_checklist), "taxa\n")

# ------------------------------------------------------------
# STEP 2: Clean up the species name
# iNaturalist stores taxon_species_name as the FULL binomial
# (e.g. "Bombus californicus"). We extract just the specific
# epithet so the keys read cleanly.
# ------------------------------------------------------------
bee_reference <- bee_checklist %>%
  mutate(
    # Treat empty strings as NA throughout
    across(where(is.character), ~ na_if(.x, "")),
    
    # Extract just the specific epithet (second word) from species name
    species_epithet = if_else(
      !is.na(taxon_species_name),
      word(taxon_species_name, 2),
      NA_character_
    ),
    
    # Extract just the subspecies epithet (third word) if present
    subspecies_epithet = if_else(
      !is.na(taxon_subspecies_name),
      word(taxon_subspecies_name, 3),
      NA_character_
    )
  )

# ------------------------------------------------------------
# STEP 3: Assign a rank to each taxon
# Determines the finest level this taxon was identified to.
# ------------------------------------------------------------
bee_reference <- bee_reference %>%
  mutate(
    rank = case_when(
      !is.na(subspecies_epithet) ~ "subspecies",
      !is.na(species_epithet)    ~ "species",
      !is.na(subgenus)           ~ "subgenus",
      !is.na(taxon_genus_name)   ~ "genus",
      TRUE                       ~ "above_genus"
    )
  )

# ------------------------------------------------------------
# STEP 4: Build the key columns for flexible counting
# ------------------------------------------------------------
bee_reference <- bee_reference %>%
  mutate(
    # FULL KEY: genus_subgenus_species (taxon richness)
    # NA parts become "NA" text so keys are always comparable
    key_full = paste(
      coalesce(taxon_genus_name, "NA"),
      coalesce(subgenus, "NA"),
      coalesce(species_epithet, "NA"),
      sep = "_"
    ),
    
    # GENUS + SPECIES KEY: ignores subgenus
    key_genus_species = paste(
      coalesce(taxon_genus_name, "NA"),
      coalesce(species_epithet, "NA"),
      sep = "_"
    ),
    
    # SPECIES-ONLY KEY: only filled for species-level IDs,
    # NA otherwise (for strict species richness)
    key_species_only = if_else(
      rank %in% c("species", "subspecies"),
      paste(taxon_genus_name, species_epithet),
      NA_character_
    )
  )

# ------------------------------------------------------------
# STEP 5: Deduplicate on the full key
# Collapses any cases where multiple taxon_ids map to the
# same genus_subgenus_species combination.
# ------------------------------------------------------------
bee_reference <- bee_reference %>%
  arrange(taxon_family_name, taxon_genus_name, subgenus, species_epithet) %>%
  distinct(key_full, .keep_all = TRUE)

cat("After deduplicating on genus_subgenus_species:", nrow(bee_reference), "unique taxa\n")

# Remove records with no genus-level information (uncountable)
bee_reference <- bee_reference %>%
  filter(key_full != "NA_NA_NA")

# ------------------------------------------------------------
# STEP 6: Final column order
# ------------------------------------------------------------
bee_reference <- bee_reference %>%
  select(
    # Keys first (what future scripts join on)
    key_full,
    key_genus_species,
    key_species_only,
    rank,
    # Then full taxonomy
    taxon_id,
    scientific_name,
    common_name,
    taxon_family_name,
    taxon_subfamily_name,
    taxon_tribe_name,
    taxon_subtribe_name,
    taxon_genus_name,
    subgenus,
    species_epithet,
    subspecies_epithet
  )

# ------------------------------------------------------------
# STEP 7: Diversity counts three ways (demonstration)
# ------------------------------------------------------------
cat("\n--- DIVERSITY COUNTS (San Diego County) ---\n")
cat("Taxon richness (full key):        ",
    n_distinct(bee_reference$key_full), "\n")
cat("Genus+species richness:           ",
    n_distinct(bee_reference$key_genus_species), "\n")
cat("Strict species richness:          ",
    n_distinct(bee_reference$key_species_only[!is.na(bee_reference$key_species_only)]), "\n")

cat("\n--- BREAKDOWN BY RANK ---\n")
print(bee_reference %>% count(rank))

# ------------------------------------------------------------
# STEP 8: Save the master reference table
# ------------------------------------------------------------
write.csv(bee_reference,
          "data/SD_bee_reference_table.csv",
          row.names = FALSE)

cat("\nMaster reference table saved to data/SD_bee_reference_table.csv\n")

# Preview
print(head(bee_reference, 10))

