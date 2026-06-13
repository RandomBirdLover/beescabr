# =============================================================
# SD Native Bee Checklist with Subgenus
# Created: June 11, 2026
# Author: Brandi Sanchez
# Data: iNaturalist export, San Diego County, all years,
#       all quality grades, Anthophila (excl. Apis mellifera)
# Description: Builds a unique checklist of all native bee
#              taxa observed in San Diego County, pulling
#              subgenus from the iNaturalist API automatically
#              using taxon_id as the stable key throughout.
# =============================================================

# Run once to install, then leave commented:
# install.packages(c("tidyverse", "httr2"))
library(tidyverse)
library(httr2)

# ------------------------------------------------------------
# STEP 1: Load iNaturalist export
# ------------------------------------------------------------
bees <- read.csv("data/reference_exports/bees/SD_native_bees_11_june_2026.csv")

cat("Loaded", nrow(bees), "observations\n")

# ------------------------------------------------------------
# STEP 2: Build initial checklist of unique taxa (by taxon_id)
# ------------------------------------------------------------
bee_checklist <- bees %>%
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

cat("Found", nrow(bee_checklist), "unique taxa\n")

# ------------------------------------------------------------
# STEP 3: Pull subgenus from iNaturalist API (keyed on taxon_id)
# Uses the /v1/taxa/{id} path endpoint which returns full
# ranked ancestors including subgenus.
# ------------------------------------------------------------
get_subgenus <- function(taxon_id) {
  Sys.sleep(0.5)  # be polite to the API
  
  tryCatch({
    resp <- request(paste0("https://api.inaturalist.org/v1/taxa/", taxon_id)) %>%
      req_perform() %>%
      resp_body_json()
    
    ancestors <- resp$results[[1]]$ancestors
    
    # Find the ancestor whose rank is "subgenus"
    subgenus_name <- NA_character_
    for (a in ancestors) {
      if (!is.null(a$rank) && a$rank == "subgenus") {
        subgenus_name <- a$name
        break
      }
    }
    
    tibble(taxon_id = taxon_id, subgenus = subgenus_name)
    
  }, error = function(e) {
    tibble(taxon_id = taxon_id, subgenus = NA_character_)
  })
}

# Get unique taxon IDs
unique_ids <- unique(bee_checklist$taxon_id)
unique_ids <- unique_ids[!is.na(unique_ids)]

cat("\nFetching subgenus from iNaturalist API for", length(unique_ids), "taxa...\n")
cat("Estimated time:", round(length(unique_ids) * 0.5 / 60, 1), "minutes\n\n")

# Run with progress indicator
subgenus_lookup <- map_dfr(
  seq_along(unique_ids),
  function(i) {
    cat(sprintf("\r  Progress: %d / %d taxa (%.0f%%)",
                i, length(unique_ids),
                i / length(unique_ids) * 100))
    flush.console()
    get_subgenus(unique_ids[[i]])
  }
)

cat("\n\nDone fetching subgenus data!\n")

# ------------------------------------------------------------
# STEP 4: Join subgenus back to checklist (by taxon_id)
# ------------------------------------------------------------
bee_checklist <- bee_checklist %>%
  left_join(subgenus_lookup, by = "taxon_id") %>%
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
    subgenus,
    taxon_species_name,
    taxon_subspecies_name
  )

# ------------------------------------------------------------
# STEP 5: Quality control checks
# ------------------------------------------------------------
cat("\n--- QUALITY CONTROL ---\n")

# Family count (should be 6 for San Diego County)
families <- bee_checklist %>%
  filter(!is.na(taxon_family_name), taxon_family_name != "") %>%
  distinct(taxon_family_name) %>%
  arrange(taxon_family_name)

cat("Families found:", nrow(families), "(expected 6 for San Diego)\n")
print(families$taxon_family_name)

if (nrow(families) == 6) {
  cat("PASS: Family count matches expectation\n")
} else {
  cat("CHECK: Family count differs from expected 6\n")
}

# ------------------------------------------------------------
# STEP 6: Summary
# ------------------------------------------------------------
cat("\n--- CHECKLIST SUMMARY ---\n")
cat("Total unique taxa:   ", nrow(bee_checklist), "\n")
cat("Taxa with subgenus:  ", sum(!is.na(bee_checklist$subgenus)), "\n")
cat("Genera represented:  ", n_distinct(bee_checklist$taxon_genus_name), "\n\n")

print(head(bee_checklist, 10))

# ------------------------------------------------------------
# STEP 7: Save checklist as CSV
# ------------------------------------------------------------
write.csv(bee_checklist,
          "data/outputs/SD_native_bee_checklist.csv",
          row.names = FALSE)

cat("\nChecklist saved to data/outputs/SD_native_bee_checklist.csv\n")
