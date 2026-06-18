# =============================================================
# SD Native Bee Checklist with Subgenus and Complex
# Created: June 11, 2026
# Author: Brandi Sanchez
# Data: iNaturalist export, San Diego County, all years,
#       all quality grades, Anthophila (excl. Apis mellifera)
# Description: Builds a unique checklist of all native bee
#              taxa observed in San Diego County, pulling
#              subgenus, complex name, and complex taxon_id
#              from the iNaturalist API automatically.
#              taxon_id is the stable key throughout.
#
# Complex handling:
#   - complex     : name of the complex (e.g. "Andrena osmioides")
#   - complex_taxon_id : iNat taxon_id of the complex itself
#   - Complexes are NOT excluded from richness counts
#   - complex is the join key for matching against specimen data
# =============================================================

# Run once to install, then leave commented:
# install.packages(c("tidyverse", "httr2", "stringr"))
library(tidyverse)
library(httr2)
library(stringr)

# ------------------------------------------------------------
# UTILITY: Auto-detect newest dated export
# ------------------------------------------------------------
read_latest <- function(folder, pattern) {
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files matching '", pattern, "' found in ", folder)
  dates <- as.Date(str_extract(basename(files), "\\d{4}-\\d{2}-\\d{2}"))
  files[which.max(dates)]
}

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
# STEP 3: Pull subgenus, complex, and complex_taxon_id
#         from iNaturalist API (keyed on taxon_id)
#
# Uses /v1/taxa/{id} — returns full ranked ancestors.
# Complex handling:
#   - If the taxon ITSELF has rank "complex", it is flagged directly
#   - If a complex appears in the ancestors, it is captured there
#   - subgenus and complex are kept in separate columns
# ------------------------------------------------------------
get_subgenus_and_complex <- function(taxon_id) {
  Sys.sleep(0.5)  # be polite to the API
  
  tryCatch({
    resp <- request(paste0("https://api.inaturalist.org/v1/taxa/", taxon_id)) %>%
      req_perform() %>%
      resp_body_json()
    
    taxon     <- resp$results[[1]]
    ancestors <- taxon$ancestors
    
    subgenus_name    <- NA_character_
    complex     <- NA_character_
    complex_taxon_id <- NA_integer_
    
    # Check if the taxon ITSELF is a complex rank
    if (!is.null(taxon$rank) && taxon$rank == "complex") {
      complex     <- taxon$name
      complex_taxon_id <- as.integer(taxon$id)
    }
    
    # Walk ancestors for subgenus and complex
    for (a in ancestors) {
      if (!is.null(a$rank)) {
        if (a$rank == "subgenus" && is.na(subgenus_name)) {
          subgenus_name <- a$name
        }
        if (a$rank == "complex" && is.na(complex)) {
          complex     <- a$name
          complex_taxon_id <- as.integer(a$id)
        }
      }
    }
    
    tibble(
      taxon_id         = taxon_id,
      subgenus         = subgenus_name,
      complex     = complex,
      complex_taxon_id = complex_taxon_id
    )
    
  }, error = function(e) {
    tibble(
      taxon_id         = taxon_id,
      subgenus         = NA_character_,
      complex     = NA_character_,
      complex_taxon_id = NA_integer_
    )
  })
}

# Get unique taxon IDs
unique_ids <- unique(bee_checklist$taxon_id)
unique_ids <- unique_ids[!is.na(unique_ids)]

cat("\nFetching subgenus and complex from iNaturalist API for",
    length(unique_ids), "taxa...\n")
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

# ------------------------------------------------------------
# STEP 4: Join subgenus, complex, complex_taxon_id
#         back to checklist (by taxon_id)
# ------------------------------------------------------------
bee_checklist <- bee_checklist %>%
  left_join(taxonomy_lookup, by = "taxon_id") %>%
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
    complex,       # name of complex — join key for specimen matching
    complex_taxon_id,   # iNat taxon_id of the complex — stable reference
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

# Flag any taxa where rank was complex (taxon itself = complex)
complex_taxa <- bee_checklist %>%
  filter(!is.na(complex) & scientific_name == complex)

if (nrow(complex_taxa) > 0) {
  cat("\nCHECK: The following taxa are complex-rank (not species-level IDs):\n")
  print(complex_taxa %>% select(taxon_id, scientific_name, complex, taxon_family_name))
}

# ------------------------------------------------------------
# STEP 6: Summary
# ------------------------------------------------------------
cat("\n--- CHECKLIST SUMMARY ---\n")
cat("Total unique taxa:          ", nrow(bee_checklist), "\n")
cat("Taxa with subgenus:         ", sum(!is.na(bee_checklist$subgenus)), "\n")
cat("Taxa with complex:     ", sum(!is.na(bee_checklist$complex)), "\n")
cat("Taxa that ARE complexes:    ", nrow(complex_taxa), "\n")
cat("Taxa belonging to a complex:", sum(!is.na(bee_checklist$complex)) - nrow(complex_taxa), "\n")
cat("Genera represented:         ", n_distinct(bee_checklist$taxon_genus_name), "\n\n")

print(head(bee_checklist, 10))

# ------------------------------------------------------------
# STEP 7: Save checklist as CSV
# ------------------------------------------------------------
write.csv(
  bee_checklist,
  "data/outputs/SD_native_bee_checklist.csv",
  row.names = FALSE
)

cat("\nChecklist saved to data/outputs/SD_native_bee_checklist.csv\n")