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
# install.packages(c("tidyverse", "httr2", "stringr"))
library(tidyverse)
library(httr2)
library(stringr)

source("scripts/utils.R")  # read_latest()

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
inat_bee_checklist <- bees %>%
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

cat("Found", nrow(inat_bee_checklist), "unique taxa\n")

# ------------------------------------------------------------
# STEP 3: Pull taxon_subgenus_name, taxon_complex_name,
#         and taxon_complex_id from iNaturalist API
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

# Get unique taxon IDs
unique_ids <- unique(inat_bee_checklist$taxon_id)
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
# STEP 4: Join taxonomy_lookup back to checklist (by taxon_id)
#         then parse species and subspecies to epithet only.
#
# scientific_name is the ONLY column with full "Genus species".
# taxon_species_name    → epithet only (e.g. "ribifloris")
# taxon_subspecies_name → epithet only (e.g. "biedermannii")
# ------------------------------------------------------------
inat_bee_checklist <- inat_bee_checklist %>%
  left_join(taxonomy_lookup, by = "taxon_id") %>%
  mutate(
    taxon_species_name    = word(taxon_species_name, -1),
    taxon_subspecies_name = word(taxon_subspecies_name, -1)
  )

# Report any taxa that failed to fetch even after retries — these need
# manual follow-up, NOT a silent NA that looks identical to "no subgenus exists"
failed_fetches <- inat_bee_checklist %>% filter(fetch_failed == TRUE)
if (nrow(failed_fetches) > 0) {
  cat("\n*** WARNING:", nrow(failed_fetches),
      "taxa failed to fetch from the API after retries. ***\n")
  cat("*** Results below are INCOMPLETE for these taxa — rerun or investigate. ***\n")
  print(failed_fetches %>% select(taxon_id, scientific_name))
} else {
  cat("\nAll", nrow(inat_bee_checklist), "taxa fetched successfully — no API failures.\n")
}

inat_bee_checklist <- inat_bee_checklist %>%
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

# ------------------------------------------------------------
# STEP 5: Quality control checks
# ------------------------------------------------------------
cat("\n--- QUALITY CONTROL ---\n")

# Family count (should be 6 for San Diego County)
families <- inat_bee_checklist %>%
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

# Flag taxa that ARE complexes (taxon_id == taxon_complex_id)
# This correctly distinguishes complex-rank taxa from species that
# merely belong to a complex (which share the name but differ in taxon_id)
complex_taxa <- inat_bee_checklist %>%
  filter(!is.na(taxon_complex_id) & taxon_id == taxon_complex_id)

# Full list of distinct complexes represented in the data, regardless of
# whether the complex itself has its own complex-rank observation.
# This will generally be LARGER than nrow(complex_taxa) — a complex can be
# represented entirely by species-level observations within it, with no
# observation ever IDed to the complex level itself.
distinct_complexes <- inat_bee_checklist %>%
  filter(!is.na(taxon_complex_name)) %>%
  distinct(taxon_complex_name, taxon_complex_id) %>%
  arrange(taxon_complex_name)

if (nrow(complex_taxa) > 0) {
  cat("\nCHECK: The following taxa ARE complex-rank (not species-level IDs):\n")
  print(complex_taxa %>%
    select(taxon_id, scientific_name, taxon_complex_name, taxon_family_name))
}

if (nrow(distinct_complexes) > 0) {
  cat("\nAll distinct complexes represented in this dataset:\n")
  print(distinct_complexes)
}

# ------------------------------------------------------------
# STEP 6: Summary
# ------------------------------------------------------------
cat("\n--- CHECKLIST SUMMARY ---\n")
cat("Total unique taxa:                  ", nrow(inat_bee_checklist), "\n")
cat("Taxa with taxon_subgenus_name:       ", sum(!is.na(inat_bee_checklist$taxon_subgenus_name)), "\n")
cat("Taxa with taxon_complex_name:        ", sum(!is.na(inat_bee_checklist$taxon_complex_name)), "\n")
cat("Distinct complexes represented:      ", nrow(distinct_complexes),
    "(every unique complex appearing in this dataset, at any resolution)\n")
cat("  - with a complex-rank observation: ", nrow(complex_taxa),
    "(complexes where at least one observation could not be resolved past complex)\n")
cat("  - species-level only:              ", nrow(distinct_complexes) - nrow(complex_taxa),
    "(complexes only ever observed at full species resolution)\n")
cat("Genera represented:                  ", n_distinct(inat_bee_checklist$taxon_genus_name), "\n\n")

print(head(inat_bee_checklist, 10))

# ------------------------------------------------------------
# STEP 7: Save checklist as CSV
# ------------------------------------------------------------
write.csv(
  inat_bee_checklist,
  "data/outputs/SD_inat_bee_checklist.csv",
  row.names = FALSE
)

cat("\nChecklist saved to data/outputs/SD_inat_bee_checklist.csv\n")

