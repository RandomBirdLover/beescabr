# =============================================================
# Clean Non-Lethal iNaturalist Survey Data (Intern + Beeple)
# Created: June 21, 2026
# Author: Brandi Sanchez
# Data: data/cabr_surveys/nonlethal_inat_intern/
#       data/cabr_surveys/nonlethal_inat_beeple/
# Description: Loads and cleans CABR-specific non-lethal survey
#              observations — photo IDs collected by interns and by
#              the "beeple" photographer group. Both sources share
#              the same standard iNat export column structure (see
#              README iNat export instructions), so one shared
#              cleaning function handles both, tagging each record
#              with its data_source for later comparison.
#
# ASSUMPTIONS — update this script once real exports exist if these
# don't hold:
#   - Files follow the project naming convention: inat_*_YYYY-MM-DD.csv
#   - Columns match the standard iNat export used elsewhere in this
#     project: taxon_id, scientific_name, latitude, longitude,
#     observed_on, quality_grade.
#
# Output: clean_intern_data, clean_beeple_data, and the combined
#         clean_nonlethal_data (with data_source column). Combined
#         result written to data/outputs/CABR_nonlethal_inat_clean.csv
# =============================================================

library(tidyverse)

source("scripts/utils.R")

# ------------------------------------------------------------
# STEP 1: Shared cleaning function for one source folder
# Keeps intern and beeple cleaning logic identical and in one place,
# rather than duplicating it twice with two chances to drift apart.
# ------------------------------------------------------------
clean_inat_source <- function(folder, pattern, source_label) {
  path <- tryCatch(read_latest(folder, pattern), error = function(e) NULL)

  if (is.null(path)) {
    message("No files found yet in ", folder, " — skipping ", source_label, ".")
    return(NULL)
  }

  cat("Loading", source_label, "data:", basename(path), "\n")
  raw <- read.csv(path)

  require_columns(
    raw,
    c("taxon_id", "scientific_name", "latitude", "longitude",
      "observed_on", "quality_grade"),
    paste0(source_label, "_raw")
  )

  clean <- raw %>%
    mutate(
      observed_date  = as.Date(observed_on),
      missing_coords = is.na(latitude) | is.na(longitude),
      missing_taxon  = is.na(taxon_id),
      data_source    = source_label
    )

  cat("  Loaded:", nrow(clean), "observations | Missing coords:",
      sum(clean$missing_coords), "| Missing taxon_id:",
      sum(clean$missing_taxon), "\n")

  clean
}

# ------------------------------------------------------------
# STEP 2: Clean each source
# ------------------------------------------------------------
clean_intern_data <- clean_inat_source(
  "data/cabr_surveys/nonlethal_inat_intern", "^inat_.*", "intern"
)

clean_beeple_data <- clean_inat_source(
  "data/cabr_surveys/nonlethal_inat_beeple", "^inat_.*", "beeple"
)

# ------------------------------------------------------------
# STEP 3: Combine and save
# ------------------------------------------------------------
clean_nonlethal_data <- bind_rows(clean_intern_data, clean_beeple_data)

cat("\n--- NON-LETHAL CLEANING SUMMARY ---\n")
if (nrow(clean_nonlethal_data) == 0) {
  cat("No non-lethal iNat data found yet in either source folder.\n")
  cat("This is expected until intern/beeple exports are added.\n")
} else {
  cat("Combined observations:", nrow(clean_nonlethal_data), "\n")
  print(table(clean_nonlethal_data$data_source))

  write.csv(
    clean_nonlethal_data,
    "data/outputs/CABR_nonlethal_inat_clean.csv",
    row.names = FALSE
  )
  cat("\nSaved to data/outputs/CABR_nonlethal_inat_clean.csv\n")
}
