# =============================================================
# Clean CABR Bee Specimens (Lethal Survey Data)
# Created: June 21, 2026
# Updated: June 21, 2026 — column names updated to match the V9
#          snake_case schema (see SPECIMEN_CHANGELOG.md).
# Author: Brandi Sanchez
# Data: CABR_bee_specimens_V{n}_{YYYY-MM-DD}.xlsx
#       (data/cabr_surveys/lethal/) — newest version auto-detected
# Description: Loads and cleans the authoritative CABR bee specimen
#              spreadsheet. Validates expected columns, parses dates,
#              flags missing coordinates/genus/species/method, and
#              builds an oldscientificname key for joining against
#              the SD reference table (native_bee_reference_table.R).
#
# Note on taxon_complex_name: this column is now baked into the
# specimen sheet itself (added in V9 via a one-time match against
# SD_inat_bee_checklist.csv). This script reads it as-is — it does
# NOT re-derive it, since that matching is a data-prep step, not a
# repeatable cleaning step.
#
# Output: clean_bee_data (data frame in environment), plus QC tables
#         missing_latlong, missing_genus_species, missing_method_plant.
#         Also written to data/outputs/CABR_bee_specimens_clean.csv
# =============================================================

library(tidyverse)
library(readxl)
library(lubridate)

source("scripts/utils.R")

# ------------------------------------------------------------
# STEP 1: Load newest specimen file
# All versions live together in lethal/ — see SPECIMEN_CHANGELOG.md.
# ------------------------------------------------------------
specimens_path <- read_latest("data/cabr_surveys/lethal", "^CABR_bee_specimens_V")
cat("Loading specimens:", basename(specimens_path), "\n")

raw_bee_data <- read_excel(specimens_path)
cat("Loaded", nrow(raw_bee_data), "specimen rows\n")

# ------------------------------------------------------------
# STEP 2: Validate structure
# Fails loudly here, not silently three steps from now, if a future
# version reorders or renames a column this script depends on.
# ------------------------------------------------------------
require_columns(
  raw_bee_data,
  c("date", "latitude", "longitude", "taxon_genus_name", "taxon_species_name"),
  "raw_bee_data"
)

# ------------------------------------------------------------
# STEP 3: Parse dates
# date is stored as a real date/datetime type as of V9 (not text),
# so we use as_date() rather than mdy() string parsing.
# ------------------------------------------------------------
clean_bee_data <- raw_bee_data %>%
  mutate(
    date_clean = as_date(date),
    month = month(date_clean),
    day   = day(date_clean),
    year  = year(date_clean),
    across(where(is.character), ~ na_if(.x, ""))
  )

# ------------------------------------------------------------
# STEP 4: QC flags
# ------------------------------------------------------------
clean_bee_data <- clean_bee_data %>%
  mutate(missing_latlong       = is.na(latitude) | is.na(longitude),
         missing_genus_species = is.na(taxon_genus_name) | is.na(taxon_species_name),
         missing_method_plant  = is.na(method_or_plant))

missing_latlong       <- clean_bee_data %>% filter(missing_latlong)
missing_genus_species <- clean_bee_data %>% filter(missing_genus_species)
missing_method_plant  <- clean_bee_data %>% filter(missing_method_plant)

# ------------------------------------------------------------
# STEP 5: Build join key
# Matches the genus_subgenus_species style used in the SD reference
# table so lethal specimens can be joined against it later.
# ------------------------------------------------------------
clean_bee_data <- clean_bee_data %>%
  mutate(oldscientificname = paste0(taxon_genus_name, "_", taxon_subgenus_name, "_", taxon_species_name))

# ------------------------------------------------------------
# STEP 6: Summary and save
# ------------------------------------------------------------
cat("\n--- SPECIMEN CLEANING SUMMARY ---\n")
cat("Total specimens:          ", nrow(clean_bee_data), "\n")
cat("Missing lat/long:         ", nrow(missing_latlong), "\n")
cat("Missing genus/species:    ", nrow(missing_genus_species), "\n")
cat("Missing method/plant:     ", nrow(missing_method_plant), "\n")
cat("Unique old_scientificname:", n_distinct(clean_bee_data$oldscientificname), "\n")
cat("Specimens with a known complex (taxon_complex_name):",
    sum(!is.na(clean_bee_data$taxon_complex_name)), "\n\n")

write.csv(
  clean_bee_data,
  "data/outputs/CABR_bee_specimens_clean.csv",
  row.names = FALSE
)
cat("Cleaned specimens saved to data/outputs/CABR_bee_specimens_clean.csv\n")

