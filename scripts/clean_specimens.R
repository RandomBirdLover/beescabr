# =============================================================
# Clean CABR Bee Specimens (Lethal Survey Data)
# Created: June 21, 2026
# Author: Brandi Sanchez
# Data: CABR_bee_specimens_V{n}_{YYYY-MM-DD}.xlsx
#       (data/cabr_surveys/lethal/) — newest version auto-detected
# Description: Loads and cleans the authoritative CABR bee specimen
#              spreadsheet. Validates expected columns, parses dates,
#              flags missing coordinates/genus/species/method, and
#              builds an oldscientificname key for joining against
#              the SD reference table (native_bee_reference_table.R).
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
  c("Date", "Latitude", "Longitude", "Genus", "Species"),
  "raw_bee_data"
)

# ------------------------------------------------------------
# STEP 3: Parse dates
# ------------------------------------------------------------
clean_bee_data <- raw_bee_data %>%
  mutate(
    date_clean = mdy(Date),
    month = month(date_clean),
    day   = day(date_clean),
    year  = year(date_clean),
    across(where(is.character), ~ na_if(.x, ""))
  )

# ------------------------------------------------------------
# STEP 4: QC flags
# ------------------------------------------------------------
clean_bee_data <- clean_bee_data %>%
  mutate(missing_latlong       = is.na(Latitude) | is.na(Longitude),
         missing_genus_species = is.na(Genus) | is.na(Species))

missing_latlong       <- clean_bee_data %>% filter(missing_latlong)
missing_genus_species <- clean_bee_data %>% filter(missing_genus_species)

# Method/Plant column name can shift depending on how Excel merges
# headers between versions — find it by pattern, not a hardcoded
# mangled name like "Method...Plant".
method_plant_col <- names(clean_bee_data)[str_detect(names(clean_bee_data), "^Method")]

if (length(method_plant_col) == 1) {
  clean_bee_data <- clean_bee_data %>%
    mutate(missing_method_plant = is.na(.data[[method_plant_col]]))
  missing_method_plant <- clean_bee_data %>% filter(missing_method_plant)
} else {
  warning("Could not uniquely identify the Method/Plant column. Found: ",
          paste(method_plant_col, collapse = ", "),
          ". Check names(raw_bee_data) and update this script.")
  missing_method_plant <- tibble()
}

# ------------------------------------------------------------
# STEP 5: Build join key
# Matches the genus_subgenus_species style used in the SD reference
# table so lethal specimens can be joined against it later.
# ------------------------------------------------------------
clean_bee_data <- clean_bee_data %>%
  mutate(oldscientificname = paste0(Genus, "_", Subgenus, "_", Species))

# ------------------------------------------------------------
# STEP 6: Summary and save
# ------------------------------------------------------------
cat("\n--- SPECIMEN CLEANING SUMMARY ---\n")
cat("Total specimens:          ", nrow(clean_bee_data), "\n")
cat("Missing lat/long:         ", nrow(missing_latlong), "\n")
cat("Missing genus/species:    ", nrow(missing_genus_species), "\n")
cat("Missing method/plant:     ", nrow(missing_method_plant), "\n")
cat("Unique old_scientificname:", n_distinct(clean_bee_data$oldscientificname), "\n\n")

write.csv(
  clean_bee_data,
  "data/outputs/CABR_bee_specimens_clean.csv",
  row.names = FALSE
)
cat("Cleaned specimens saved to data/outputs/CABR_bee_specimens_clean.csv\n")
