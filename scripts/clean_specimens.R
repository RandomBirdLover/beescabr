# =============================================================
# Clean CABR Bee Specimens (Lethal Survey Data)
# Created: June 21, 2026
# Rewritten: June 24, 2026 -- short, focused script: load specimens,
#            QC-flag specific fields, match complex/complex_taxon_id
#            from the SD County iNat checklist, save.
# Author: Brandi Sanchez
# Data: CABR_bee_specimens_V{n}_{YYYY-MM-DD}.xlsx
#       (data/cabr_surveys/lethal/) — newest version auto-detected
#
# Column naming: as of V10, the specimen sheet uses bare rank names
# (order, family, subfamily, tribe, genus, subgenus, species,
# subspecies, complex) -- no taxon_*_name prefix/suffix. Read directly,
# no renaming needed here.
#
# QC scope (2026-06-24, per project decision): flags missing
# lat/long, missing date, missing sdnhm_id, missing ucsd_id, and
# missing genus -- genus is the one rank expected to be identified on
# EVERY specimen. Deliberately does NOT flag missing species or
# missing method_or_plant: not every specimen is identified to
# species (see SPECIMEN_CHANGELOG.md known-data-notes -- this is
# backlog, not an error), and method/plant gaps are out of scope here.
#
# Complex matching: gated on species-level ID -- complex/
# complex_taxon_id are only ever populated when BOTH genus and species
# are present, since complex is metadata about a confirmed species,
# not an independent identification claim. Match source is
# data/outputs/SD_county_inat_native_bee_checklist.csv (SD-County-wide
# -- broadest tier, guaranteed to contain every CABR taxon). The
# checklist has two kinds of complex-tagged rows: complex-LEVEL taxon
# entries (species blank -- the row IS the complex, e.g.
# "Andrena cerasifolii" can look exactly like a species binomial) and
# species that BELONG TO a complex (species populated --
# the only valid match targets). complex/complex_taxon_id are fully
# recomputed from the checklist on every run, not merged with whatever
# was already in the sheet.
#
# complex values are written as "(Complex) Name" (e.g. "(Complex)
# Diadasia australis") -- some complex names look exactly like a
# normal species binomial, so without the prefix a complex-level ID
# could be misread as a confirmed species.
#
# Output: clean_bee_data (data frame in environment), plus
#         missing_latlong, missing_date, missing_sdnhm_id,
#         missing_ucsd_id, missing_genus, and missing_specimens_list
#         (specimens where missing_specimen == "Y").
#         Written to data/outputs/cabr_bee_specimens_clean.csv and
#         data/outputs/cabr_missing_specimens_list.csv
#         Column order enforces genus, subgenus, complex,
#         complex_taxon_id, species, subspecies in that exact sequence.
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

require_columns(
  raw_bee_data,
  c("date", "latitude", "longitude", "sdnhm_id", "ucsd_id", "accession", "locality",
    "genus", "subgenus", "complex", "species", "subspecies", "missing_specimen"),
  "raw_bee_data"
)

# ------------------------------------------------------------
# STEP 2: Parse date
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
# STEP 3: QC flags -- see header note on scope.
# ------------------------------------------------------------
clean_bee_data <- clean_bee_data %>%
  mutate(missing_latlong  = is.na(latitude) | is.na(longitude),
         missing_date     = is.na(date),
         missing_sdnhm_id = is.na(sdnhm_id) | sdnhm_id == "",
         missing_ucsd_id  = is.na(ucsd_id) | ucsd_id == "",
         missing_genus    = is.na(genus) | genus == "")

missing_latlong  <- clean_bee_data %>% filter(missing_latlong)
missing_date     <- clean_bee_data %>% filter(missing_date)
missing_sdnhm_id <- clean_bee_data %>% filter(missing_sdnhm_id)
missing_ucsd_id  <- clean_bee_data %>% filter(missing_ucsd_id)
missing_genus    <- clean_bee_data %>% filter(missing_genus)

# ------------------------------------------------------------
# STEP 4: List of specimens flagged as physically missing
# (missing_specimen == "Y" in the raw sheet -- a different thing from
# the missing_* QC flags above, which are about missing DATA, not
# missing physical specimens). Keeps ALL columns, just filtered to
# the missing rows -- no column narrowing.
# ------------------------------------------------------------
missing_specimens_list <- clean_bee_data %>%
  filter(missing_specimen == "Y")

cat(sprintf("\nSpecimens flagged as physically missing (missing_specimen == 'Y'): %d\n",
            nrow(missing_specimens_list)))

write.csv(
  missing_specimens_list,
  "data/outputs/cabr_missing_specimens_list.csv",
  row.names = FALSE
)
cat("Missing-specimens list saved to data/outputs/cabr_missing_specimens_list.csv\n")

# ------------------------------------------------------------
# STEP 5: Load the checklist and build the complex match lookup --
# species-level rows only (see header note on the two kinds of
# complex-tagged rows). distinct() guards against any duplicate
# genus+species keys silently multiplying specimen rows in the join.
# ------------------------------------------------------------
checklist_path <- "data/outputs/SD_county_inat_native_bee_checklist.csv"
checklist <- read.csv(checklist_path)
require_columns(checklist,
                 c("genus", "species", "complex", "complex_taxon_id"),
                 "checklist")

complex_lookup <- checklist %>%
  filter(!is.na(species), species != "",
         !is.na(complex), complex != "") %>%
  transmute(
    genus  = str_to_lower(genus),
    species = str_to_lower(species),
    complex_match    = complex,
    complex_taxon_id_match = complex_taxon_id
  ) %>%
  distinct()

cat(sprintf("\nComplex lookup built: %d valid species-level match targets.\n", nrow(complex_lookup)))

# ------------------------------------------------------------
# STEP 6: Apply the match -- gated on genus AND species both being
# populated. Genus-only/unidentified specimens are left with
# complex = NA, complex_taxon_id = NA, no exceptions. Matched complex
# values are prefixed "(Complex) " -- see header note.
# ------------------------------------------------------------
clean_bee_data <- clean_bee_data %>%
  mutate(.match_genus = str_to_lower(genus), .match_species = str_to_lower(species)) %>%
  left_join(complex_lookup, by = c(".match_genus" = "genus", ".match_species" = "species")) %>%
  mutate(
    complex = ifelse(!is.na(genus) & genus != "" & !is.na(species) & species != "" & !is.na(complex_match),
                      paste0("(Complex) ", complex_match), NA_character_),
    complex_taxon_id = ifelse(!is.na(genus) & genus != "" & !is.na(species) & species != "",
                               complex_taxon_id_match, NA)
  ) %>%
  select(-.match_genus, -.match_species, -complex_match, -complex_taxon_id_match)

n_matched <- sum(!is.na(clean_bee_data$complex))
cat(sprintf("Specimens matched to a complex: %d of %d.\n", n_matched, nrow(clean_bee_data)))

# ------------------------------------------------------------
# STEP 7: Enforce column order -- genus, subgenus, complex,
# complex_taxon_id, species, subspecies, in that exact sequence.
# ------------------------------------------------------------
clean_bee_data <- clean_bee_data %>%
  relocate(genus, subgenus, complex, complex_taxon_id, species, subspecies,
           .after = tribe)

# ------------------------------------------------------------
# STEP 8: Summary and save
# ------------------------------------------------------------
cat("\n--- SPECIMEN CLEANING SUMMARY ---\n")
cat("Total specimens:    ", nrow(clean_bee_data), "\n")
cat("Missing lat/long:   ", nrow(missing_latlong), "\n")
cat("Missing date:       ", nrow(missing_date), "\n")
cat("Missing sdnhm_id:   ", nrow(missing_sdnhm_id), "\n")
cat("Missing ucsd_id:    ", nrow(missing_ucsd_id), "\n")
cat("Missing genus:      ", nrow(missing_genus), "\n")
cat("Physically missing: ", nrow(missing_specimens_list), "\n")

write.csv(
  clean_bee_data,
  "data/outputs/cabr_bee_specimens_clean.csv",
  row.names = FALSE
)
cat("\nCleaned specimens saved to data/outputs/cabr_bee_specimens_clean.csv\n")
