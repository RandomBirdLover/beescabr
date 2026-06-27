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
# STEP 2b: Standardize genus/species/subspecies capitalization
# (2026-06-25). Per standard scientific naming convention: genus
# capitalized first letter (e.g. "Andrena"), species and subspecies
# all-lowercase (e.g. "cerasifolii"). Source data has been seen with
# inconsistent casing (e.g. "ANDRENA", "andrena") -- this was the root
# cause of a duplicate-detection bug downstream in
# native_bee_checklist.R (distinct() is case-sensitive, so two
# differently-cased copies of the same genus survived as "unique" rows
# and silently multiplied a join). Fixing casing here, at the source,
# means the saved CSV itself is consistent -- not just an in-memory
# workaround applied later in the pipeline.
# str_to_title() on genus handles any casing (ANDRENA, andrena,
# AnDrEnA) -> "Andrena" in one step.
# ------------------------------------------------------------
clean_bee_data <- clean_bee_data %>%
  mutate(
    genus      = str_to_title(genus),
    species    = str_to_lower(species),
    subspecies = str_to_lower(subspecies)
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
# STEP 7b: Defensive guard against embedded null bytes (2026-06-24).
# A previous run hit "duplicate 'row.names' are not allowed" /
# "embedded nul(s) found in input" when re-reading this exact CSV back
# in with read.csv() in native_bee_checklist.R. A direct scan of the
# V10 source xlsx (openpyxl) found NO null bytes or control characters
# in any cell, so the source file itself is clean -- this strip is
# just a cheap, harmless guard in case something gets introduced
# between read_excel() and write.csv() (e.g. by an OS/locale quirk),
# not a fix targeting a known bad cell.
# ------------------------------------------------------------
clean_bee_data <- clean_bee_data %>%
  mutate(across(where(is.character), ~ str_replace_all(.x, "[\\x00-\\x1F]", "")))

# ------------------------------------------------------------
# STEP 7c: Build old_scientific_name from old_genus_name +
# old_species_name (2026-06-24, for advisor-facing name-change
# tracking). Same blank-handling care as the rest of the pipeline:
#   - both blank        -> blank (no prior ID was ever revised)
#   - genus only        -> just the genus (not "Andrena NA")
#   - genus + species   -> full "Genus species" binomial
# Placed right after the two source columns, before STEP 8.
# ------------------------------------------------------------
clean_bee_data <- clean_bee_data %>%
  mutate(
    old_scientific_name = case_when(
      (is.na(old_genus_name) | old_genus_name == "") &
        (is.na(old_species_name) | old_species_name == "")     ~ NA_character_,
      (is.na(old_species_name) | old_species_name == "")       ~ old_genus_name,
      TRUE                                                     ~ paste(old_genus_name, old_species_name)
    )
  ) %>%
  relocate(old_scientific_name, .after = old_species_name)

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

# ------------------------------------------------------------
# STEP 8b: Name-change tracking list (2026-06-24) -- a quick reference
# of every unique old_scientific_name found in this version, each with
# the ucsd_id(s) of the specimen(s) it appeared on, so this can be
# handed to advisors as a running list of names that have changed.
# Specimens with no old_scientific_name (never revised) are excluded.
# ------------------------------------------------------------
old_name_changes <- clean_bee_data %>%
  filter(!is.na(old_scientific_name)) %>%
  group_by(old_scientific_name) %>%
  summarise(ucsd_ids = paste(unique(ucsd_id), collapse = ", "),
            n_specimens = n(),
            .groups = "drop") %>%
  arrange(old_scientific_name)

cat(sprintf("\n--- OLD NAME CHANGES (%d unique old name(s) found) ---\n", nrow(old_name_changes)))
if (nrow(old_name_changes) > 0) {
  print(old_name_changes)
} else {
  cat("None -- no specimens have an old_genus_name/old_species_name on file.\n")
}

write.csv(
  clean_bee_data,
  "data/outputs/cabr_bee_specimens_clean.csv",
  row.names = FALSE
)
cat("\nCleaned specimens saved to data/outputs/cabr_bee_specimens_clean.csv\n")
