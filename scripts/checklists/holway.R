# =============================================================
# checklists/holway.R
# beescabr pipeline -- Holway v3 reference helpers
# Created: 2026-07-13 (extracted from native_bee_checklist.R monolith)
#
# Everything that reads or reconciles Dr. Holway's v3 combined checklist:
#   - genus-level family/subfamily/tribe lookup (for backfilling blanks in
#     the iNat taxonomy), with the same "collapse conflicting tribes,
#     first-alphabetical wins, warn loudly" behavior as the old STEP 1b.
#   - qualifier stripping ("CF ", "MSN ", " sp. nov.") to turn species_raw
#     into a clean epithet.
#   - the genus+species match-key set used for the SD County Holway
#     cross-check.
#
# The transform functions are pure (df in, df/vector out) and unit-tested;
# only load_holway() touches disk.
#
# Depends on: dplyr, stringr.
# =============================================================

library(dplyr)
library(stringr)

load_holway <- function(path) utils::read.csv(path, stringsAsFactors = FALSE)

# ------------------------------------------------------------
# clean_holway_species(): strip tentative/unpublished qualifiers so an
# entry like "CF annectens" or "MSN foo sp. nov." becomes a bare epithet.
# Vectorized, pure.
# ------------------------------------------------------------
clean_holway_species <- function(species_raw) {
  species_raw |>
    str_remove("^CF\\s+") |>
    str_remove("^MSN\\s+") |>
    str_remove("\\s+sp\\.\\s*nov\\.$") |>
    str_trim()
}

# ------------------------------------------------------------
# holway_genus_taxonomy(): one row per genus with its family/subfamily/tribe.
# Holway occasionally files a genus under two tribes (a documented
# inconsistency, not a real split); we collapse to the first alphabetical
# tribe and, if `warn`, surface the conflicting genera so they stay visible.
# Returns columns: genus, family_holway, subfamily_holway, tribe_holway.
# ------------------------------------------------------------
holway_genus_taxonomy <- function(holway_df, warn = TRUE) {
  raw <- holway_df |>
    filter(!is.na(genus), genus != "") |>
    distinct(genus, family, subfamily, tribe)

  conflicts <- raw |>
    group_by(genus) |>
    summarise(n_combos = n(), .groups = "drop") |>
    filter(n_combos > 1)

  if (warn && nrow(conflicts) > 0) {
    message("NOTE: Holway has conflicting higher-rank assignments for genus/genera: ",
            paste(conflicts$genus, collapse = ", "),
            ". Taking first (alphabetical) tribe per genus for backfill.")
  }

  raw |>
    arrange(genus, family, subfamily, tribe) |>
    group_by(genus) |>
    slice(1) |>
    ungroup() |>
    rename(family_holway = family, subfamily_holway = subfamily, tribe_holway = tribe)
}

# ------------------------------------------------------------
# backfill_taxonomy(): fill blank family/subfamily/tribe on iNat observations
# from the Holway genus lookup. iNat values win when present (coalesce);
# Holway only fills blanks. Returns the bees df with columns coalesced and
# the *_holway helper columns dropped.
# ------------------------------------------------------------
backfill_taxonomy <- function(bees, genus_lookup) {
  bees |>
    left_join(genus_lookup, by = "genus") |>
    mutate(
      family    = coalesce(ifelse(family    == "", NA_character_, family),    family_holway),
      subfamily = coalesce(ifelse(subfamily == "", NA_character_, subfamily), subfamily_holway),
      tribe     = coalesce(ifelse(tribe     == "", NA_character_, tribe),     tribe_holway)
    ) |>
    select(-family_holway, -subfamily_holway, -tribe_holway)
}

# ------------------------------------------------------------
# holway_match_keys(): distinct "genus_species" keys (lowercased,
# qualifier-stripped) for the cross-check. Pure.
# ------------------------------------------------------------
holway_match_keys <- function(holway_df) {
  holway_df |>
    mutate(
      species_clean = clean_holway_species(species_raw),
      match_key = paste(str_to_lower(genus), str_to_lower(species_clean), sep = "_")
    ) |>
    filter(match_key != "_", !is.na(genus), genus != "") |>
    pull(match_key) |>
    unique()
}
