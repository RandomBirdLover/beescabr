# =============================================================
# clean/specimen_clean.R
# beescabr pipeline -- CABR bee specimen cleaning (pure helpers)
# Created: 2026-07-13 (extracted from specimen_bee_clean.R)
#
# The testable, side-effect-free transforms behind specimen cleaning. The
# orchestrator (specimen_bee_clean.R) does the I/O (read the .xlsx, read the
# lookup/checklist CSVs, run the interactive review gate, write outputs) and
# calls these. Every function here is df-in / df-out (or a small decision),
# so the QC logic is unit-tested in test-specimen.R.
#
# Depends on: dplyr, stringr, lubridate.
# =============================================================

library(dplyr)
library(stringr)
library(lubridate)

# Parse the specimen date column into date_clean + month/day/year, and turn
# empty strings into NA across character columns.
parse_specimen_dates <- function(df) {
  df |>
    mutate(
      date_clean = as_date(date),
      month = month(date_clean),
      day   = day(date_clean),
      year  = year(date_clean),
      across(where(is.character), ~ na_if(.x, ""))
    )
}

# Standard scientific-name casing: Genus title-case, species/subspecies lower.
# Fixes a real source-data problem (ANDRENA / andrena) that broke downstream
# case-sensitive joins.
standardize_specimen_names <- function(df) {
  df |>
    mutate(
      genus      = str_to_title(genus),
      species    = str_to_lower(species),
      subspecies = str_to_lower(subspecies)
    )
}

# Fill blank family/subfamily/tribe from the taxonomy lookup (Holway-derived
# authority). iNat/source values win when present; the lookup only fills blanks.
fill_specimen_taxonomy <- function(df, tax_lookup) {
  df |>
    left_join(tax_lookup, by = c("genus", "species", "subspecies"), suffix = c("", "_lookup")) |>
    mutate(
      family    = coalesce(na_if(family, ""),    family_lookup),
      subfamily = coalesce(na_if(subfamily, ""), subfamily_lookup),
      tribe     = coalesce(na_if(tribe, ""),     tribe_lookup)
    ) |>
    select(-ends_with("_lookup"))
}

# Build the "known names" sets used by the spell-check, from the taxonomy
# lookup + the SD County iNat checklist (second authority for valid names not
# yet in Holway). Returns list(genera, genus_species).
build_known_names <- function(tax_check, inat_species) {
  known_genera <- unique(c(tax_check$genus, inat_species$genus))
  known_genus_species <- bind_rows(
    tax_check |> filter(!is.na(species), species != "") |> distinct(genus, species),
    inat_species
  ) |> distinct(genus, species)
  list(genera = known_genera, genus_species = known_genus_species)
}

# Spell-check: flag (1) genus not in the known set, (2) known genus but the
# genus+species combo isn't known. Returns one row per flagged specimen.
compute_taxonomy_flags <- function(df, known_genera, known_genus_species) {
  df |>
    filter(!is.na(genus), genus != "") |>
    mutate(
      flag_unknown_genus   = !(genus %in% known_genera),
      flag_unknown_species = !flag_unknown_genus & !is.na(species) & species != "" &
        !paste(genus, species) %in% paste(known_genus_species$genus, known_genus_species$species)
    ) |>
    filter(flag_unknown_genus | flag_unknown_species) |>
    mutate(flag_reason = case_when(
      flag_unknown_genus   ~ "genus not in taxonomy lookup",
      flag_unknown_species ~ "genus+species combo not in taxonomy lookup"
    )) |>
    select(any_of(c("ucsd_id", "sdnhm_id")), genus, species, subspecies, flag_reason) |>
    distinct()
}

# Decide what to do about spell-check flags. PURE (prompt injected):
#   0 flags            -> "clean"
#   flags, batch mode  -> "continue" (log & proceed; the automated pipeline)
#   flags, interactive -> prompt; "continue" on y, "stop" otherwise
resolve_flag_gate <- function(n_flags, interactive_ok, prompt_fn = readline) {
  if (n_flags == 0) return("clean")
  if (!interactive_ok) return("continue")
  ans <- prompt_fn("  Have you reviewed the flags above and fixed the source .xlsx? (y = fixed / confirmed, n = stop and fix now): ")
  if (tolower(trimws(ans)) == "y") "continue" else "stop"
}

# QC flags: which required-data fields are missing. genus is the one rank
# expected on every specimen; species is deliberately NOT flagged.
add_qc_flags <- function(df) {
  df |>
    mutate(
      missing_latlong  = is.na(latitude) | is.na(longitude),
      missing_date     = is.na(date),
      missing_sdnhm_id = is.na(sdnhm_id) | sdnhm_id == "",
      missing_ucsd_id  = is.na(ucsd_id) | ucsd_id == "",
      missing_genus    = is.na(genus) | genus == ""
    )
}

# Duplicate ID detection: any repeated ucsd_id (should be unique), or repeated
# sdnhm_id excluding 0/NA (0 is the intentional "needs new tag" sentinel).
detect_duplicate_ids <- function(df) {
  dup_ucsd <- df |>
    filter(duplicated(ucsd_id) | duplicated(ucsd_id, fromLast = TRUE)) |>
    mutate(duplicate_reason = "duplicate ucsd_id")
  dup_sdnhm <- df |>
    filter(!is.na(sdnhm_id), sdnhm_id != 0, sdnhm_id != "") |>
    filter(duplicated(sdnhm_id) | duplicated(sdnhm_id, fromLast = TRUE)) |>
    mutate(duplicate_reason = "duplicate sdnhm_id")
  bind_rows(dup_ucsd, dup_sdnhm) |>
    distinct(ucsd_id, .keep_all = TRUE) |>
    arrange(sdnhm_id, ucsd_id)
}

# Build the species-level complex match lookup from the SD County iNat
# checklist (only species rows that belong to a complex are valid targets).
build_complex_lookup <- function(checklist) {
  checklist |>
    filter(!is.na(species), species != "", !is.na(complex), complex != "") |>
    transmute(
      genus   = str_to_lower(genus),
      species = str_to_lower(species),
      complex_match          = complex,
      complex_taxon_id_match = complex_taxon_id
    ) |>
    distinct()
}

# Apply the complex match -- gated on BOTH genus and species present. Matched
# names are prefixed "(Complex) " so a complex-level id isn't misread as a
# confirmed species.
match_specimen_complex <- function(df, complex_lookup) {
  df |>
    mutate(.mg = str_to_lower(genus), .ms = str_to_lower(species)) |>
    left_join(complex_lookup, by = c(".mg" = "genus", ".ms" = "species")) |>
    mutate(
      complex = ifelse(!is.na(genus) & genus != "" & !is.na(species) & species != "" & !is.na(complex_match),
                       paste0("(Complex) ", complex_match), NA_character_),
      complex_taxon_id = ifelse(!is.na(genus) & genus != "" & !is.na(species) & species != "",
                                complex_taxon_id_match, NA)
    ) |>
    select(-.mg, -.ms, -complex_match, -complex_taxon_id_match)
}

# Build old_scientific_name from old_genus_name + old_species_name (blank/genus-
# only/binomial cases), for advisor-facing name-change tracking.
build_old_scientific_name <- function(df) {
  df |>
    mutate(old_scientific_name = case_when(
      (is.na(old_genus_name) | old_genus_name == "") &
        (is.na(old_species_name) | old_species_name == "") ~ NA_character_,
      (is.na(old_species_name) | old_species_name == "")   ~ old_genus_name,
      TRUE                                                  ~ paste(old_genus_name, old_species_name)
    ))
}

# Defensive: strip embedded control/null bytes from character columns so the
# saved CSV re-reads cleanly.
strip_control_chars <- function(df) {
  df |> mutate(across(where(is.character), ~ str_replace_all(.x, "[\\x00-\\x1F]", "")))
}
