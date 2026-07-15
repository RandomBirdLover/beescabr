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
    str_remove("^(aff\\.|AFF)\\s+") |>          # "aff. x" (lowercase) OR "AFF x" (uppercase, no period)
    str_remove("(^|\\s+)sp\\.\\s*nov\\.$") |>   # also strips a BARE "sp. nov." -> ""
    str_trim()
}

# ------------------------------------------------------------
# holway_qualifier(): PURE, vectorized. Pull out the tentative/unpublished MARKER
# that Holway prefixes or suffixes onto a provisional name -- "CF"/"MSN"/"aff."
# (leading) and "sp. nov." (trailing) -- and return it as a single string, so the
# reference table can record the name's STATUS in its own column while the species
# column keeps the clean epithet a user can look up on iNat/ITIS.
#   "CF annectens"            -> "CF"
#   "MSN atripes sp. nov."    -> "MSN sp. nov."
#   "aff. miserabilis sp. nov." -> "aff. sp. nov."
#   "AFF salicicola"          -> "aff."   (Holway also writes aff. as uppercase "AFF")
#   "sp. nov."                -> "sp. nov."
#   "robustior" / "affinis" / "a / b" -> NA  (plain names & slash pairs carry no marker;
#                                             a real epithet like "affinis" is NOT the aff. marker)
# ------------------------------------------------------------
holway_qualifier <- function(species_raw) {
  x     <- ifelse(is.na(species_raw), "", species_raw)
  # marker must be a standalone token (followed by whitespace), so "affinis" is
  # never mistaken for "aff."; "aff." (lowercase, with period) and "AFF" (uppercase,
  # no period) are the two forms Holway uses.
  m     <- str_match(x, "^(CF|MSN|AFF|aff\\.)\\s")[, 2]
  lead  <- ifelse(is.na(m), NA_character_, ifelse(m %in% c("AFF", "aff."), "aff.", m))
  trail <- ifelse(str_detect(x, "sp\\.\\s*nov\\.$"), "sp. nov.", NA_character_)
  ifelse(is.na(lead) & is.na(trail), NA_character_,
    ifelse(is.na(lead),  trail,
    ifelse(is.na(trail), lead, paste(lead, trail))))
}

# ------------------------------------------------------------
# split_holway_species(): Holway packs the subspecies epithet into species_raw
# ("cactorum basalis"). After stripping CF/MSN/sp. nov. qualifiers, the FIRST
# token is the species epithet and any remaining token(s) the subspecies.
#
# EXCEPTION -- a slash pair "epithet A / epithet B" is an either/or SPECIES pair
# (a synonym / uncertain identification), NEVER a subspecies. So for a slash row
# we keep the first name as the primary epithet and force subspecies = NA -- the
# part after the "/" must never be recorded as a subspecies. Which of A/B is
# actually correct is decided by a human in holway_reference_build.R (the
# interactive "which to use?" prompt); this pure split just avoids the bogus
# subspecies. Returns a tibble(species, subspecies); either is NA when absent.
# ------------------------------------------------------------
split_holway_species <- function(species_raw) {
  cleaned   <- clean_holway_species(species_raw)
  has_slash <- !is.na(cleaned) & str_detect(cleaned, "/")
  sp <- ifelse(has_slash,
               str_trim(word(str_extract(cleaned, "^[^/]*"), 1)),  # first name, before the "/"
               str_trim(word(cleaned, 1)))
  ss <- ifelse(has_slash, NA_character_, str_trim(word(cleaned, 2, -1)))
  tibble::tibble(
    species    = ifelse(is.na(sp) | sp == "", NA_character_, sp),
    subspecies = ifelse(is.na(ss) | ss == "", NA_character_, ss)
  )
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
# holway_match_keys(): distinct "genus_species" keys (lowercased) for the SD
# County Holway cross-check. Pure. Two things keep it from producing false
# "not in Holway" flags:
#   * SPECIES ONLY -- keys use the species epithet (word 1), never the packed
#     subspecies, so a Holway entry listed at subspecies level ("cactorum
#     basalis") still matches a species-level checklist row ("cactorum").
#   * SLASH EXPANDED -- a "epithet A / epithet B" pair yields a key for EACH
#     name, so a match on either counts (the pair is an either/or species, not
#     a subspecies).
# ------------------------------------------------------------
holway_match_keys <- function(holway_df) {
  holway_df |>
    filter(!is.na(genus), genus != "") |>
    select(genus, species_raw) |>
    tidyr::separate_rows(species_raw, sep = "\\s*/\\s*") |>          # "A / B" -> two rows
    mutate(species_epithet = str_to_lower(str_trim(word(clean_holway_species(species_raw), 1))),
           match_key = paste(str_to_lower(str_trim(genus)), species_epithet, sep = "_")) |>
    filter(!is.na(species_epithet), species_epithet != "") |>
    pull(match_key) |>
    unique()
}
