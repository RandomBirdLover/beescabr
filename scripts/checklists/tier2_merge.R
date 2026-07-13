# =============================================================
# checklists/tier2_merge.R
# beescabr pipeline -- Tier 2 (merged, Holway-format) checklist construction
# Created: 2026-07-13 (extracted from native_bee_checklist.R monolith, PART B)
#
# Builds the merged Tier 2 checklists that fold CABR specimen evidence and
# (SD County only) a Holway cross-check into the Tier 1 iNat checklists,
# formatted to match Dr. Holway's workbook columns plus Complex/Subspecies.
#
# The subtle join behavior is preserved verbatim from the debugged monolith:
#   - genus-only rows are KEPT (genus-minimum rule), match_key = NA for them
#   - the specimen join uses na_matches = "never" so NA keys never collide
#   - specimen evidence rolls up genus/subgenus; genus-only specimens are
#     matched separately
# See the inline notes; these encode real bugs that were chased down.
#
# Depends on: dplyr, stringr.
# =============================================================

library(dplyr)
library(stringr)

# ------------------------------------------------------------
# specimen_species_table(): unique genus+species combos (lowercased BEFORE
# distinct, to catch case-variant duplicates at the source) flagged as
# having a CABR specimen. Used for the Recent-survey / Museum-collection join.
# ------------------------------------------------------------
specimen_species_table <- function(specimens) {
  specimens |>
    filter(!is.na(genus), genus != "") |>
    mutate(genus = str_to_lower(genus), species = str_to_lower(species)) |>
    distinct(genus, species) |>
    mutate(has_cabr_specimen = TRUE)
}

# ------------------------------------------------------------
# build_specimen_checklist(): unique taxa (genus required) straight from the
# specimen sheet -- the specimen-only checklist, column-parallel to the iNat
# Tier 1 file for direct diffing.
# ------------------------------------------------------------
build_specimen_checklist <- function(specimens) {
  cols <- c("order", "family", "subfamily", "tribe", "genus", "subgenus",
            "complex", "complex_taxon_id", "species", "subspecies")
  specimens |>
    select(any_of(cols)) |>
    distinct() |>
    filter(!is.na(genus), genus != "") |>
    arrange(family, genus, subgenus, species)
}

# ------------------------------------------------------------
# build_tier2_checklist(): merge one Tier 1 checklist with specimen evidence
# (optional) and a Holway cross-check (optional). Returns the Holway-format
# output frame.
# ------------------------------------------------------------
build_tier2_checklist <- function(tier1_checklist, specimen_species,
                                  holway_keys = character(0),
                                  run_holway_check = FALSE, label = "") {

  # match_key only when species is populated -- genus-only rows keep NA so
  # they never spuriously match (paste() would collapse them to "genus_na").
  checklist <- tier1_checklist |>
    mutate(match_key = ifelse(!is.na(species) & species != "",
                              paste(str_to_lower(genus), str_to_lower(species), sep = "_"),
                              NA_character_))

  if (!is.null(specimen_species)) {
    checklist <- checklist |>
      left_join(
        specimen_species |>
          mutate(match_key = ifelse(!is.na(species) & species != "",
                                    paste(str_to_lower(genus), str_to_lower(species), sep = "_"),
                                    NA_character_)) |>
          select(match_key, has_cabr_specimen),
        by = "match_key",
        na_matches = "never"   # NA key never matches NA key (see monolith notes)
      )

    genera_with_specimen <- checklist |>
      filter(!is.na(has_cabr_specimen) & has_cabr_specimen) |>
      distinct(genus) |> mutate(genus_rollup = TRUE)

    subgenera_with_specimen <- checklist |>
      filter(!is.na(has_cabr_specimen) & has_cabr_specimen,
             !is.na(subgenus) & subgenus != "") |>
      distinct(genus, subgenus) |> mutate(subgenus_rollup = TRUE)

    checklist <- checklist |>
      left_join(genera_with_specimen, by = "genus") |>
      left_join(subgenera_with_specimen, by = c("genus", "subgenus")) |>
      mutate(has_cabr_specimen = case_when(
        !is.na(has_cabr_specimen) & has_cabr_specimen ~ TRUE,
        (is.na(species) | species == "") & !is.na(subgenus) & subgenus != "" &
          !is.na(subgenus_rollup) ~ TRUE,
        (is.na(species) | species == "") & (is.na(subgenus) | subgenus == "") &
          !is.na(genus_rollup) ~ TRUE,
        TRUE ~ has_cabr_specimen
      )) |>
      select(-genus_rollup, -subgenus_rollup)

    # genus-only specimens (species = NA) matched directly to genus-only rows.
    # specimen_species genus is lowercased (for case-insensitive dedup), so the
    # join is done on a lowercased key to avoid an "andrena" != "Andrena" miss.
    genus_only_specimen_genera <- specimen_species |>
      filter(is.na(species) | species == "") |>
      mutate(genus_lc = str_to_lower(genus)) |>
      distinct(genus_lc) |> mutate(genus_direct = TRUE)

    checklist <- checklist |>
      mutate(genus_lc = str_to_lower(genus)) |>
      left_join(genus_only_specimen_genera, by = "genus_lc") |>
      mutate(has_cabr_specimen = case_when(
        !is.na(has_cabr_specimen) & has_cabr_specimen ~ TRUE,
        (is.na(species) | species == "") & (is.na(subgenus) | subgenus == "") &
          !is.na(genus_direct) ~ TRUE,
        TRUE ~ has_cabr_specimen
      )) |>
      select(-genus_direct, -genus_lc)
  } else {
    checklist$has_cabr_specimen <- NA
  }

  checklist <- checklist |>
    mutate(
      Family = family, Subfamily = subfamily, Tribe = tribe,
      Genus = genus, Subgenus = subgenus,
      Complex = ifelse(!is.na(complex) & complex != "",
                       paste0("(Complex) ", complex), NA_character_),
      Species = species, Subspecies = subspecies,
      Authority = NA_character_,
      `Museum Collection` = ifelse(!is.na(has_cabr_specimen) & has_cabr_specimen, "X", NA_character_),
      Literature = NA_character_,
      iNaturalist = "X",
      Notes = NA_character_
    )

  if (run_holway_check) {
    checklist <- checklist |>
      mutate(`Found in Holway checklist?` = case_when(
        is.na(species) | species == "" ~ NA_character_,
        match_key %in% holway_keys ~ "Yes",
        TRUE ~ "No"
      ))
    n_missing <- sum(checklist$`Found in Holway checklist?` == "No", na.rm = TRUE)
    message(sprintf("%s: %d species not found in Holway v3 (flagged)", label, n_missing))
  }

  output_cols <- c("Family", "Subfamily", "Tribe", "Genus", "Subgenus", "Complex",
                   "Species", "Subspecies", "Authority", "Museum Collection",
                   "Literature", "iNaturalist", "Notes")
  if (run_holway_check) output_cols <- c(output_cols, "Found in Holway checklist?")

  checklist |>
    select(all_of(output_cols)) |>
    arrange(Family, Genus, Subgenus, Species)
}
