# =============================================================
# checklists/taxonomy_reference.R
# beescabr pipeline -- bee_taxonomy_lookup.csv builder
# Created: 2026-07-13 (extracted from native_bee_checklist.R monolith, STEP 8)
#
# Builds the single reference file listing every taxonomic entry known to
# the pipeline -- one row per genus / subgenus / complex / species /
# subspecies plus higher-rank rows -- with every rank above it populated and
# an explicit `rank` column. Sources: Holway v3 (genus + species), the iNat
# taxon cache via the SD County Tier 1 checklist (subgenus + complex +
# subspecies), and the raw export (higher-rank-only observations).
#
# Logic is ported verbatim from the debugged monolith, including the
# two-pass join (genus/subgenus/complex on five keys; species/subspecies on
# three keys because Holway carries no complex) and the two-pass dedupe
# (taxon_id rows vs Holway-only NA-taxon_id rows).
#
# Depends on: dplyr, stringr.
# =============================================================

library(dplyr)
library(stringr)

if (!exists("holway_name_sets")) source("scripts/clean/verify.R")

BEE_KINGDOM     <- "Animalia"
BEE_PHYLUM      <- "Arthropoda"
BEE_CLASS       <- "Insecta"
BEE_ORDER       <- "Hymenoptera"
BEE_SUPERFAMILY <- "Apoidea"

TAXONOMY_COLUMN_ORDER <- c(
  "taxon_id", "scientific_name", "common_name",
  "kingdom", "phylum", "class", "order", "superfamily",
  "family", "subfamily", "tribe",
  "genus", "subgenus", "complex", "complex_taxon_id",
  "species", "subspecies", "rank"
)

.higher_rank_constants <- function(df) {
  df |> mutate(
    kingdom = BEE_KINGDOM, phylum = BEE_PHYLUM, class = BEE_CLASS,
    order = BEE_ORDER, superfamily = BEE_SUPERFAMILY
  )
}

build_bee_taxonomy_lookup <- function(holway_df, checklist_sd_county, bees,
                                      verified_ids = integer(0)) {

  holway_taxonomy <- holway_df |>
    mutate(species = str_trim(
      species_raw |>
        str_remove("^CF\\s+") |>
        str_remove("^MSN\\s+") |>
        str_remove("\\s+sp\\.\\s*nov\\.$")
    ))

  # --- GENUS ROWS (Holway) ---
  genus_rows <- holway_taxonomy |>
    filter(!is.na(genus), genus != "") |>
    distinct(family, subfamily, tribe, genus) |>
    arrange(genus, family, subfamily, tribe) |>
    group_by(genus) |> slice(1) |> ungroup() |>
    .higher_rank_constants() |>
    mutate(subgenus = NA_character_, complex = NA_character_,
           species = NA_character_, subspecies = NA_character_, rank = "genus")

  # --- SPECIES ROWS (Holway) --- strip subgenus parens to match iNat notation
  species_rows <- holway_taxonomy |>
    filter(!is.na(genus), genus != "", !is.na(species), species != "") |>
    mutate(subgenus = str_remove(str_remove(subgenus, "^\\("), "\\)$")) |>
    distinct(family, subfamily, tribe, genus, subgenus, species) |>
    .higher_rank_constants() |>
    mutate(complex = NA_character_, subspecies = NA_character_, rank = "species",
           subgenus = ifelse(subgenus == "", NA_character_, subgenus))

  # helper: Holway-first, iNat-export fallback for family/subfamily/tribe
  with_holway_fallback <- function(df) {
    df |>
      left_join(genus_rows |> select(genus,
                                     holway_family = family,
                                     holway_subfamily = subfamily,
                                     holway_tribe = tribe),
                by = "genus") |>
      mutate(
        family    = coalesce(holway_family, family),
        subfamily = coalesce(holway_subfamily, subfamily),
        tribe     = coalesce(holway_tribe, tribe)
      ) |>
      select(-holway_family, -holway_subfamily, -holway_tribe)
  }

  # --- SUBGENUS ROWS (iNat) ---
  subgenus_rows <- checklist_sd_county |>
    filter(!is.na(subgenus), subgenus != "") |>
    distinct(genus, subgenus, family, subfamily, tribe) |>
    with_holway_fallback() |>
    .higher_rank_constants() |>
    mutate(complex = NA_character_, species = NA_character_,
           subspecies = NA_character_, rank = "subgenus")

  # --- COMPLEX ROWS (iNat) ---
  complex_rows <- checklist_sd_county |>
    filter(!is.na(complex), complex != "") |>
    distinct(genus, subgenus, complex, family, subfamily, tribe) |>
    with_holway_fallback() |>
    .higher_rank_constants() |>
    mutate(species = NA_character_, subspecies = NA_character_, rank = "complex")

  # --- SUBSPECIES ROWS (iNat) ---
  subspecies_rows <- checklist_sd_county |>
    filter(!is.na(subspecies), subspecies != "") |>
    distinct(genus, subgenus, species, subspecies, family, subfamily, tribe) |>
    with_holway_fallback() |>
    .higher_rank_constants() |>
    mutate(complex = NA_character_, rank = "subspecies")

  inat_ref <- checklist_sd_county |>
    select(taxon_id, scientific_name, common_name, genus, subgenus,
           complex, complex_taxon_id, species, subspecies)

  # Two-pass join: higher-group rows on five keys; species-group on three.
  higher_group_rows <- bind_rows(
    genus_rows    |> mutate(complex_taxon_id = NA_integer_),
    subgenus_rows |> mutate(complex_taxon_id = NA_integer_),
    complex_rows
  ) |>
    left_join(inat_ref, by = c("genus", "subgenus", "complex", "species", "subspecies"),
              relationship = "many-to-many") |>
    mutate(complex_taxon_id = coalesce(complex_taxon_id.y, complex_taxon_id.x)) |>
    select(-complex_taxon_id.x, -complex_taxon_id.y) |>
    distinct()

  # --- iNat SPECIES ROWS --- species OBSERVED on iNat (incl. ones Holway
  # doesn't list, e.g. Agapostemon subtilior). This is the fix so the lookup
  # is truly Holway + iNat, not Holway-only at species level.
  inat_species_rows <- checklist_sd_county |>
    filter(!is.na(species), species != "") |>
    distinct(genus, subgenus, species, family, subfamily, tribe) |>
    with_holway_fallback() |>
    .higher_rank_constants() |>
    mutate(complex = NA_character_, subspecies = NA_character_, rank = "species")

  species_group_rows <- bind_rows(
    species_rows      |> mutate(complex_taxon_id = NA_integer_),
    inat_species_rows |> mutate(complex_taxon_id = NA_integer_),
    subspecies_rows   |> mutate(complex_taxon_id = NA_integer_)
  ) |>
    left_join(inat_ref |> rename(inat_complex = complex,
                                 inat_complex_taxon_id = complex_taxon_id),
              by = c("genus", "species", "subspecies"),
              relationship = "many-to-many") |>
    mutate(
      complex = coalesce(inat_complex, complex),
      complex_taxon_id = coalesce(inat_complex_taxon_id, complex_taxon_id)
    ) |>
    select(-inat_complex, -inat_complex_taxon_id) |>
    distinct()

  genus_below_rows <- bind_rows(higher_group_rows, species_group_rows)

  # --- HIGHER-RANK ROWS (family/subfamily/tribe/epifamily) from raw export ---
  higher_rank_rows <- bees |>
    filter(is.na(genus) | genus == "") |>
    select(taxon_id, scientific_name, common_name, family, subfamily, tribe) |>
    distinct(taxon_id, .keep_all = TRUE) |>
    filter(!is.na(taxon_id)) |>
    mutate(
      rank = case_when(
        !is.na(tribe)     & tribe     != "" ~ "tribe",
        !is.na(subfamily) & subfamily != "" ~ "subfamily",
        !is.na(family)    & family    != "" ~ "family",
        TRUE ~ "epifamily"
      ),
      kingdom = BEE_KINGDOM, phylum = BEE_PHYLUM, class = BEE_CLASS,
      order = BEE_ORDER, superfamily = BEE_SUPERFAMILY,
      genus = NA_character_, subgenus = NA_character_, complex = NA_character_,
      complex_taxon_id = NA_integer_, species = NA_character_, subspecies = NA_character_
    )

  # --- COMBINE + two-pass dedupe (taxon_id rows vs Holway-only NA rows) ---
  combined <- bind_rows(
    genus_below_rows |> select(any_of(TAXONOMY_COLUMN_ORDER)),
    higher_rank_rows |> select(any_of(TAXONOMY_COLUMN_ORDER))
  )

  deduped <- bind_rows(
    combined |> filter(!is.na(taxon_id)) |> distinct(taxon_id, .keep_all = TRUE),
    combined |> filter(is.na(taxon_id))  |> distinct(genus, species, subspecies, .keep_all = TRUE)
  )

  # --- subtribe: pull from the observations by taxon_id (Holway has none) ---
  if ("subtribe" %in% names(bees)) {
    subtribe_map <- bees |> filter(!is.na(taxon_id)) |>
      distinct(taxon_id, subtribe)
    deduped <- deduped |> left_join(subtribe_map, by = "taxon_id")
  }
  if (!"subtribe" %in% names(deduped)) deduped$subtribe <- NA_character_

  # --- holway_status: Described / Tentative / Unpublished, kept for every
  # Holway species (so Tentative & Unpublished rows stay and are identifiable);
  # NA for iNat-only species and higher ranks. ---
  if ("source_sheet" %in% names(holway_taxonomy)) {
    holway_status_map <- holway_taxonomy |>
      filter(!is.na(genus), genus != "", !is.na(species), species != "") |>
      mutate(.key = paste(tolower(genus), tolower(species))) |>
      distinct(.key, .keep_all = TRUE) |>
      transmute(.key, holway_status = source_sheet)
    deduped <- deduped |>
      mutate(.key = ifelse(!is.na(species) & species != "",
                           paste(tolower(genus), tolower(species)), NA_character_)) |>
      left_join(holway_status_map, by = ".key") |>
      select(-.key)
  } else {
    deduped$holway_status <- NA_character_
  }

  # --- verified: TRUE if the taxon is in Holway or you've verified its id ---
  sets <- holway_name_sets(holway_df)
  flagged <- flag_new_taxa(deduped, sets, verified_ids = verified_ids)
  deduped$verified <- !flagged$needs_verification

  # --- final column order: metadata first, then the taxonomic hierarchy ---
  ordered <- c("taxon_id", "scientific_name", "rank", "verified", "holway_status",
               TAXONOMY_LEVELS, "complex_taxon_id", "common_name")
  deduped |>
    select(any_of(ordered), everything()) |>
    arrange(family, genus, rank, subgenus, complex, species, subspecies)
}
