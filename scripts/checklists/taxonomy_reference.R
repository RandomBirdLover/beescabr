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
if (!exists("split_holway_species")) source("scripts/checklists/holway.R")

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

# ------------------------------------------------------------
# merge_holway_resolved(): fill taxon_id + scientific_name on the lookup's
# Holway genus/species rows from the enriched Holway reference table
# (holway_sd_bee_reference_table.csv, built by holway_reference_build.R).
# PURE. holway_resolved is the clean reference table (columns: taxon_id,
# scientific_name, rank, genus, species [bare epithet], ...). Species rows match
# on "genus epithet", genus rows on the genus name (both case-insensitive); we
# fill ONLY where the lookup still has no taxon_id, taking scientific_name
# straight from the reference table (the authoritative iNat taxon name). in_inat
# must already be computed BEFORE calling this, since a resolved-but-unobserved
# Holway taxon now carries a taxon_id yet was never observed on iNat.
# ------------------------------------------------------------
# parse_holway_decision_map(): PURE. From recorded 'pick' decisions (search_term
# = "Genus species_raw", chosen_taxon_id), build a map keyed on the ORIGINAL
# genus + cleaned epithet(s) -> taxon_id, used to merge renamed Holway rows.
parse_holway_decision_map <- function(decisions) {
  empty <- tibble(.dkey = character(0), dm_id = integer(0))
  if (is.null(decisions) || nrow(decisions) == 0) return(empty)
  d <- decisions |>
    filter(!is.na(chosen_taxon_id)) |>
    mutate(.g  = str_trim(word(search_term, 1)),
           .sp = str_trim(str_remove(search_term, "^\\S+\\s*"))) |>   # everything after genus
    filter(!is.na(.sp), .sp != "", !is.na(.g), .g != "")
  if (nrow(d) == 0) return(empty)
  d |>
    transmute(.dkey = str_squish(tolower(paste(.g, clean_holway_species(.sp)))),
              dm_id = as.integer(chosen_taxon_id)) |>
    filter(.dkey != "") |>
    distinct(.dkey, .keep_all = TRUE)
}

# build_holway_decision_map(): read the recorded 'pick' decisions from DuckDB and
# parse them into the merge map. Returns an empty map if the table is missing.
build_holway_decision_map <- function(con) {
  d <- tryCatch(
    DBI::dbGetQuery(con,
      "SELECT search_term, chosen_taxon_id FROM holway_decisions
       WHERE action = 'pick' AND chosen_taxon_id IS NOT NULL"),
    error = function(e) NULL)
  parse_holway_decision_map(d)
}

# reconcile_lookup_dupes(): PURE. Collapse rows that share a taxon_id into ONE,
# OR-ing the source-membership flags and keeping the observed/current name. This
# is what merges a renamed Holway row (e.g. "Calliopsis rhodophilus", blank id)
# with its resolved iNat twin ("Calliopsis rhodophila", 271415) once a taxon_id
# has been attached to the Holway row -- so the stale old-name row disappears.
# Rows with no taxon_id are passed through untouched.
reconcile_lookup_dupes <- function(df) {
  if (!"taxon_id" %in% names(df)) return(df)
  id_rows <- df |> filter(!is.na(taxon_id))
  na_rows <- df |> filter(is.na(taxon_id))
  if (nrow(id_rows) == 0) return(df)

  bool_or <- function(x) any(as.logical(x), na.rm = TRUE)
  nb_first <- function(x) {
    y <- if (is.character(x)) x[!is.na(x) & x != ""] else x[!is.na(x)]
    if (length(y)) y[[1]] else x[[1]]
  }
  flag_cols   <- intersect(c("verified", "in_holway", "in_inat", "in_cabr_specimens"), names(df))
  status_cols <- intersect(c("holway_status"), names(df))
  taxo_cols   <- setdiff(names(df), c("taxon_id", flag_cols, status_cols))

  merged <- id_rows |>
    # a non-blank scientific_name (the observed/current form) wins the taxonomy
    # columns; blank-name (old Holway) rows sort last.
    arrange(is.na(scientific_name) | scientific_name == "") |>
    group_by(taxon_id) |>
    summarise(across(all_of(taxo_cols),   nb_first),
              across(all_of(status_cols), nb_first),
              across(all_of(flag_cols),   bool_or),
              .groups = "drop")
  # a taxon that is in Holway (under any name) is known -> verified
  if (all(c("verified", "in_holway") %in% names(merged)))
    merged$verified <- as.logical(merged$verified) | as.logical(merged$in_holway)

  bind_rows(merged, na_rows) |> select(any_of(names(df)))
}

merge_holway_resolved <- function(deduped, holway_resolved, holway_decision_map = NULL) {
  out <- deduped
  ref_ok <- !is.null(holway_resolved) && nrow(holway_resolved) > 0 &&
    all(c("taxon_id", "genus", "species", "scientific_name") %in% names(holway_resolved))

  if (ref_ok) {
    hr <- holway_resolved |> filter(!is.na(taxon_id))
    has_rank <- "rank" %in% names(hr)
    sp_map <- hr |>
      filter(!is.na(genus), genus != "", !is.na(species), species != "",
             if (has_rank) rank == "species" else TRUE) |>
      transmute(.mkey = paste(tolower(str_trim(genus)), tolower(.clean_epithet(species))),
                hr_sp_id  = as.integer(taxon_id),
                hr_sp_sci = scientific_name) |>
      distinct(.mkey, .keep_all = TRUE)
    g_map <- hr |>
      filter(!is.na(genus), genus != "", (is.na(species) | species == ""),
             if (has_rank) rank == "genus" else TRUE) |>
      transmute(.gkey = tolower(str_trim(genus)),
                hr_g_id  = as.integer(taxon_id),
                hr_g_sci = scientific_name) |>
      distinct(.gkey, .keep_all = TRUE)
    out <- out |>
      mutate(
        .mkey = ifelse(rank == "species" & !is.na(genus) & !is.na(species) & species != "",
                       tolower(paste(str_trim(genus), .clean_epithet(species))), NA_character_),
        .gkey = ifelse(rank == "genus" & !is.na(genus) & genus != "",
                       tolower(str_trim(genus)), NA_character_)
      ) |>
      left_join(sp_map, by = ".mkey") |>
      left_join(g_map,  by = ".gkey") |>
      mutate(taxon_id        = dplyr::coalesce(taxon_id, hr_sp_id, hr_g_id),
             scientific_name = dplyr::coalesce(scientific_name, hr_sp_sci, hr_g_sci)) |>
      select(-.mkey, -.gkey, -hr_sp_id, -hr_sp_sci, -hr_g_id, -hr_g_sci)
  }

  # Decision-map fill: Holway rows RENAMED during resolution (the sheet's
  # original name resolved to a taxon whose iNat name differs) get their
  # taxon_id from the recorded decision, keyed on the ORIGINAL genus+epithet(s).
  if (!is.null(holway_decision_map) && nrow(holway_decision_map) > 0) {
    out <- out |>
      mutate(.okey = ifelse(
        !is.na(genus) & genus != "" & !is.na(species) & species != "",
        str_squish(tolower(paste(str_trim(genus), .clean_epithet(species),
          ifelse(!is.na(subspecies) & subspecies != "", .clean_epithet(subspecies), "")))),
        NA_character_)) |>
      left_join(holway_decision_map, by = c(".okey" = ".dkey")) |>
      mutate(taxon_id = dplyr::coalesce(taxon_id, dm_id)) |>
      select(-.okey, -dm_id)
  }

  # Merge any rows that now share a taxon_id (the renamed-Holway <-> iNat twins).
  out <- reconcile_lookup_dupes(out)

  # Carry itis_valid from the reference onto matching Holway rows so old/invalid
  # names (itis_valid = FALSE) can be filtered without losing the record.
  if (!is.null(holway_resolved) && "itis_valid" %in% names(holway_resolved)) {
    iv <- holway_resolved |>
      transmute(.ikey = str_squish(tolower(paste(
                  ifelse(is.na(genus), "", genus),
                  ifelse(is.na(species), "", species),
                  ifelse(is.na(subspecies), "", subspecies)))),
                itis_valid = as.logical(itis_valid)) |>
      filter(.ikey != "") |>
      distinct(.ikey, .keep_all = TRUE)
    out <- out |>
      mutate(.ikey = str_squish(tolower(paste(
                ifelse(is.na(genus), "", genus),
                ifelse(is.na(species), "", species),
                ifelse(is.na(subspecies), "", subspecies))))) |>
      left_join(iv, by = ".ikey") |>
      select(-.ikey)
  }
  out
}

build_bee_taxonomy_lookup <- function(holway_df, checklist_sd_county, bees,
                                      verified_ids = integer(0),
                                      specimen_species = NULL,
                                      holway_resolved = NULL,
                                      holway_decision_map = NULL) {

  # Holway packs species + subspecies into species_raw ("cactorum basalis").
  # Split it: first token = species epithet, remainder = subspecies epithet.
  .hsplit <- split_holway_species(holway_df$species_raw)
  holway_taxonomy <- holway_df |>
    mutate(species = .hsplit$species, subspecies_h = .hsplit$subspecies)

  # --- GENUS ROWS (Holway) ---
  genus_rows <- holway_taxonomy |>
    filter(!is.na(genus), genus != "") |>
    distinct(family, subfamily, tribe, genus) |>
    arrange(genus, family, subfamily, tribe) |>
    group_by(genus) |> slice(1) |> ungroup() |>
    .higher_rank_constants() |>
    mutate(subgenus = NA_character_, complex = NA_character_,
           species = NA_character_, subspecies = NA_character_, rank = "genus")

  # --- SPECIES + SUBSPECIES ROWS (Holway) --- strip subgenus parens to match
  # iNat notation. A row with a subspecies epithet becomes a rank="subspecies"
  # row; otherwise rank="species".
  species_rows <- holway_taxonomy |>
    filter(!is.na(genus), genus != "", !is.na(species), species != "") |>
    mutate(subgenus = str_remove(str_remove(subgenus, "^\\("), "\\)$")) |>
    distinct(family, subfamily, tribe, genus, subgenus, species, subspecies_h) |>
    .higher_rank_constants() |>
    mutate(complex = NA_character_,
           subspecies = ifelse(is.na(subspecies_h) | subspecies_h == "",
                               NA_character_, subspecies_h),
           rank = ifelse(is.na(subspecies), "species", "subspecies"),
           subgenus = ifelse(subgenus == "", NA_character_, subgenus)) |>
    select(-subspecies_h)

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

  # --- verified + source-membership columns ---
  sets <- holway_name_sets(holway_df)
  flagged <- flag_new_taxa(deduped, sets, verified_ids = verified_ids)
  deduped$verified <- !flagged$needs_verification
  deduped$in_holway <- is.na(flagged$new_at_rank)   # nothing new to Holway = known to Holway
  deduped$in_inat   <- !is.na(deduped$taxon_id)      # matched an iNat-observed taxon

  # in_cabr_specimens: TRUE if this taxon's genus+species (or genus, for a
  # genus-only row) appears in the cleaned CABR specimen records. All FALSE
  # until specimen_bee_clean.R has produced that file.
  gs <- character(0); gonly <- character(0)
  if (!is.null(specimen_species) && nrow(specimen_species) > 0) {
    has_sp <- !is.na(specimen_species$species) & specimen_species$species != ""
    gs    <- paste(tolower(specimen_species$genus[has_sp]), tolower(specimen_species$species[has_sp]))
    gonly <- tolower(specimen_species$genus[!has_sp])
  }
  has_species <- !is.na(deduped$species) & deduped$species != ""
  deduped$in_cabr_specimens <-
    (has_species  & paste(tolower(deduped$genus), tolower(deduped$species)) %in% gs) |
    (!has_species & tolower(deduped$genus) %in% gonly)

  # --- fill Holway taxon_id + scientific_name from the enriched reference
  # table (must run AFTER in_inat, so unobserved-but-resolved Holway taxa keep
  # in_inat = FALSE while gaining a taxon_id). ---
  deduped <- merge_holway_resolved(deduped, holway_resolved, holway_decision_map)

  # --- restore the original Holway qualifier (CF/MSN/sp. nov.) in the species
  # column for DISPLAY. All internal joins/dedup/verify above ran on the
  # stripped epithet, so matching is unaffected; here we swap the shown value
  # back to Holway's original notation via a genus+stripped-epithet map. ---
  # Restore only the leading qualifier (CF/MSN) onto the species epithet -- NOT
  # the subspecies, which now lives in its own column.
  holway_species_display <- holway_taxonomy |>
    filter(!is.na(genus), genus != "", !is.na(species), species != "") |>
    mutate(.qual = str_trim(str_extract(species_raw, "^(CF|MSN)\\s+") %||% NA_character_),
           species_display = ifelse(is.na(.qual), species, paste(.qual, species))) |>
    transmute(.dkey = paste(tolower(genus), tolower(species)),
              species_display = species_display) |>
    distinct(.dkey, .keep_all = TRUE)
  deduped <- deduped |>
    mutate(.dkey = ifelse(!is.na(species) & species != "",
                          paste(tolower(genus), tolower(species)), NA_character_)) |>
    left_join(holway_species_display, by = ".dkey") |>
    mutate(species = coalesce(species_display, species)) |>
    select(-.dkey, -species_display)

  # --- final column order: metadata first (common_name right after
  # scientific_name), then the taxonomic hierarchy ---
  ordered <- c("taxon_id", "scientific_name", "common_name",
               "rank", "verified", "holway_status", "itis_valid",
               "in_holway", "in_inat", "in_cabr_specimens",
               TAXONOMY_LEVELS)
  # complex_taxon_id is dropped from this human-facing lookup; it's still carried
  # in the SD County checklist that the specimen complex-matching reads.
  deduped |>
    select(any_of(ordered), everything(), -any_of("complex_taxon_id")) |>
    arrange(family, genus, rank, subgenus, complex, species, subspecies)
}
