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

# ------------------------------------------------------------
# reference_name_sets(): the Holway name-membership sets (genus / subgenus /
# species / subspecies), built from the CLEANED reference table -- NOT the raw
# sheet. Used by flag_new_taxa() to decide in_holway / verified. Because the
# reference already holds clean epithets, a lookup row matches iff Holway truly
# knows that name. PURE.
# ------------------------------------------------------------
reference_name_sets <- function(ref) {
  g  <- tolower(trimws(ref$genus))
  sg <- tolower(str_remove_all(ifelse(is.na(ref$subgenus), "", ref$subgenus), "[()]"))
  sp <- tolower(.clean_epithet(ifelse(is.na(ref$species), "", ref$species)))
  ss <- tolower(.clean_epithet(ifelse(is.na(ref$subspecies), "", ref$subspecies)))
  has_sp <- !is.na(sp) & sp != ""
  has_ss <- !is.na(ss) & ss != ""
  list(
    genus      = unique(g[!is.na(g) & g != ""]),
    subgenus   = unique(sg[!is.na(sg) & sg != ""]),
    species    = unique(paste(g, sp)[has_sp]),
    subspecies = unique(paste(g, sp, ss)[has_ss])
  )
}

# ============================================================================
# build_bee_taxonomy_lookup(): the reference table is the START of the lookup.
#
# The Holway BASE is taken ENTIRELY from the cleaned reference table
# (holway_resolved); the raw Holway sheet is never read for names. iNat-observed
# taxa and CABR specimen evidence are then layered ON TOP. This is the rewrite
# that (a) stops the raw slash leak -- "Bombus sonorus" is what the reference
# holds, so "pensylvanicus" can never appear -- and (b) carries the tentative/
# unpublished names properly: clean epithet in `species`, marker in `qualifier`,
# and a real taxon_id where one exists (e.g. Andrena annectens 573509).
#
# merge_holway_resolved()/build_holway_decision_map() above are retained for
# their unit tests but are no longer part of this path: with the reference as the
# base, taxon_ids are already present and there is no stale raw row to reconcile.
# ============================================================================
build_bee_taxonomy_lookup <- function(holway_resolved, checklist_sd_county, bees,
                                      verified_ids = integer(0),
                                      specimen_species = NULL) {

  if (is.null(holway_resolved) || nrow(holway_resolved) == 0)
    stop("build_bee_taxonomy_lookup(): the cleaned Holway reference table is REQUIRED ",
         "as the Holway base. Build it first with holway_reference_build.R -- the lookup ",
         "never reads the raw Holway sheet for names.")

  # NB: base ifelse() returns a *logical* vector on length-0 input, which would
  # poison an empty column's type and break bind_rows -> guard length 0 explicitly.
  strip_par <- function(x) {
    x <- as.character(x); if (length(x) == 0) return(character(0))
    y <- str_remove_all(ifelse(is.na(x), "", x), "[()]")
    ifelse(y == "", NA_character_, y)
  }
  blank_na  <- function(x) {
    x <- as.character(x); if (length(x) == 0) return(character(0))
    ifelse(is.na(x) | x == "", NA_character_, x)
  }

  # ---------------------------------------------------------------
  # 1. HOLWAY BASE -- reshaped from the reference table into lookup columns.
  # ---------------------------------------------------------------
  need <- c("taxon_id","scientific_name","common_name","rank","source_sheet",
            "qualifier","itis_valid","family","subfamily","tribe","subtribe",
            "genus","subgenus","complex","species","subspecies")
  hr <- holway_resolved
  for (nm in need) if (!nm %in% names(hr)) hr[[nm]] <- NA
  holway_ref <- hr |>
    transmute(
      taxon_id        = suppressWarnings(as.integer(taxon_id)),
      scientific_name = blank_na(scientific_name),
      common_name     = blank_na(common_name),
      rank            = as.character(rank),
      holway_status   = blank_na(source_sheet),
      qualifier       = blank_na(qualifier),
      itis_valid      = as.logical(itis_valid),
      family = blank_na(family), subfamily = blank_na(subfamily),
      tribe  = blank_na(tribe),  subtribe  = blank_na(subtribe),
      genus  = blank_na(genus),  subgenus  = strip_par(subgenus),
      complex = blank_na(complex), complex_taxon_id = NA_integer_,
      species = blank_na(species), subspecies = blank_na(subspecies)
    ) |>
    filter(!is.na(genus))

  # Named Holway taxa from the reference: species / subspecies / complex (and any
  # subgenus) rows keep their reference taxon_id + name. EVERY such reference row
  # must reach the lookup -- incl. a Described species that resolved to a complex
  # (osmioides) or an unresolved subspecies -- so nothing Holway is dropped.
  # genus-level rows are synthesized below (one per genus) and excluded here.
  holway_named <- holway_ref |>
    filter(rank %in% c("species", "subspecies", "complex", "subgenus")) |>
    .higher_rank_constants()

  # reference genus-level ids (genus-only Holway entries) to fold into the synth
  ref_genus_id <- holway_ref |>
    filter(rank == "genus", !is.na(taxon_id)) |>
    distinct(genus, .keep_all = TRUE) |>
    transmute(genus, rg_id = as.integer(taxon_id), rg_sci = scientific_name)

  # genus-level family/subfamily/tribe (Holway wins over iNat when backfilling)
  holway_genus_map <- holway_ref |>
    distinct(genus, family, subfamily, tribe) |>
    arrange(genus, family, subfamily, tribe) |>
    group_by(genus) |> slice(1) |> ungroup() |>
    rename(h_family = family, h_subfamily = subfamily, h_tribe = tribe)
  with_holway_fallback <- function(df) {
    df |>
      left_join(holway_genus_map, by = "genus") |>
      mutate(family = coalesce(h_family, family),
             subfamily = coalesce(h_subfamily, subfamily),
             tribe = coalesce(h_tribe, tribe)) |>
      select(-h_family, -h_subfamily, -h_tribe)
  }

  # ---------------------------------------------------------------
  # 2. iNat OBSERVED taxa (SD County checklist) -- genus / subgenus / complex /
  #    species / subspecies rows, incl. taxa Holway never lists.
  # ---------------------------------------------------------------
  cl <- checklist_sd_county
  # An all-NA column can arrive typed <logical>, which clashes with <character>
  # in bind_rows -> coerce the name/text columns on both inputs up front.
  .chr <- c("genus","subgenus","complex","species","subspecies",
            "family","subfamily","tribe","subtribe","scientific_name","common_name")
  for (c in intersect(.chr, names(cl)))   cl[[c]]   <- as.character(cl[[c]])
  for (c in intersect(.chr, names(bees))) bees[[c]] <- as.character(bees[[c]])
  mk_rows <- function(df) df |> .higher_rank_constants() |>
    mutate(taxon_id = NA_integer_, scientific_name = NA_character_,
           common_name = NA_character_, complex_taxon_id = NA_integer_)

  genera_all <- sort(unique(c(holway_ref$genus,
                              cl$genus[!is.na(cl$genus) & cl$genus != ""])))
  genus_rows <- tibble(genus = genera_all,
                       family = NA_character_, subfamily = NA_character_, tribe = NA_character_) |>
    with_holway_fallback() |> mk_rows() |>
    mutate(subgenus = NA_character_, complex = NA_character_,
           species = NA_character_, subspecies = NA_character_, rank = "genus") |>
    left_join(ref_genus_id, by = "genus") |>
    mutate(taxon_id = coalesce(taxon_id, rg_id),
           scientific_name = coalesce(scientific_name, rg_sci)) |>
    select(-rg_id, -rg_sci)

  subgenus_rows <- cl |> filter(!is.na(subgenus), subgenus != "") |>
    distinct(genus, subgenus, family, subfamily, tribe) |>
    mutate(subgenus = strip_par(subgenus)) |>
    with_holway_fallback() |> mk_rows() |>
    mutate(complex = NA_character_, species = NA_character_,
           subspecies = NA_character_, rank = "subgenus")

  complex_rows <- cl |> filter(!is.na(complex), complex != "") |>
    distinct(genus, subgenus, complex, family, subfamily, tribe) |>
    mutate(subgenus = strip_par(subgenus)) |>
    with_holway_fallback() |> mk_rows() |>
    mutate(species = NA_character_, subspecies = NA_character_, rank = "complex")

  inat_species_rows <- cl |> filter(!is.na(species), species != "") |>
    distinct(genus, subgenus, species, family, subfamily, tribe) |>
    mutate(subgenus = strip_par(subgenus)) |>
    with_holway_fallback() |> mk_rows() |>
    mutate(complex = NA_character_, subspecies = NA_character_, rank = "species")

  subspecies_rows <- cl |> filter(!is.na(subspecies), subspecies != "") |>
    distinct(genus, subgenus, species, subspecies, family, subfamily, tribe) |>
    mutate(subgenus = strip_par(subgenus)) |>
    with_holway_fallback() |> mk_rows() |>
    mutate(complex = NA_character_, rank = "subspecies")

  # observed-taxon reference for attaching ids/names to the rank rows above
  inat_ref <- cl |>
    filter(!is.na(taxon_id)) |>
    transmute(genus = blank_na(genus), subgenus = strip_par(subgenus),
              complex = blank_na(complex), species = blank_na(species),
              subspecies = blank_na(subspecies),
              i_id = as.integer(taxon_id), i_sci = scientific_name,
              i_common = common_name, i_cplx = complex_taxon_id)

  # higher-group rows (genus/subgenus/complex) attach on the 5 name keys
  higher_group <- bind_rows(genus_rows, subgenus_rows, complex_rows) |>
    left_join(inat_ref, by = c("genus","subgenus","complex","species","subspecies"),
              relationship = "many-to-many") |>
    mutate(taxon_id = coalesce(taxon_id, i_id),
           scientific_name = coalesce(scientific_name, i_sci),
           common_name = coalesce(common_name, i_common),
           complex_taxon_id = coalesce(complex_taxon_id, i_cplx)) |>
    select(-i_id, -i_sci, -i_common, -i_cplx) |> distinct()

  # species-group rows attach on 3 keys (genus+species+subspecies); Holway has no
  # complex + subgenus can differ, so those are not part of the species key.
  sp_ref <- inat_ref |>
    group_by(genus, species, subspecies) |>
    summarise(i_id = dplyr::first(i_id), i_sci = dplyr::first(i_sci),
              i_common = dplyr::first(i_common), i_cplx = dplyr::first(i_cplx),
              i_complex = dplyr::first(complex), .groups = "drop")
  species_group <- bind_rows(inat_species_rows, subspecies_rows) |>
    left_join(sp_ref, by = c("genus","species","subspecies"),
              relationship = "many-to-many") |>
    mutate(taxon_id = coalesce(taxon_id, i_id),
           scientific_name = coalesce(scientific_name, i_sci),
           common_name = coalesce(common_name, i_common),
           complex = coalesce(complex, i_complex),
           complex_taxon_id = coalesce(complex_taxon_id, i_cplx)) |>
    select(-i_id, -i_sci, -i_common, -i_cplx, -i_complex) |> distinct()

  # ---------------------------------------------------------------
  # 3. HIGHER-RANK rows (family/subfamily/tribe) from the raw observations export.
  # ---------------------------------------------------------------
  higher_rank_rows <- bees |>
    filter(is.na(genus) | genus == "") |>
    select(any_of(c("taxon_id","scientific_name","common_name","family","subfamily","tribe"))) |>
    distinct(taxon_id, .keep_all = TRUE) |>
    filter(!is.na(taxon_id)) |>
    mutate(rank = case_when(!is.na(tribe)     & tribe     != "" ~ "tribe",
                            !is.na(subfamily) & subfamily != "" ~ "subfamily",
                            !is.na(family)    & family    != "" ~ "family",
                            TRUE ~ "epifamily"),
           kingdom = BEE_KINGDOM, phylum = BEE_PHYLUM, class = BEE_CLASS,
           order = BEE_ORDER, superfamily = BEE_SUPERFAMILY,
           subtribe = NA_character_, genus = NA_character_, subgenus = NA_character_,
           complex = NA_character_, complex_taxon_id = NA_integer_,
           species = NA_character_, subspecies = NA_character_)

  # ---------------------------------------------------------------
  # 4. COMBINE (base carries holway_status/qualifier/itis_valid; others blank) +
  #    flags + dedupe.
  # ---------------------------------------------------------------
  add_meta <- function(df) {
    if (!"holway_status" %in% names(df)) df$holway_status <- NA_character_
    if (!"qualifier"     %in% names(df)) df$qualifier     <- NA_character_
    if (!"itis_valid"    %in% names(df)) df$itis_valid    <- NA
    df
  }
  keep <- c(TAXONOMY_COLUMN_ORDER, "holway_status", "qualifier", "itis_valid", "subtribe")
  combined <- bind_rows(
    holway_named     |> add_meta() |> select(any_of(keep)),
    higher_group     |> add_meta() |> select(any_of(keep)),
    species_group    |> add_meta() |> select(any_of(keep)),
    higher_rank_rows |> add_meta() |> select(any_of(keep))
  )

  # subtribe from the observations by taxon_id (Holway/iNat rank rows carry none)
  if ("subtribe" %in% names(bees)) {
    st <- bees |> filter(!is.na(taxon_id)) |> distinct(taxon_id, obs_subtribe = subtribe)
    combined <- combined |> left_join(st, by = "taxon_id") |>
      mutate(subtribe = coalesce(subtribe, obs_subtribe)) |> select(-obs_subtribe)
  }
  if (!"subtribe" %in% names(combined)) combined$subtribe <- NA_character_

  # in_inat: observed on iNat -- NOT merely "has a taxon_id". A resolved-but-
  # unobserved Holway taxon (Andrena annectens, 573509, 0 obs) stays in_inat=FALSE.
  observed_ids <- unique(c(cl$taxon_id[!is.na(cl$taxon_id)],
                           bees$taxon_id[!is.na(bees$taxon_id)]))
  combined$in_inat <- !is.na(combined$taxon_id) & combined$taxon_id %in% observed_ids

  # dedupe NA-taxon rows on their name identity; id rows collapse in reconcile
  na_rows <- combined |> filter(is.na(taxon_id)) |>
    distinct(rank, genus, subgenus, complex, species, subspecies, .keep_all = TRUE)
  deduped <- bind_rows(combined |> filter(!is.na(taxon_id)), na_rows)

  # verified + in_holway from Holway NAME membership (reference-derived)
  sets    <- reference_name_sets(holway_ref)
  flagged <- flag_new_taxa(deduped, sets, verified_ids = verified_ids)
  deduped$verified  <- !flagged$needs_verification
  # in_holway = TRUE means Holway's checklist lists THIS taxon by its own name at
  # its own rank. NOTE FOR FUTURE READERS: a rank="complex" row shows in_holway=FALSE
  # NOT because the complex is missing from Holway, but because Holway lists species
  # (never complexes) -- the complex has no row of its own here. Its member species
  # that ARE in Holway are still flagged in_holway=TRUE on their own species rows.
  # This literal "does it have its own Holway row" definition is intentional: it's
  # what lets the reference + taxonomy tables build cleanly. Don't read a complex's
  # FALSE as "the bees in that complex aren't in Holway."
  deduped$in_holway <- is.na(flagged$new_at_rank)

  # in_cabr_specimens: genus+species (or genus, for a genus row) in the specimens
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

  # collapse rows that share a taxon_id (a Holway taxon and its iNat-observed twin)
  deduped <- reconcile_lookup_dupes(deduped)

  # ---------------------------------------------------------------
  # 5. Final column order + sort. qualifier sits right after holway_status.
  # ---------------------------------------------------------------
  ordered <- c("taxon_id","scientific_name","common_name","rank","verified",
               "holway_status","qualifier","itis_valid",
               "in_holway","in_inat","in_cabr_specimens", TAXONOMY_LEVELS)
  deduped |>
    select(any_of(ordered), everything(), -any_of("complex_taxon_id")) |>
    arrange(family, genus, rank, subgenus, complex, species, subspecies)
}
