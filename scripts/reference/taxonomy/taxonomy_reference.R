# =============================================================
# reference/taxonomy/taxonomy_reference.R
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

if (!exists("holway_name_sets")) source("scripts/reference/taxonomy/verify.R")
if (!exists("split_holway_species")) source("scripts/reference/taxonomy/holway.R")

BEE_KINGDOM     <- "Animalia"
BEE_PHYLUM      <- "Arthropoda"
BEE_CLASS       <- "Insecta"
BEE_ORDER       <- "Hymenoptera"
BEE_SUPERFAMILY <- "Apoidea"

# The 5 sub-ranks are invariant across all bees, but per the design we take the
# EXACT names from the captured iNat ancestry at build time; these are only the
# fallback used when the ancestry side-table is unavailable.
BEE_SUBRANK_FALLBACK <- c(
  subphylum = "Hexapoda", subclass = "Pterygota", suborder = "Apocrita",
  infraorder = "Aculeata", epifamily = "Anthophila"
)

TAXONOMY_COLUMN_ORDER <- c(
  "taxon_id", "scientific_name", "common_name",
  "kingdom", "phylum", "subphylum", "class", "subclass",
  "order", "suborder", "infraorder", "superfamily",
  "family", "epifamily", "subfamily", "tribe",
  "genus", "subgenus", "complex", "complex_taxon_id",
  "species", "subspecies", "rank"
)

.higher_rank_constants <- function(df) {
  df |> mutate(
    kingdom = BEE_KINGDOM, phylum = BEE_PHYLUM, class = BEE_CLASS,
    order = BEE_ORDER, superfamily = BEE_SUPERFAMILY
  )
}

# infer_higher_rank(): the OWN rank of an above-genus taxon, from the DEEPEST filled
# lineage column. subtribe is tested BEFORE tribe -- an observation identified to a
# subtribe (e.g. Halictina) also carries its parent tribe (Halictini), so a tribe-
# first test mislabels the subtribe taxon as rank "tribe". That put a SECOND, wrong-id
# row under tribe Halictini / Panurgini / Epeolini (Halictina 1597678, Perditina
# 572165, Epeolina 1671673), making those tribe ids ambiguous. Vectorized; NA/"" count
# as unfilled.
infer_higher_rank <- function(family, subfamily, tribe, subtribe = NA_character_) {
  nz <- function(x) !is.na(x) & as.character(x) != ""
  dplyr::case_when(
    nz(subtribe)  ~ "subtribe",
    nz(tribe)     ~ "tribe",
    nz(subfamily) ~ "subfamily",
    nz(family)    ~ "family",
    TRUE          ~ "epifamily"
  )
}

# ------------------------------------------------------------
# backfill_parent_taxonomy(lk): fill blank ANCESTOR-rank columns from kin already in the lookup.
# The lookup is a normalized tree (every parent taxon has its own, complete row), but a specimen-only
# species can arrive with intermediate ranks blank (e.g. subphylum Hexapoda, subclass Pterygota,
# suborder Apocrita, infraorder Aculeata, epifamily Anthophila, subtribe) while its OWN genus/family
# rows carry them. For each blank ancestor rank we copy the value shared by the taxon's genus, then
# family -- but ONLY when that value is unambiguous within the group. We deliberately stop at family:
# an order-wide pass would over-generalize a fine rank (e.g. copy one family's subtribe onto another
# family's species). Adds NO taxa; never overwrites a populated cell.
# Run as the LAST step of build_taxonomy_lookup(), after every taxon + id is merged.
# ------------------------------------------------------------
#' Fill blank ancestor ranks from other rows that share a lower rank
#'
#' This is what keeps a record identified only to `Halictini` from losing its
#' family and order.
#'
#' @param lk The lookup being built.
#' @return `lk` with blank ancestor ranks filled where a sibling row knew them.
backfill_parent_taxonomy <- function(lk) {
  anc <- intersect(c("kingdom","phylum","subphylum","class","subclass","order","suborder",
                     "infraorder","superfamily","family","epifamily","subfamily","tribe","subtribe"),
                   names(lk))
  if (!length(anc)) return(lk)
  blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))
  fill_by <- function(lk, key) {
    if (!key %in% names(lk)) return(lk)
    k <- trimws(as.character(lk[[key]])); keyed <- !blank(k)
    for (r in anc) {
      if (identical(r, key)) next
      col <- as.character(lk[[r]]); need <- blank(col) & keyed; have <- !blank(col) & keyed
      if (!any(need) || !any(have)) next
      donor <- tapply(col[have], k[have], function(v) { u <- unique(v); if (length(u) == 1L) u else NA_character_ })
      idx <- k[need]; hit <- idx %in% names(donor) & !is.na(donor[idx])
      col[which(need)[hit]] <- donor[idx[hit]]
      lk[[r]] <- col
    }
    lk
  }
  for (key in c("genus", "family")) lk <- fill_by(lk, key)
  lk
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
  flag_cols   <- intersect(c("verified", "in_holway", "in_inat"), names(df))
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

# ------------------------------------------------------------
# derive_bee_subranks(): PURE. The EXACT names for the 5 invariant sub-ranks
# (subphylum, subclass, suborder, infraorder, epifamily) taken from the captured
# iNat ancestry -- the most common name seen at each rank -- falling back to the
# known bee constants only when the ancestry side-table has no entry there.
# ------------------------------------------------------------
derive_bee_subranks <- function(ancestry_ids) {
  pick <- function(rk) {
    if (is.null(ancestry_ids) || !nrow(ancestry_ids) ||
        !all(c("rank", "name") %in% names(ancestry_ids)))
      return(unname(BEE_SUBRANK_FALLBACK[[rk]]))
    v <- ancestry_ids$name[!is.na(ancestry_ids$rank) & ancestry_ids$rank == rk &
                           !is.na(ancestry_ids$name) & ancestry_ids$name != ""]
    if (length(v)) names(sort(table(v), decreasing = TRUE))[1]
    else unname(BEE_SUBRANK_FALLBACK[[rk]])
  }
  c(subphylum = pick("subphylum"), subclass = pick("subclass"),
    suborder = pick("suborder"), infraorder = pick("infraorder"),
    epifamily = pick("epifamily"))
}

# ------------------------------------------------------------
# fill_parent_ids(): PURE. Give every blank-id PARENT row (genus / subgenus /
# complex / family / subfamily / tribe / subtribe / epifamily / superfamily) its
# real iNat taxon_id from the ancestry side-table, matched on rank + the name at
# that rank. Species/subspecies rows are LEFT ALONE -- an unresolved species keeps
# taxon_id = NA rather than borrowing a parent's id. subgenus matches on the last
# word (iNat may name a subgenus "Genus Subgenus"); everything else on full name.
# This is what fixes the blank-id parents: a parent never observed in SD County
# still gets its own id because a resolved child carried it in the ancestry.
# ------------------------------------------------------------
fill_parent_ids <- function(df, ancestry_ids) {
  if (is.null(ancestry_ids) || !nrow(ancestry_ids) ||
      !all(c("taxon_id", "rank", "name") %in% names(ancestry_ids))) return(df)
  last_word <- function(x) ifelse(is.na(x) | x == "", NA_character_, str_trim(word(x, -1)))
  amap <- ancestry_ids |>
    filter(!is.na(taxon_id), !is.na(rank), !is.na(name), name != "") |>
    mutate(.rk = tolower(rank),
           .nm = tolower(ifelse(rank == "subgenus", last_word(name), name))) |>
    group_by(.rk, .nm) |>
    summarise(.aid = as.integer(dplyr::first(taxon_id)), .groups = "drop")

  PARENT_RANKS <- c("genus", "subgenus", "complex", "family", "subfamily",
                    "tribe", "subtribe", "epifamily", "superfamily")
  name_at_rank <- dplyr::case_when(
    df$rank == "genus"       ~ df$genus,
    df$rank == "subgenus"    ~ df$subgenus,
    df$rank == "complex"     ~ df$complex,
    df$rank == "family"      ~ df$family,
    df$rank == "subfamily"   ~ df$subfamily,
    df$rank == "tribe"       ~ df$tribe,
    df$rank == "subtribe"    ~ df$subtribe,
    df$rank == "epifamily"   ~ df$epifamily,
    df$rank == "superfamily" ~ df$superfamily,
    TRUE ~ NA_character_)
  df$.rk <- ifelse(df$rank %in% PARENT_RANKS, tolower(df$rank), NA_character_)
  df$.nm <- tolower(ifelse(df$rank == "subgenus", last_word(name_at_rank), name_at_rank))
  df |>
    left_join(amap, by = c(".rk", ".nm")) |>
    mutate(taxon_id = dplyr::coalesce(taxon_id, .aid)) |>
    select(-.rk, -.nm, -.aid)
}

# ------------------------------------------------------------
# ancestry_ids_from_reference(): PURE. Build the (taxon_id, rank, name) id map that
# fill_parent_ids/derive_bee_subranks consume, from every id-bearing row of the
# reference table -- using the name at each row's own rank. Replaces the retired
# ancestry side-file now that the ancestor taxa live in the reference table itself.
# ------------------------------------------------------------
#' Pull the per-rank ancestor ids out of a resolved reference table
#'
#' @param ref A reference table carrying `taxon_id`, `rank`, `scientific_name`
#'   and the rank-name columns.
#' @return One row per taxon with an id for each ancestral rank, so a record
#'   identified only to tribe can still be rolled up.
ancestry_ids_from_reference <- function(ref) {
  LEVELS <- c("kingdom", "phylum", "subphylum", "class", "subclass", "order",
              "suborder", "infraorder", "superfamily", "family", "epifamily",
              "subfamily", "tribe", "subtribe", "genus", "subgenus", "complex",
              "species", "subspecies")
  for (nm in c("taxon_id", "rank", "scientific_name", LEVELS))
    if (!nm %in% names(ref))
      ref[[nm]] <- if (nm == "taxon_id") NA_integer_ else NA_character_
  name_at <- as.character(ref$scientific_name)
  for (lv in LEVELS) {
    hit <- !is.na(ref$rank) & ref$rank == lv
    name_at[hit] <- as.character(ref[[lv]][hit])
  }
  tibble(taxon_id = suppressWarnings(as.integer(ref$taxon_id)),
         rank = as.character(ref$rank), name = name_at) |>
    filter(!is.na(taxon_id), !is.na(rank), !is.na(name), name != "") |>
    distinct(taxon_id, .keep_all = TRUE)
}

# ============================================================================
# specimen_additions_to_lookup(): PURE. Merge NEW specimen-only taxa into the lookup WITHOUT
# fabricating parent rows. This is the deferred "lookup = Holway + iNat + specimen additions"
# step (see the TODO in taxonomy_lookup_build.R) -- GATED on the raw-specimen cleanup, so it is
# NOT wired into the live build yet; it lives here, tested, ready to call.
#
# `additions` are lookup-shaped taxon rows (a `rank` + the rank-name columns). For each:
#   * SKIP it if a row for that taxon already exists (matched by its name-at-own-rank);
#   * otherwise APPEND its own (leaf) row;
#   * its PARENT taxa are only ever LINKED to an EXISTING lookup row -- a missing parent is
#     NEVER created (by request). Missing parents are returned so you can decide later.
# Returns list(lookup = <lookup + appended leaves>, added = <appended rows>, missing_parents=<tibble>).
# ============================================================================
SPECIMEN_ADDITION_RANKS <- c("kingdom","phylum","subphylum","class","subclass","order",
                             "suborder","infraorder","superfamily","family","epifamily",
                             "subfamily","tribe","subtribe","genus","subgenus","complex",
                             "species","subspecies")

# .qual_key(): fully-qualified (rank, name) identity key. Infra-generic ranks are qualified by
# their genus (species/subgenus/complex) or genus+species (subspecies), so "Colletes phaceliae" and
# "Chelostoma phaceliae" are DIFFERENT taxa -- a bare epithet would wrongly collide them. Subgenus
# names drop parens + keep the last word ("(Neochelostoma)" == "Neochelostoma"). NA/blank -> NA.
.qual_key <- function(rank, name, genus_ctx = NA, species_ctx = NA) {
  rk <- tolower(trimws(as.character(rank)))
  nm <- trimws(as.character(name))
  is_sub <- !is.na(rk) & rk == "subgenus"
  nm[is_sub] <- sub(".*\\s", "", trimws(gsub("[()]", "", nm[is_sub])))   # strip parens, last word
  nm <- tolower(nm)
  g  <- tolower(trimws(as.character(genus_ctx)))
  s  <- tolower(trimws(as.character(species_ctx)))
  qual <- dplyr::case_when(
    rk == "subspecies"                          ~ paste(g, s, sep = "|"),
    rk %in% c("species", "subgenus", "complex") ~ g,
    TRUE                                        ~ "")
  key <- ifelse(qual == "", paste(rk, nm, sep = "\t"), paste(rk, qual, nm, sep = "\t"))
  ifelse(is.na(rk) | rk == "" | is.na(nm) | nm == "", NA_character_, key)
}

# .row_keys(): identity key for each row of a lookup/addition frame -- its name-at-own-rank,
# qualified by the row's own genus/species.
.row_keys <- function(df) .qual_key(df$rank, .name_at_own_rank(df),
                                    if ("genus" %in% names(df)) df$genus else NA,
                                    if ("species" %in% names(df)) df$species else NA)

# .name_at_own_rank(): the name a row carries AT ITS OWN rank (value of its rank-named column).
.name_at_own_rank <- function(df) {
  out <- rep(NA_character_, nrow(df))
  rk  <- as.character(df$rank)
  for (lv in SPECIMEN_ADDITION_RANKS) {
    if (!lv %in% names(df)) next
    hit <- !is.na(rk) & rk == lv
    if (any(hit)) out[hit] <- as.character(df[[lv]][hit])
  }
  out
}

#' Add taxa found only in the specimen records to the lookup
#'
#' @param lookup The lookup so far.
#' @param additions Specimen-only taxa, carrying at least a `rank`.
#' @return A list of `lookup` (with the additions merged), `added` (what was
#'   new) and `missing_parents` (additions whose parent rank is still unknown,
#'   so the run can report them instead of silently dropping the lineage).
specimen_additions_to_lookup <- function(lookup, additions) {
  empty_mp <- tibble(taxon = character(), rank = character(),
                     missing_parent_rank = character(), missing_parent_name = character())
  if (is.null(additions) || !nrow(additions) || !"rank" %in% names(additions))
    return(list(lookup = lookup, added = additions[0, , drop = FALSE], missing_parents = empty_mp))

  existing <- unique(na.omit(.row_keys(lookup)))

  add_name <- .name_at_own_rank(additions)
  add_key  <- .row_keys(additions)
  is_new   <- !is.na(add_key) & !(add_key %in% existing) & !duplicated(add_key)  # dedupe vs lookup + within batch
  added    <- additions[is_new, , drop = FALSE]
  added_nm <- add_name[is_new]

  # PARENT-EXISTENCE report -- higher ranks named on each added leaf that have NO lookup row.
  # We report them; we do NOT create them (the caller decides later).
  parents_above <- function(rk) {
    i <- match(rk, SPECIMEN_ADDITION_RANKS)
    if (is.na(i) || i <= 1L) character(0) else SPECIMEN_ADDITION_RANKS[seq_len(i - 1L)]
  }
  mp <- list()
  if (nrow(added)) for (r in seq_len(nrow(added))) {
    gctx <- if ("genus" %in% names(added)) added$genus[r] else NA
    sctx <- if ("species" %in% names(added)) added$species[r] else NA
    for (pr in parents_above(as.character(added$rank[r]))) {
      if (!pr %in% names(added)) next
      k <- .qual_key(pr, added[[pr]][r], gctx, sctx)
      if (is.na(k) || k %in% existing) next                    # blank or already present -> fine
      mp[[length(mp) + 1L]] <- tibble(taxon = added_nm[r], rank = as.character(added$rank[r]),
                                      missing_parent_rank = pr,
                                      missing_parent_name = as.character(added[[pr]][r]))
    }
  }
  missing_parents <- if (length(mp)) bind_rows(mp) else empty_mp
  list(lookup = bind_rows(lookup, added), added = added, missing_parents = missing_parents)
}

# load_specimen_additions(): read the curated specimen-only additions CSV (lookup-shaped rows:
# rank + rank-name columns, blank taxon_id). Missing/empty file -> 0-row tibble. taxon_id coerced
# to integer to match the lookup so the append doesn't clash types. Only rows with a rank are kept.
load_specimen_additions <- function(path) {
  if (is.null(path) || !file.exists(path)) return(tibble(rank = character()))
  a <- tryCatch(suppressWarnings(readr::read_csv(path, show_col_types = FALSE)), error = function(e) NULL)
  if (is.null(a) || !nrow(a) || !"rank" %in% names(a)) return(tibble(rank = character()))
  if ("taxon_id" %in% names(a)) a$taxon_id <- suppressWarnings(as.integer(a$taxon_id))
  a[!is.na(a$rank) & trimws(a$rank) != "", , drop = FALSE]
}

# apply_verified_ids(): PURE. Flip `verified` TRUE on any lookup row whose taxon_id is in the
# remembered verified set (verified_taxa.csv). build_bee_taxonomy_lookup applies the memory to its
# BASE rows, but rows appended later (the specimen-additions merge) miss it -- without this re-apply
# a verified addition rebuilds as unverified and the pass-2 prompt re-asks it EVERY run.
apply_verified_ids <- function(lookup, verified_ids) {
  if (is.null(lookup) || !nrow(lookup) || !"verified" %in% names(lookup)) return(lookup)
  tid <- suppressWarnings(as.integer(lookup$taxon_id))
  ids <- suppressWarnings(as.integer(verified_ids))
  lookup$verified <- as.logical(lookup$verified) | (!is.na(tid) & tid %in% ids)
  lookup
}

# drop_phantom_additions(): PURE. Keep a curated specimen-addition only if its taxon STILL has >= 1
# record in the current cleaned specimen OR iNat table (matched by taxon_id OR scientific_name -- the
# same rule the verify prompt's evidence check uses). A row whose evidence has vanished (e.g. the
# specimen was re-ID'd away) is a PHANTOM and is skipped at build time. This does NOT edit
# specimen_additions.csv, so the row silently reactivates if the bee is collected/photographed again.
# Missing/empty cleaned tables -> no evidence -> everything drops (never errors).
drop_phantom_additions <- function(additions, spec_clean = NULL, inat_clean = NULL) {
  if (is.null(additions) || !nrow(additions)) return(additions)
  low <- function(x) tolower(trimws(as.character(x)))
  tid <- suppressWarnings(as.integer(additions$taxon_id))
  sci <- if ("scientific_name" %in% names(additions)) low(additions$scientific_name) else rep("", nrow(additions))
  has_ev <- function(df) {                          # per-addition: >= 1 matching row in df?
    if (is.null(df) || !nrow(df)) return(rep(FALSE, nrow(additions)))
    d_tid <- if ("taxon_id" %in% names(df)) suppressWarnings(as.integer(df$taxon_id)) else integer(0)
    d_sci <- if ("scientific_name" %in% names(df)) low(df$scientific_name) else character(0)
    vapply(seq_len(nrow(additions)), function(i)
      (!is.na(tid[i]) && length(d_tid) && tid[i] %in% d_tid) ||
      (nzchar(sci[i]) && length(d_sci) && sci[i] %in% d_sci),
      logical(1))
  }
  additions[has_ev(spec_clean) | has_ev(inat_clean), , drop = FALSE]
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
#' Assemble the bee taxonomy lookup from all of its sources
#'
#' @param holway_resolved The cleaned Holway reference table. Required -- it is
#'   the base every other source is layered onto.
#' @param checklist_sd_county The San Diego County iNaturalist checklist.
#' @param bees Cleaned bee observations, for taxa the checklists missed.
#' @param verified_ids Taxon ids a human has confirmed.
#' @param ancestry_ids Per-rank ancestor ids, from
#'   `ancestry_ids_from_reference()`.
#' @return The lookup, one row per taxon. 17 rows legitimately have no
#'   `taxon_id`: iNaturalist has published no taxon for them.
build_bee_taxonomy_lookup <- function(holway_resolved, checklist_sd_county, bees,
                                      verified_ids = integer(0),
                                      ancestry_ids = NULL) {

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

  # A complex row has no complex-LEVEL observation to borrow a taxon_id from (people ID to species),
  # but the complex's iNat id rides on its SPECIES as complex_taxon_id. Map it by (genus, complex)
  # so complex rows aren't left blank -- e.g. (Complex) Colletes inaequalis -> 1438690.
  complex_id_map <- inat_ref |>
    filter(!is.na(i_cplx), !is.na(complex), complex != "") |>
    distinct(genus, complex, i_cplx) |>
    group_by(genus, complex) |>
    summarise(cx_id = dplyr::first(i_cplx), .groups = "drop")

  # higher-group rows (genus/subgenus/complex) attach on the 5 name keys
  higher_group <- bind_rows(genus_rows, subgenus_rows, complex_rows) |>
    left_join(inat_ref, by = c("genus","subgenus","complex","species","subspecies"),
              relationship = "many-to-many") |>
    left_join(complex_id_map, by = c("genus","complex")) |>
    mutate(taxon_id = coalesce(taxon_id, i_id),
           taxon_id = coalesce(taxon_id, dplyr::if_else(rank == "complex", cx_id, NA_integer_)),
           scientific_name = coalesce(scientific_name, i_sci),
           common_name = coalesce(common_name, i_common),
           complex_taxon_id = coalesce(complex_taxon_id, i_cplx)) |>
    select(-i_id, -i_sci, -i_common, -i_cplx, -cx_id) |> distinct()

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
    select(any_of(c("taxon_id","scientific_name","common_name",
                    "family","subfamily","tribe","subtribe"))) |>
    distinct(taxon_id, .keep_all = TRUE) |>
    filter(!is.na(taxon_id))
  # subtribe may be absent from the export; ensure the column exists so the rank
  # inference (and the kept subtribe value) are well-defined.
  if (!"subtribe" %in% names(higher_rank_rows)) higher_rank_rows$subtribe <- NA_character_
  higher_rank_rows <- higher_rank_rows |>
    mutate(rank = infer_higher_rank(family, subfamily, tribe, subtribe),
           kingdom = BEE_KINGDOM, phylum = BEE_PHYLUM, class = BEE_CLASS,
           order = BEE_ORDER, superfamily = BEE_SUPERFAMILY,
           genus = NA_character_, subgenus = NA_character_,
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

  # 5 sub-rank columns (subphylum/subclass/suborder/infraorder/epifamily): create
  # them if absent, then stamp the EXACT bee-wide value taken from the iNat
  # ancestry (coalesced so any per-row value already present is kept).
  bee_sub <- derive_bee_subranks(ancestry_ids)
  for (nm in names(bee_sub))
    if (!nm %in% names(combined)) combined[[nm]] <- NA_character_
  combined <- combined |>
    mutate(subphylum  = dplyr::coalesce(subphylum,  bee_sub[["subphylum"]]),
           subclass   = dplyr::coalesce(subclass,   bee_sub[["subclass"]]),
           suborder   = dplyr::coalesce(suborder,   bee_sub[["suborder"]]),
           infraorder = dplyr::coalesce(infraorder, bee_sub[["infraorder"]]),
           epifamily  = dplyr::coalesce(epifamily,  bee_sub[["epifamily"]]))

  # Give each PARENT taxon its own iNat id from the ancestry side-table (a species
  # never borrows one). Done before in_inat/dedupe so a newly-filled parent id can
  # collapse duplicates yet still read in_inat = FALSE when it was never observed.
  combined <- fill_parent_ids(combined, ancestry_ids)

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

  # NOTE: this lookup is intentionally HOLWAY + iNAT ONLY. CABR specimen evidence
  # (in_cabr_specimens) is deliberately NOT joined here -- the raw specimen names
  # aren't QC'd, and the specimen leg belongs downstream in the CABR checklists,
  # built from a CLEANED specimen record. Keep this reference table specimen-free.

  # collapse rows that share a taxon_id (a Holway taxon and its iNat-observed twin)
  deduped <- reconcile_lookup_dupes(deduped)

  # ---------------------------------------------------------------
  # 5. Final column order + sort. qualifier sits right after holway_status.
  # ---------------------------------------------------------------
  ordered <- c("taxon_id","scientific_name","common_name","rank","verified",
               "holway_status","qualifier","itis_valid",
               "in_holway","in_inat", TAXONOMY_LEVELS)
  deduped |>
    select(any_of(ordered), everything(), -any_of("complex_taxon_id")) |>
    arrange(family, genus, rank, subgenus, complex, species, subspecies)
}
