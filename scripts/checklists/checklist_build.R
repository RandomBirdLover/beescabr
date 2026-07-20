# =============================================================
# checklists/checklist_build.R
# beescabr pipeline -- Tier 1 (iNat-only) checklist construction
# Created: 2026-07-13 (extracted from native_bee_checklist.R monolith)
#
# The per-tier Tier 1 pipeline, one function per step so each is testable
# in isolation:
#   spatial_split()      -- clip observations to a boundary (needs sf)
#   build_checklist()    -- dedupe to unique taxa, enforce the genus rule
#   finalize_checklist() -- join subgenus/complex, parse epithets, order cols
#
# The GENUS-REQUIRED rule (2026-06-24): a row belongs in a checklist only if
# genus is populated. Identifications no further than family/tribe/etc. are
# dropped, not kept as placeholder rows. Preserved verbatim from the monolith.
#
# Depends on: dplyr, stringr; sf (spatial_split only).
# =============================================================

library(dplyr)
library(stringr)

# ------------------------------------------------------------
# spatial_split(): return the rows of an sf point layer that fall within a
# boundary polygon, dropped back to a plain data frame. Boundary and points
# must already share a CRS (the orchestrator transforms both to PROJECT_CRS).
# ------------------------------------------------------------
spatial_split <- function(points_sf, boundary, label = "", verbose = TRUE) {
  inside <- sf::st_within(points_sf, boundary, sparse = FALSE)[, 1]
  result <- points_sf |> filter(inside) |> sf::st_drop_geometry()
  if (verbose) message(sprintf("%-12s: %d of %d observations inside boundary",
                               label, nrow(result), nrow(points_sf)))
  result
}

# ------------------------------------------------------------
# build_checklist(): unique taxa (by taxon_id) with genus populated.
# Returns the deduped, genus-filtered, sorted checklist for one tier.
# ------------------------------------------------------------
build_checklist <- function(obs_df, label = "", verbose = TRUE) {
  before <- obs_df |>
    select(
      taxon_id, scientific_name, common_name,
      kingdom, phylum, class, order, superfamily,
      family, subfamily, tribe, any_of("subtribe"), genus, species, subspecies
    ) |>
    distinct(taxon_id, .keep_all = TRUE)

  result <- before |>
    filter(!is.na(genus), genus != "") |>
    arrange(family, genus, species)

  if (verbose) message(sprintf("%-12s: %d unique taxa, %d dropped (no genus), %d remain",
                               label, nrow(before), nrow(before) - nrow(result), nrow(result)))
  result
}

# ------------------------------------------------------------
# finalize_checklist(): join the subgenus/complex map (already resolved from
# the taxon cache during the export read) and parse species/subspecies down
# to the epithet, then fix the column order. word(...,-1) yields the last
# token of the binomial; na_if("") keeps genus-level rows as NA so downstream
# joins align.
# ------------------------------------------------------------
finalize_checklist <- function(checklist, taxonomy_lookup) {
  checklist |>
    left_join(taxonomy_lookup, by = "taxon_id") |>
    mutate(
      species    = na_if(word(species, -1), ""),
      subspecies = na_if(word(subspecies, -1), "")
    ) |>
    select(
      taxon_id, scientific_name, common_name,
      kingdom, phylum, class, order, superfamily,
      family, subfamily, tribe, any_of("subtribe"), genus, subgenus,
      complex, complex_taxon_id,
      species, subspecies
    )
}

# ------------------------------------------------------------
# taxonomy_lookup_from_bees(): the subgenus/complex/complex_taxon_id map that
# STEP 4 used to fetch from the API is now already present on the export
# frame (resolve_taxonomy filled it during the read). Derive it directly --
# no second API pass.
# ------------------------------------------------------------
taxonomy_lookup_from_bees <- function(bees) {
  bees |>
    distinct(taxon_id, subgenus, complex, complex_taxon_id)
}

# ------------------------------------------------------------
# run_qc(): per-tier quality-control summary (families, subgenus/complex
# counts, genera). Returns an invisible summary list; prints for the log.
# ------------------------------------------------------------
run_qc <- function(checklist, label) {
  families <- checklist |>
    filter(!is.na(family), family != "") |>
    distinct(family) |> arrange(family)
  distinct_complexes <- checklist |>
    filter(!is.na(complex)) |> distinct(complex, complex_taxon_id)

  message(sprintf("\n--- QC: %s ---", label))
  message("Families (", nrow(families), "): ", paste(families$family, collapse = ", "))
  message("Total unique taxa: ", nrow(checklist))
  message("With subgenus: ", sum(!is.na(checklist$subgenus)),
          " | with complex: ", sum(!is.na(checklist$complex)),
          " | distinct complexes: ", nrow(distinct_complexes))
  message("Genera represented: ", dplyr::n_distinct(checklist$genus))

  invisible(list(
    n_families = nrow(families),
    n_taxa = nrow(checklist),
    n_genera = dplyr::n_distinct(checklist$genus)
  ))
}

# ------------------------------------------------------------
# combine_checklists(): union taxa across named source checklists into ONE row per taxon,
# with a boolean in_<name> column per source flagging presence in that source. NULL sources
# are SKIPPED (no column emitted) so absent/pending data never shows a misleading all-FALSE
# flag -- e.g. in_specimen only appears where specimens actually exist (CABR), never on SD/PL.
# Taxonomy columns are taken from the first source that carries each taxon (list richer
# sources first). Key = lowercased genus + species epithet (genus-only taxa key on genus).
#   combine_checklists(list(inat = cl_inat, specimen = cl_specimen))  # -> in_inat, in_specimen
# ------------------------------------------------------------
combine_checklists <- function(sources) {
  sources <- sources[!vapply(sources, is.null, logical(1))]
  if (!length(sources)) return(NULL)
  keyed <- lapply(sources, function(df) { d <- cl_format(df); d$.k <- .cl_own_key(d); d[!is.na(d$.k), , drop = FALSE] })
  backbone <- do.call(rbind, keyed)
  backbone <- backbone[!duplicated(backbone$.k), , drop = FALSE]     # one row per taxon
  for (nm in names(sources)) backbone[[nm]] <- backbone$.k %in% keyed[[nm]]$.k   # boolean flag per source
  backbone$.k <- NULL
  ord <- order(backbone$family, backbone$genus, match(backbone$taxon_rank, CL_LINEAGE_RANKS),
               backbone$species, na.last = TRUE, method = "radix")
  backbone <- backbone[ord, c(CHECKLIST_COLS, names(sources)), drop = FALSE]
  rownames(backbone) <- NULL
  backbone
}

# ------------------------------------------------------------
# build_specimen_checklist(): unique taxa (genus required) straight from the specimen
# sheet -- the specimen-only checklist, column-parallel to the iNat file for direct diffing.
# (Moved here from the retired tier2_merge.R, 2026-07-20.)
# ------------------------------------------------------------
build_specimen_checklist <- function(specimens) {
  cols <- c("order", "family", "subfamily", "tribe", "genus", "subgenus",
            "complex", "complex_taxon_id", "species", "subspecies")
  specimens |>
    select(any_of(cols)) |>
    distinct() |>
    filter(!is.na(genus), genus != "") |>
    arrange(across(any_of(c("family", "genus", "subgenus", "species"))))
}

# ============================================================================
# NORMALIZED-TREE checklists (2026-07-20): each checklist carries PARENT taxa as their own rows
# (genus, subfamily, family, ...), exactly like the taxonomy lookup / Holway reference. taxon_id,
# taxon_rank, scientific_name/common_name, and taxonomy all come from the taxonomy lookup.
# ============================================================================
CHECKLIST_COLS <- c("taxon_id", "taxon_rank", "scientific_name", "common_name",
                    "order", "family", "subfamily", "tribe", "genus", "subgenus", "complex",
                    "species", "subspecies")
CL_LINEAGE_RANKS <- c("kingdom", "phylum", "subphylum", "class", "subclass", "order", "suborder",
                      "infraorder", "superfamily", "family", "epifamily", "subfamily", "tribe",
                      "subtribe", "genus", "subgenus", "complex", "species", "subspecies")

.cl_rankcol <- function(df) if ("rank" %in% names(df)) as.character(df$rank) else
  if ("taxon_rank" %in% names(df)) as.character(df$taxon_rank) else rep(NA_character_, nrow(df))

# fully-qualified (rank, name) identity key. Infra-generic ranks are qualified by their genus
# (species/subgenus/complex) or genus+species (subspecies), so "Andrena annectens" and
# "Brachynomada annectens" are DIFFERENT taxa -- a bare epithet would wrongly merge them. Subgenus
# names drop parens + keep the last word. Recycles a scalar rank over a vector name. NA/blank -> NA.
.cl_key <- function(rank, name, genus_ctx = NA, species_ctx = NA) {
  n <- max(length(rank), length(name), length(genus_ctx), length(species_ctx))
  rk <- tolower(trimws(rep_len(as.character(rank), n)))
  nm <- trimws(rep_len(as.character(name), n))
  is_sub <- !is.na(rk) & rk == "subgenus"
  nm[is_sub] <- sub(".*\\s", "", trimws(gsub("[()]", "", nm[is_sub])))
  nm <- tolower(nm)
  g <- tolower(trimws(rep_len(as.character(genus_ctx), n)))
  s <- tolower(trimws(rep_len(as.character(species_ctx), n)))
  qual <- dplyr::case_when(
    rk == "subspecies"                          ~ paste(g, s, sep = "|"),
    rk %in% c("species", "subgenus", "complex") ~ g,
    TRUE                                        ~ "")
  key <- ifelse(qual == "", paste(rk, nm, sep = "\t"), paste(rk, qual, nm, sep = "\t"))
  ifelse(is.na(rk) | rk == "" | is.na(nm) | nm == "", NA_character_, key)
}
# value a row carries at its OWN rank
.cl_name_at_own_rank <- function(df) {
  out <- rep(NA_character_, nrow(df)); rk <- .cl_rankcol(df)
  for (lv in CL_LINEAGE_RANKS) { if (!lv %in% names(df)) next
    hit <- !is.na(rk) & rk == lv; if (any(hit)) out[hit] <- as.character(df[[lv]][hit]) }
  out
}
.cl_own_key <- function(df) {
  g <- if ("genus"   %in% names(df)) df$genus   else rep(NA_character_, nrow(df))
  s <- if ("species" %in% names(df)) df$species else rep(NA_character_, nrow(df))
  .cl_key(.cl_rankcol(df), .cl_name_at_own_rank(df), g, s)
}

# every (rank, name) key present in the lineages of df's rows (genus-qualified per row)
checklist_lineage_keys <- function(df) {
  g <- if ("genus"   %in% names(df)) df$genus   else rep(NA_character_, nrow(df))
  s <- if ("species" %in% names(df)) df$species else rep(NA_character_, nrow(df))
  keys <- character(0)
  for (lv in CL_LINEAGE_RANKS) { if (!lv %in% names(df)) next
    k <- .cl_key(lv, df[[lv]], g, s); keys <- c(keys, k[!is.na(k)]) }
  unique(keys)
}
# format any lookup/present-shaped frame to CHECKLIST_COLS (taxon_rank <- rank)
cl_format <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!"taxon_rank" %in% names(df)) df$taxon_rank <- .cl_rankcol(df)
  for (cc in CHECKLIST_COLS) if (!cc %in% names(df)) df[[cc]] <- NA
  df[, CHECKLIST_COLS, drop = FALSE]
}

# lookup_subtree(): the normalized subtree (leaves + ALL ancestor rows) of present_df's taxa,
# taken from the lookup. A present leaf whose own (rank,name) isn't in the lookup (e.g. a
# specimen-only species) is kept with its own taxonomy + taxon_id (blank). One row per taxon.
lookup_subtree <- function(lookup, present_df, label = "", verbose = TRUE) {
  pres_keys <- checklist_lineage_keys(present_df)
  lk_own    <- .cl_own_key(lookup)
  in_lk     <- lookup[!is.na(lk_own) & lk_own %in% pres_keys, , drop = FALSE]
  pdf_own   <- .cl_own_key(present_df)
  keep_extra <- !is.na(pdf_own) & !(pdf_own %in% unique(stats::na.omit(lk_own)))
  extra <- present_df[keep_extra, , drop = FALSE]
  extra <- extra[!duplicated(.cl_own_key(extra)), , drop = FALSE]
  out <- rbind(cl_format(in_lk), cl_format(extra))
  out <- out[!duplicated(.cl_own_key(out)), , drop = FALSE]              # one row per taxon
  ord <- order(out$family, out$genus, match(out$taxon_rank, CL_LINEAGE_RANKS),
               out$species, na.last = TRUE, method = "radix")
  out <- out[ord, , drop = FALSE]; rownames(out) <- NULL
  if (verbose) message(sprintf("%-14s: %d taxa (%d from lookup tree, %d not-in-lookup leaves)",
                               label, nrow(out), nrow(in_lk), nrow(extra)))
  out
}
