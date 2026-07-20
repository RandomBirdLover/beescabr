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

  tax_cols <- c("taxon_id", "scientific_name", "common_name",
                "kingdom", "phylum", "class", "order", "superfamily",
                "family", "subfamily", "tribe", "subtribe",
                "genus", "subgenus", "complex", "complex_taxon_id", "species", "subspecies")
  kc <- "combine_key"
  key_of <- function(df) {
    g <- str_to_lower(trimws(as.character(df$genus)))
    s <- str_to_lower(trimws(as.character(df$species))); s[is.na(s)] <- ""
    ifelse(is.na(g) | g == "", NA_character_, paste(g, s, sep = "_"))
  }

  backbone <- bind_rows(lapply(sources, function(df) { df[[kc]] <- key_of(df); df })) |>
    filter(!is.na(.data[[kc]])) |>
    select(any_of(c(kc, tax_cols))) |>
    distinct(.data[[kc]], .keep_all = TRUE)

  for (nm in names(sources))
    backbone[[paste0("in_", nm)]] <- backbone[[kc]] %in% key_of(sources[[nm]])

  backbone[[kc]] <- NULL
  arrange(backbone, family, genus, species)
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
