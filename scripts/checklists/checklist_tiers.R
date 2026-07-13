# =============================================================
# checklists/checklist_tiers.R
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
