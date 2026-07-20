# =============================================================
# reference/taxonomy_lookup_build.R
# beescabr pipeline -- taxonomy LOOKUP builder (sd_bee_taxonomy_lookup.csv)
# Renamed 2026-07-15 from native_bee_checklist.R.
#
# The old orchestrator (build_all_checklists) built the taxonomy lookup AND the
# Tier 1 / Tier 2 checklists in one pass. The checklist writes were split off to
# legacy_checklists.R (REMOVED to _to_delete/; was parked until the new per-source checklist architecture is
# built). This file now builds ONLY the lookup + its internal artifacts.
#
# Exposes build_taxonomy_lookup(con): from a populated observation cache builds
#   - sd_bee_taxonomy_lookup.csv   the taxonomy reference table (the deliverable)
#   - the internal complex map     bare genus/species/complex + complex_taxon_id;
#                                  specimen_bee_clean.R reads it to roll specimen
#                                  IDs up to iNat's complex-level observations
#
# It still BUILDS the SD County checklist in memory -- that checklist is the
# lookup's input, and the complex map is carved from it -- but it does NOT write
# any checklist file. Ingest is the CALLER's job (run_pipeline.R ingests once for
# the whole run). Running this file directly still works: it ingests, builds, done.
#
# Modules wired here:
#   config.R, pipelines/read_inat.R, reference/holway.R,
#   checklists/checklist_build.R, reference/taxonomy_reference.R,
#   (specimen_species_table / tier2_merge.R retired -- in_cabr_specimens pending, see below)
#
# Run standalone: Rscript scripts/reference/taxonomy_lookup_build.R
#   BEESCABR_SKIP_INGEST=1 reuses the cache without hitting the API.
# =============================================================

library(dplyr)
library(stringr)
library(sf)

local({
  need <- function(sym, file) if (!exists(sym)) source(file.path("scripts", file))
  need("PATHS",                    "config.R")
  need("write_fresh",              "utils/utils.R")
  need("decorate_complex",         "utils/utils.R")
  need("require_columns",          "utils/utils.R")
  need("store_connect",            "observations/engine/db/store_conn.R")
  need("count_observations",       "observations/engine/db/observations_store.R")
  need("taxon_cache_get",          "observations/engine/db/taxon_store.R")
  need("inat_request",             "observations/engine/api/inat_http.R")
  need("flatten_observation",      "observations/engine/api/inat_flatten.R")
  need("resolve_taxonomy",         "observations/engine/api/inat_cache.R")
  need("ingest_observations",      "observations/engine/pipelines/ingest_inat.R")
  need("read_observations_export", "observations/engine/pipelines/read_inat.R")
  need("load_holway",              "reference/holway.R")
  need("build_checklist",          "checklists/checklist_build.R")
  need("build_bee_taxonomy_lookup","reference/taxonomy_reference.R")
  need("load_verified_taxa",       "reference/verify.R")
})

# ------------------------------------------------------------
# build_taxonomy_lookup(con): build sd_bee_taxonomy_lookup.csv (+ the internal
# complex map) against a populated cache. Returns an invisible summary list.
# ------------------------------------------------------------
build_taxonomy_lookup <- function(con) {
  # Boundaries (reads shapefiles) loaded lazily so merely sourcing this file
  # to define the function does not require the spatial data on disk.
  if (!exists("sd_county_boundary")) source("scripts/spatial/spatial_utils.R")

  bees <- read_observations_export(con)

  # STEP 1: ranked-name columns -> bare names.
  bees <- bees |>
    rename(
      kingdom = taxon_kingdom_name, phylum = taxon_phylum_name,
      class = taxon_class_name, order = taxon_order_name,
      superfamily = taxon_superfamily_name, family = taxon_family_name,
      subfamily = taxon_subfamily_name, tribe = taxon_tribe_name,
      subtribe = taxon_subtribe_name,
      genus = taxon_genus_name, species = taxon_species_name,
      subspecies = taxon_subspecies_name
    )

  # STEP 2: Holway backfill of blank family/subfamily/tribe.
  holway_df <- load_holway(PATHS$holway_combined)
  bees <- backfill_taxonomy(bees, holway_genus_taxonomy(holway_df))

  # STEP 3: SD County spatial subset. SD County is the widest boundary (it
  # contains Point Loma, which contains CABR), so its checklist holds the most
  # taxa and is the natural base for the San Diego taxonomy lookup. Only this
  # tier is needed here -- PL / CABR tiers lived in legacy_checklists.R (now in _to_delete/).
  n_missing_coords <- sum(is.na(bees$latitude) | is.na(bees$longitude))
  if (n_missing_coords > 0)
    message(sprintf("NOTE: %d obs missing coords -- excluded.", n_missing_coords))

  bees_sf <- bees |>
    filter(!is.na(latitude), !is.na(longitude)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
    st_transform(PROJECT_CRS)

  message("\n--- Spatial subset (SD County) ---")
  bees_sd_county <- spatial_split(bees_sf, sd_county_boundary, "SD County")

  # STEP 4: SD County checklist IN MEMORY -- the lookup's input (never written).
  cl_sd <- build_checklist(bees_sd_county, "SD County")
  taxonomy_lookup <- taxonomy_lookup_from_bees(bees)
  cl_sd <- finalize_checklist(cl_sd, taxonomy_lookup)
  run_qc(cl_sd, "SD County")

  # Internal complex map (bare names + complex_taxon_id) from cl_sd, for the
  # specimen complex-match step. This is the ONLY place complex_taxon_id is
  # persisted -- the public checklists drop it. Written BEFORE decoration so it
  # keeps the bare complex + taxon_id that match_specimen_complex() needs.
  complex_map <- cl_sd |>
    filter(!is.na(complex), complex != "") |>
    distinct(genus, species, complex, complex_taxon_id)
  dir.create(dirname(PATHS$complex_map), recursive = TRUE, showWarnings = FALSE)
  write_fresh(complex_map, PATHS$complex_map, na = "")
  message("Wrote internal complex map (", nrow(complex_map), " species) -> ",
          basename(PATHS$complex_map))

  # CABR specimens -> the lookup's in_cabr_specimens column. PENDING: specimen_bee_clean.R
  # isn't built yet, and specimen_species_table() retired with tier2_merge.R, so
  # in_cabr_specimens stays blank/FALSE for now. TODO (when the specimen side is rebuilt):
  # restore specimen_species_table (into checklist_build.R), feed it here, and add the
  # in_cabr_specimen column to the CABR official + CABR specimen checklists.
  specimen_species <- NULL
  message("\nNOTE: in_cabr_specimens wiring pending -- left blank until specimen_bee_clean.R is built.")

  # STEP 5: sd_bee_taxonomy_lookup.csv (with source-membership columns).
  # The enriched Holway reference table (holway_sd_bee_reference_table.csv, built
  # by holway_reference_build.R earlier in run_pipeline.R) is the BASE of the
  # lookup -- it supplies iNat taxon_ids + scientific names for Holway species,
  # including ones never observed in SD County. The lookup NEVER reads the raw
  # Holway sheet for names.
  message("\n--- sd_bee_taxonomy_lookup ---")
  verified_ids <- load_verified_taxa(PATHS$verified_taxa)
  if (!file.exists(PATHS$holway_reference))
    stop("Holway reference table not found (", basename(PATHS$holway_reference), "). It is the ",
         "base of the taxonomy lookup -- build it first (run_pipeline.R step 1b, or ",
         "holway_reference_build.R).")
  holway_resolved <- readr::read_csv(PATHS$holway_reference, show_col_types = FALSE)
  message("Holway base from reference table: ", basename(PATHS$holway_reference),
          " (", sum(!is.na(holway_resolved$taxon_id)), " resolved taxa)")
  # Ancestry side-table (holway_taxon_ancestry.csv, written by holway_reference_build.R):
  # distinct (taxon_id, rank, name) for every ancestor of every resolved Holway taxon.
  # It's what lets each PARENT taxon get its OWN iNat id even when it was never
  # observed in SD County -- so no species row has to borrow a parent's id.
  ancestry_ids <- if (file.exists(PATHS$holway_ancestry))
    readr::read_csv(PATHS$holway_ancestry, show_col_types = FALSE) else NULL
  if (is.null(ancestry_ids))
    message("NOTE: no ancestry side-table (", basename(PATHS$holway_ancestry),
            ") -- parent ids fall back to observed taxa only; rebuild the ",
            "Holway reference to populate it.")
  bee_taxonomy_lookup <- build_bee_taxonomy_lookup(holway_resolved, cl_sd, bees,
                                                   verified_ids = verified_ids,
                                                   specimen_species = specimen_species,
                                                   ancestry_ids = ancestry_ids)
  write_fresh(decorate_complex(bee_taxonomy_lookup), PATHS$taxonomy_lookup, na = "")
  message("Wrote ", nrow(bee_taxonomy_lookup), " taxonomy rows (",
          sum(!bee_taxonomy_lookup$verified), " unverified).")

  invisible(list(lookup = nrow(bee_taxonomy_lookup)))
}

# ------------------------------------------------------------
# Standalone entrypoint (skipped when sourced by run_pipeline.R).
# ------------------------------------------------------------
if (!exists("BEESCABR_SOURCED_BY_RUNNER")) {
  main <- function() {
    con <- store_connect()
    on.exit(store_disconnect(con), add = TRUE)
    if (Sys.getenv("BEESCABR_SKIP_INGEST", "0") != "1") ingest_observations(con)
    else message("BEESCABR_SKIP_INGEST=1 -- using existing cache (", count_observations(con), " obs)")
    build_taxonomy_lookup(con)
  }
  main()
}
