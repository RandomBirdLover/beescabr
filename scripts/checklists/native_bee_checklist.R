# =============================================================
# checklists/native_bee_checklist.R
# beescabr pipeline -- Tier 1 + Tier 2 checklist builder
# Rewritten: 2026-07-13 (API + DuckDB rewrite; monolith split into modules)
#
# Exposes build_all_checklists(con): builds all Tier 1 + Tier 2 checklists
# and bee_taxonomy_lookup.csv from an already-populated observation cache.
# Ingest is the CALLER's job (so run_pipeline.R can ingest once for the whole
# run). Running this file directly still works -- it ingests, builds, done.
#
# Modules wired here:
#   config.R, pipelines/read_inat.R, checklists/holway.R,
#   checklists/checklist_tiers.R, checklists/taxonomy_reference.R,
#   checklists/tier2_merge.R
#
# Run standalone: Rscript scripts/checklists/native_bee_checklist.R
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
  need("store_connect",            "engine/db/store_conn.R")
  need("count_observations",       "engine/db/observations_store.R")
  need("taxon_cache_get",          "engine/db/taxon_store.R")
  need("inat_request",             "engine/api/inat_http.R")
  need("flatten_observation",      "engine/api/inat_flatten.R")
  need("resolve_taxonomy",         "engine/api/inat_cache.R")
  need("ingest_observations",      "engine/pipelines/ingest_inat.R")
  need("read_observations_export", "engine/pipelines/read_inat.R")
  need("load_holway",              "checklists/holway.R")
  need("build_checklist",          "checklists/checklist_tiers.R")
  need("build_bee_taxonomy_lookup","checklists/taxonomy_reference.R")
  need("load_verified_taxa",       "clean/verify.R")
  need("build_tier2_checklist",    "checklists/tier2_merge.R")
})

# ------------------------------------------------------------
# build_all_checklists(con): the full Tier 1 + Tier 2 + lookup build against
# a populated cache. Returns an invisible summary list of row counts.
# ------------------------------------------------------------
build_all_checklists <- function(con) {
  # Boundaries (reads shapefiles) loaded lazily so merely sourcing this file
  # to define the function does not require the spatial data on disk.
  if (!exists("cabr_survey_box")) source("scripts/spatial/spatial_utils.R")

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

  # STEP 3: spatial split into the three tiers.
  n_missing_coords <- sum(is.na(bees$latitude) | is.na(bees$longitude))
  if (n_missing_coords > 0)
    message(sprintf("NOTE: %d obs missing coords -- excluded from all tiers.", n_missing_coords))

  bees_sf <- bees |>
    filter(!is.na(latitude), !is.na(longitude)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
    st_transform(PROJECT_CRS)

  message("\n--- Spatial split ---")
  bees_sd_county  <- spatial_split(bees_sf, sd_county_boundary,  "SD County")
  bees_point_loma <- spatial_split(bees_sf, point_loma_boundary, "Point Loma")
  bees_cabr       <- spatial_split(bees_sf, cabr_survey_box,     "CABR")

  # STEP 4: Tier 1 build + finalize.
  message("\n--- Tier 1 build ---")
  cl_sd   <- build_checklist(bees_sd_county,  "SD County")
  cl_pl   <- build_checklist(bees_point_loma, "Point Loma")
  cl_cabr <- build_checklist(bees_cabr,       "CABR")

  taxonomy_lookup <- taxonomy_lookup_from_bees(bees)
  cl_sd   <- finalize_checklist(cl_sd,   taxonomy_lookup)
  cl_pl   <- finalize_checklist(cl_pl,   taxonomy_lookup)
  cl_cabr <- finalize_checklist(cl_cabr, taxonomy_lookup)

  run_qc(cl_sd,   "SD County")
  run_qc(cl_pl,   "Point Loma")
  run_qc(cl_cabr, "CABR")

  # Internal complex map (bare names + complex_taxon_id) for the specimen
  # complex-match step. Written from the SD County checklist (broadest tier)
  # BEFORE decoration/stripping, so it keeps the bare complex + taxon_id that
  # match_specimen_complex() needs. This is the ONLY place complex_taxon_id
  # survives to disk -- the public checklists drop it (see strip_public below).
  complex_map <- cl_sd |>
    filter(!is.na(complex), complex != "") |>
    distinct(genus, species, complex, complex_taxon_id)
  dir.create(dirname(PATHS$complex_map), recursive = TRUE, showWarnings = FALSE)
  write_fresh(complex_map, PATHS$complex_map, na = "")
  message("Wrote internal complex map (", nrow(complex_map), " species) -> ",
          basename(PATHS$complex_map))

  # Public checklists: add the "(Complex)" prefix (a complex name is otherwise
  # identical to a species scientific name) and drop the internal
  # complex_taxon_id column. decorate_complex is idempotent + display-only.
  strip_public <- function(cl) decorate_complex(cl) |> select(-any_of("complex_taxon_id"))

  write_fresh(strip_public(cl_sd),   PATHS$checklist_sd_county_inat)
  write_fresh(strip_public(cl_pl),   PATHS$checklist_point_loma_inat)
  write_fresh(strip_public(cl_cabr), PATHS$checklist_cabr_inat)
  message("\nTier 1 checklists written.")

  # Read CABR specimens once (optional) -- used by BOTH the lookup's
  # in_cabr_specimens column and the Tier 2 specimen evidence below. Cleaning
  # the specimen workbook is a separate interactive step; if its output isn't
  # there yet, specimen columns are simply blank/FALSE.
  specimen_species <- NULL; cabr_specimens <- NULL
  if (file.exists(PATHS$specimen_clean)) {
    cabr_specimens <- readr::read_csv(PATHS$specimen_clean, show_col_types = FALSE)
    require_columns(cabr_specimens,
                    c("order", "family", "subfamily", "tribe", "genus", "subgenus",
                      "complex", "complex_taxon_id", "species", "subspecies"),
                    "cabr_specimens")
    specimen_species <- specimen_species_table(cabr_specimens)
    message("Specimen evidence: using ", basename(PATHS$specimen_clean))
  } else {
    message("\nNOTE: specimen clean file not found -- in_cabr_specimens/Tier 2 specimen",
            " evidence blank. Run scripts/clean/specimen_bee_clean.R when ready.")
  }

  # STEP 5: sd_bee_taxonomy_lookup.csv (with source-membership columns).
  # The enriched Holway reference table (holway_sd_bee_reference_table.csv,
  # built by holway_reference_build.R earlier in run_pipeline.R) supplies iNat
  # taxon_ids + scientific names for Holway species, including ones never
  # observed in SD County. If it isn't present yet, the lookup falls back to
  # Holway rows without taxon_ids (blank, as before).
  message("\n--- sd_bee_taxonomy_lookup ---")
  verified_ids <- load_verified_taxa(PATHS$verified_taxa)
  # The cleaned Holway reference table is the BASE of the lookup (never the raw
  # sheet). run_pipeline.R step 1b builds it before we get here; require it.
  if (!file.exists(PATHS$holway_reference))
    stop("Holway reference table not found (", basename(PATHS$holway_reference), "). It is the ",
         "base of the taxonomy lookup -- build it first (run_pipeline.R step 1b, or ",
         "holway_reference_build.R).")
  holway_resolved <- readr::read_csv(PATHS$holway_reference, show_col_types = FALSE)
  message("Holway base from reference table: ", basename(PATHS$holway_reference),
          " (", sum(!is.na(holway_resolved$taxon_id)), " resolved taxa)")
  bee_taxonomy_lookup <- build_bee_taxonomy_lookup(holway_resolved, cl_sd, bees,
                                                   verified_ids = verified_ids,
                                                   specimen_species = specimen_species)
  write_fresh(decorate_complex(bee_taxonomy_lookup), PATHS$taxonomy_lookup, na = "")
  message("Wrote ", nrow(bee_taxonomy_lookup), " taxonomy rows (",
          sum(!bee_taxonomy_lookup$verified), " unverified).")

  # STEP 6: Tier 2 merged checklists (+ CABR specimen evidence & Holway check).
  if (!is.null(cabr_specimens))
    write_fresh(build_specimen_checklist(cabr_specimens), PATHS$checklist_cabr_specimen, na = "")

  holway_keys <- holway_match_keys(holway_df)

  message("\n--- Tier 2 build ---")
  cl_cabr_v2 <- build_tier2_checklist(cl_cabr, specimen_species, holway_keys, FALSE, "CABR")
  cl_pl_v2   <- build_tier2_checklist(cl_pl,   NULL, holway_keys, FALSE, "Point Loma")
  cl_sd_v2   <- build_tier2_checklist(cl_sd,   NULL, holway_keys, TRUE,  "SD County")

  write_fresh(cl_cabr_v2, PATHS$checklist_cabr_v2, na = "")
  write_fresh(cl_pl_v2,   PATHS$checklist_point_loma_v2, na = "")
  write_fresh(cl_sd_v2,   PATHS$checklist_sd_county_v2, na = "")

  message("\nAll checklists written.")
  invisible(list(
    tier1 = c(sd = nrow(cl_sd), pl = nrow(cl_pl), cabr = nrow(cl_cabr)),
    lookup = nrow(bee_taxonomy_lookup),
    tier2 = c(sd = nrow(cl_sd_v2), pl = nrow(cl_pl_v2), cabr = nrow(cl_cabr_v2))
  ))
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
    build_all_checklists(con)
  }
  main()
}
