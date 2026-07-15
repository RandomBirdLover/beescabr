# =============================================================
# checklists/legacy_checklists.R
# beescabr pipeline -- OLD Tier 1 + Tier 2 checklist writer (PARKED)
# Split out 2026-07-15 from native_bee_checklist.R (now taxonomy_lookup_build.R).
#
# This holds the pre-rewrite checklist outputs:
#   - 3 Tier 1 iNat checklists   (SD County / Point Loma / CABR)
#   - 3 Tier 2 merged checklists (SD County / Point Loma / CABR)
#   - the CABR specimen checklist
#
# It is intentionally NOT wired into run_pipeline.R. The checklist stage is being
# rebuilt into the new per-source architecture (cabr_inat / cabr_specimen /
# cabr_official / pl_raw_inat / sd_holway / sd_raw_inat / sd_holway_and_raw_inat),
# which runs LAST in the pipeline. This file is kept runnable by hand so the old
# outputs can still be regenerated until the replacement lands.
#
# The taxonomy lookup + the internal complex map now live in
# taxonomy_lookup_build.R. This file rebuilds the SD / PL / CABR tiers from the
# cache independently and does NOT touch the lookup or the complex map.
#
# Run standalone: Rscript scripts/checklists/legacy_checklists.R
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
  need("build_tier2_checklist",    "checklists/tier2_merge.R")  # + specimen_species_table / build_specimen_checklist
})

# ------------------------------------------------------------
# build_legacy_checklists(con): the old Tier 1 + Tier 2 + specimen checklist
# build against a populated cache. Returns an invisible summary list of counts.
# Self-contained: rebuilds the tiers from the cache; does not depend on
# taxonomy_lookup_build.R.
# ------------------------------------------------------------
build_legacy_checklists <- function(con) {
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

  # Public checklists: add the "(Complex)" prefix (a complex name is otherwise
  # identical to a species scientific name) and drop the internal
  # complex_taxon_id column. decorate_complex is idempotent + display-only.
  strip_public <- function(cl) decorate_complex(cl) |> select(-any_of("complex_taxon_id"))

  write_fresh(strip_public(cl_sd),   PATHS$checklist_sd_county_inat)
  write_fresh(strip_public(cl_pl),   PATHS$checklist_point_loma_inat)
  write_fresh(strip_public(cl_cabr), PATHS$checklist_cabr_inat)
  message("\nTier 1 checklists written.")

  # CABR specimens (optional) -> Tier 2 specimen evidence + specimen checklist.
  # If the cleaned workbook isn't there yet, specimen columns are blank/FALSE.
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
    message("\nNOTE: specimen clean file not found -- Tier 2 specimen evidence blank.",
            " Run scripts/clean/specimen_bee_clean.R when ready.")
  }

  if (!is.null(cabr_specimens))
    write_fresh(build_specimen_checklist(cabr_specimens), PATHS$checklist_cabr_specimen, na = "")

  # STEP 5: Tier 2 merged checklists (+ CABR specimen evidence & Holway check).
  holway_keys <- holway_match_keys(holway_df)

  message("\n--- Tier 2 build ---")
  cl_cabr_v2 <- build_tier2_checklist(cl_cabr, specimen_species, holway_keys, FALSE, "CABR")
  cl_pl_v2   <- build_tier2_checklist(cl_pl,   NULL, holway_keys, FALSE, "Point Loma")
  cl_sd_v2   <- build_tier2_checklist(cl_sd,   NULL, holway_keys, TRUE,  "SD County")

  write_fresh(cl_cabr_v2, PATHS$checklist_cabr_v2, na = "")
  write_fresh(cl_pl_v2,   PATHS$checklist_point_loma_v2, na = "")
  write_fresh(cl_sd_v2,   PATHS$checklist_sd_county_v2, na = "")

  message("\nAll legacy checklists written.")
  invisible(list(
    tier1 = c(sd = nrow(cl_sd), pl = nrow(cl_pl), cabr = nrow(cl_cabr)),
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
    build_legacy_checklists(con)
  }
  main()
}
