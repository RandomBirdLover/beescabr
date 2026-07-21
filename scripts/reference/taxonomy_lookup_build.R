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
#   (no specimen join -- this lookup is Holway + iNat only, by design)
#
# TODO (deferred -- GATED on the raw-specimen cleanup): fold specimen-only species in so
# the lookup becomes  Holway + iNat + specimen additions.
#   * WHY  : specimen_bee_clean.R nets real bees the Holway checklist AND the iNat SD obs
#            both miss (species no one photographed). They surface in
#            data/specimens/cleaned/cabr_specimen_bee_taxonomy_flags.csv as
#            "genus+species combo not in taxonomy lookup" -- those flags are the candidates.
#   * WHERE: a curated specimen_additions.csv, MERGED here at build time. Do NOT hand-edit
#            sd_bee_taxonomy_lookup.csv -- stage 5 rewrites it every run and would wipe
#            manual rows (same reason the Holway reference table can't hold them either).
#   * GATE : only AFTER the tidy_raw_specimens worklist is worked (add IDs, dedupe, drop
#            missing). Raw is still dirty, so some flagged names may be misspellings
#            (e.g. Lasioglossum 'daggetti' vs 'daggettii') -- verify each before adding.
#   * IDS  : each addition needs an iNat taxon_id resolved, or kept id-less like the Holway
#            "no iNat id yet" cases. Holway's reference table stays a pure copy of the checklist.
#   * MERGE: specimen_additions_to_lookup() (reference/taxonomy_reference.R) already does the merge
#            (tested): it appends leaf taxa and LINKS parents that exist but NEVER fabricates a
#            missing parent -- it returns those in $missing_parents to resolve. NOT wired in yet.
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
  need("resolve_missing_taxon_ids","reference/resolve_missing_ids.R")
  need("apply_manual_overrides",   "reference/manual_overrides.R")
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

  # NOTE: this lookup is intentionally HOLWAY + iNAT ONLY -- no specimen join. CABR
  # specimen evidence lives downstream in the checklists (built from a CLEANED
  # specimen record), never in this reference table.

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
  # The reference table now CONTAINS the ancestor taxa as their own rows (tagged
  # source_sheet == "iNat ancestry"). Split them out: the Holway ENTRIES are the
  # lookup's base (unchanged behavior); the ancestor rows are the id source that
  # gives each parent taxon its own iNat id. The id map is derived from every
  # id-bearing reference row (name at each row's own rank).
  is_ancestry    <- !is.na(holway_resolved$source_sheet) &
                    holway_resolved$source_sheet == "iNat ancestry"
  holway_entries <- holway_resolved[!is_ancestry, , drop = FALSE]
  ancestry_ids   <- ancestry_ids_from_reference(holway_resolved)
  message("Reference base: ", nrow(holway_entries), " Holway entries + ",
          sum(is_ancestry), " ancestor rows (", nrow(ancestry_ids), " id-bearing taxa).")
  bee_taxonomy_lookup <- build_bee_taxonomy_lookup(holway_entries, cl_sd, bees,
                                                   verified_ids = verified_ids,
                                                   ancestry_ids = ancestry_ids)
  # Merge curated SPECIMEN-ONLY species (data/reference/specimen_additions.csv, e.g. Colletes
  # phaceliae, Lasioglossum daggetti) as new leaf rows -- linked to EXISTING parents, never
  # fabricating a parent. Runs BEFORE id-resolution so the new species' ids/names get filled by the
  # resolver + your overrides just like everything else. A missing parent is reported, not created.
  .adds <- load_specimen_additions(PATHS$specimen_additions)
  if (nrow(.adds)) {
    .merged <- specimen_additions_to_lookup(bee_taxonomy_lookup, .adds)
    bee_taxonomy_lookup <- .merged$lookup
    message(sprintf("Specimen additions: appended %d new taxa.", nrow(.merged$added)))
    # Only a MISSING GENUS orphans a species; higher lineage ranks lacking a standalone lookup row
    # is normal (the lookup stores genus-and-below), so those are not flagged.
    .mp_g <- .merged$missing_parents[.merged$missing_parents$missing_parent_rank == "genus", , drop = FALSE]
    if (nrow(.mp_g)) {
      message("  WARNING: added taxa whose GENUS is not in the lookup (orphaned -- add the genus too):")
      print(as.data.frame(.mp_g))
    }
  }
  # Fill any STILL-missing taxon_ids by iNaturalist name-search (Holway-only taxa never observed in
  # SD, so no observation ancestry carried their id). Safe: assigns only unambiguous rank+name+parent
  # matches, everything else stays blank; cached to resolved_missing_ids.csv (auditable, no re-hits).
  bee_taxonomy_lookup <- tryCatch(resolve_missing_taxon_ids(bee_taxonomy_lookup),
    error = function(e) { message("  (resolve_missing_taxon_ids skipped: ", conditionMessage(e), ")"); bee_taxonomy_lookup })
  # Your recorded answers (manual_taxon_overrides.csv) win over the automated search: fill the id +
  # correct the name for renamed / synonym taxa. Then write the review worklist -- the prompt listing
  # whatever is STILL missing an id (one list; it already includes the Holway taxa) for you to look up.
  bee_taxonomy_lookup <- apply_manual_overrides(bee_taxonomy_lookup)
  # INTERACTIVE: ask you for the ids the auto-search couldn't find (appends to manual_taxon_overrides.csv),
  # then re-apply so anything you enter fills THIS run's lookup. Non-interactive (Rscript) -> skipped,
  # and the worklist file below is the fallback.
  prompt_missing_taxon_ids()
  bee_taxonomy_lookup <- apply_manual_overrides(bee_taxonomy_lookup)
  write_review_worklist()   # the still-open not_found set (after your answers) -> the "look these up" file
  # in_cabr_specimens is appended last by the specimen-additions merge; move it up beside in_holway.
  if (all(c("in_cabr_specimens", "in_holway") %in% names(bee_taxonomy_lookup)))
    bee_taxonomy_lookup <- dplyr::relocate(bee_taxonomy_lookup, in_cabr_specimens, .after = in_holway)
  write_fresh(decorate_complex(bee_taxonomy_lookup), PATHS$taxonomy_lookup, na = "")
  message("Wrote ", nrow(bee_taxonomy_lookup), " taxonomy rows (",
          sum(!bee_taxonomy_lookup$verified), " unverified, not found in Holway Checklist).")

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
