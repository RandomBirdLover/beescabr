# =============================================================
# config.R
# beescabr pipeline -- central configuration
# Created: 2026-07-13 (API + DuckDB rewrite)
#
# Single source of truth for the constants that used to be scattered
# across native_bee_checklist.R, inat_bee_clean.R, spatial_utils.R and
# the old Python pipeline (place_id, taxon ids, CRS, the iNat user-agent,
# the DuckDB cache path, and the observation-field id map).
#
# Source this near the top of any script in the pipeline:
#   source("scripts/config.R")
# =============================================================

# ---- iNaturalist API ---------------------------------------------------------
INAT_BASE_URL   <- "https://api.inaturalist.org/v1/"
INAT_USER_AGENT <- "beescabr pipeline (brandirenesanchez16@gmail.com)"

# Pause between API calls (seconds). iNat's hard limit is ~100 requests/min
# and they ask you to keep well under it; with ~0.3-0.5s of network time per
# request, a 0.5s pause lands around ~70-80 req/min -- fast but courteous.
# Set to 0 to match the Python script (no pause) at your own rate-limit risk;
# the transport still backs off exponentially on 429/5xx.
INAT_THROTTLE_SEC <- 0.5

# ---- Taxa / place ------------------------------------------------------------
# Anthophila (all bees). Root taxon for every observation query.
TAXON_ANTHOPHILA <- 630955L
# Apis mellifera (Western honey bee) -- excluded via without_taxon_id so the
# checklist stays native-only. (This is the 47219 seen in the Python query.)
TAXON_APIS_MELLIFERA <- 47219L
# iNat place_id for the "San Diego County 25 Mile Buffer" custom place that
# the retired CSV export was scoped to. Same id used in the Python pipeline.
PLACE_SD_COUNTY_BUFFER <- 118491L

# ---- PLANT ingest: taxon / place ---------------------------------------------
# Vascular plants (Tracheophyta) -- the ROOT taxon for the plant pull. Chosen
# over Angiospermae because gymnosperms AND angiosperms both bear pollen (the bee
# forage we care about); the few ferns/lycophytes that ride along are harmless and
# get dropped by the flowering/pollen filter downstream. Same philosophy as the
# bee ingest: pull broad at the root, narrow in the clean/analysis layer.
# Sanity-check the id on the first run (the ingest prints it): api taxa/211194.
TAXON_TRACHEOPHYTA <- 211194L
# iNat place for the plant pull. Point Loma Peninsula (132551) FULLY CONTAINS the
# cabr_survey_box (which is hand-drawn to extend past the NPS monument outline),
# so it won't clip edge survey spots; the brain re-filters to the exact box by
# point-in-polygon, discarding the rest. Cabrillo National Monument alone
# (place 4715) is tighter/smaller but risks clipping the box -- kept for reference.
PLACE_POINT_LOMA    <- 132551L
PLACE_CABR_MONUMENT <- 4715L

# ---- DuckDB cache ------------------------------------------------------------
# One on-disk DuckDB file acts as the cache for BOTH observation objects
# (with a spatial geometry column) and taxon request objects. This replaces
# the retired CSV export entirely and the Python taxon_cache.json.
DB_CACHE_PATH <- "data/observations/cache/inat_cache.duckdb"

# Cached flattened export frame (RDS). Flattening ~77k observations from JSON
# and resolving their taxonomy is slow, so read_observations_export() memoizes
# the result here and only rebuilds when the observation/taxon cache content
# changes (a content fingerprint decides). Delete this file or set
# BEESCABR_REFRESH_FLAT=1 to force a rebuild.
EXPORT_FLAT_CACHE <- "data/observations/cache/export_flat.rds"

# Records when observations were last ingested, so a refresh can also re-pull
# observations that were re-identified/edited on iNaturalist since then
# (updated_since), not just brand-new ones.
INGEST_STATE_PATH <- "data/observations/cache/last_ingest.txt"

# ---- PLANT cache (kept SEPARATE from the bee cache) ---------------------------
# The plant ingest uses its OWN DuckDB file, export RDS, and updated_since state,
# so its incremental id-cursor never collides with the bee cache and a plant pull
# never invalidates the (expensive) 77k-bee export fingerprint. Identical schema
# and code path -- just a different connection + paths (see ingest_plants.R).
DB_CACHE_PATH_PLANT     <- "data/observations/cache/inat_cache_plant.duckdb"
EXPORT_FLAT_PLANT_CACHE <- "data/observations/cache/export_flat_plant.rds"
INGEST_STATE_PATH_PLANT <- "data/observations/cache/last_ingest_plant.txt"

# ---- Observation-field id map ------------------------------------------------
# The flatten step builds `field:<lower(name)>` columns generically from the
# API `ofvs` array (guaranteeing name-parity with the old CSV export). This
# map is the STABLE-ID reference for the handful of fields the pipeline treats
# specially -- names can be edited on iNat, ids cannot. Use these when logic
# must not break on a display-name change.
KNOWN_FIELD_IDS <- list(
  flower_visited      = 3126L,   # datatype "taxon" -> read ofv$taxon$name, NOT ofv$value
  transect_name       = 19447L,  # datatype "text"  -> structured transect (e.g. "TP1")
  tags_override       = 20521L,  # manual survey-hashtag correction
  nest                = 4837L,
  nesting             = 988L
)

# Export column names (post "field:" prefix, lowercased) that get coalesced
# into single tidy columns downstream. Kept here so the mapping lives in one
# place instead of being buried in a mutate() in inat_bee_clean.R.
FLOWER_VISITED_SOURCES <- c(
  "field:interaction->visited flower of",  # authoritative
  "field:name of associated plant",
  "field:nectar / pollen delivering plant",
  "field:nectar plant"
)
NESTING_SOURCES <- c("field:nest", "field:nesting")

# ---- Output paths ------------------------------------------------------------
# Centralized so renames happen in one spot (the old scripts hardcoded these
# inline in many places).
PATHS <- list(
  taxonomy_lookup = "data/reference/sd_bee_taxonomy_lookup.csv",
  # Internal-only: species -> species-complex taxon_id map. complex_taxon_id is
  # an iNat implementation detail (it looks identical to a species scientific
  # name and confuses non-scientists), so it's stripped from the public
  # checklists and parked here for the specimen complex-match step to read.
  complex_map = "data/observations/cache/complex_taxon_id_map.csv",
  # Built ONCE from Holway's v3 checklist (resolving names -> iNat taxon_ids is
  # slow + interactive), then reused every run. Bump the version suffix only when
  # Holway ships a new checklist and you rebuild (BEESCABR_REBUILD_HOLWAY_REF=1).
  holway_reference = "data/reference/holway_sd_bee_reference_table_v3.csv",
  verified_taxa = "data/reference/verified_taxa.csv",
  specimen_additions = "data/reference/specimen_additions.csv",   # curated specimen-only species merged into the lookup
  plant_taxonomy_lookup = "data/reference/cabr_plant_taxonomy_lookup.csv",       # basic-rank plant lookup (obs + specimen flowers)
  plant_specimen_overrides = "data/reference/plant_specimen_overrides.csv",      # curated expert corrections for specimen-label plants
  plant_not_in_park = "data/reference/cabr_plant_specimen_not_in_park.csv",       # worklist: specimen-label plants not confirmed in park
  plant_name_cache = "data/reference/plant_name_resolution_cache.csv",           # name -> iNat taxon resolution cache
  plant_all_taxa = "data/observations/inat_clean/cabr_inat_plant_all_taxa.csv",   # ALL in-park plant taxa (any observer) -- in-park truth
  plant_park_confirmed = "data/reference/plant_park_confirmed.csv",               # curated: species the botanist confirms are in the park (e.g. obscured threatened taxa)
  checklist_sd_county_inat = "data/checklists/sd_county/sd_county_inat_native_bee_checklist.csv",
  checklist_point_loma_inat = "data/checklists/point_loma/pl_inat_native_bee_checklist.csv",
  checklist_cabr_inat = "data/checklists/cabr/cabr_inat_bee_checklist.csv",
  checklist_sd_county_v2 = "data/checklists/sd_county/sd_county_native_bee_checklist.csv",
  checklist_point_loma_v2 = "data/checklists/point_loma/pl_native_bee_checklist.csv",
  checklist_cabr_v2 = "data/checklists/cabr/cabr_combined_native_bee_checklist.csv",
  checklist_cabr_specimen = "data/checklists/cabr/cabr_specimen_bee_checklist.csv",
  inat_clean = "data/observations/inat_clean/cabr_inat_bee_clean.csv",
  inat_plant_clean = "data/observations/inat_clean/cabr_inat_plant_clean.csv",
  inat_unknown_tags = "data/observations/inat_clean/qc/cabr_inat_bee_unknown_tags.csv",
  inat_to_verify = "data/observations/inat_clean/qc/cabr_inat_to_verify.csv",
  specimen_clean = "data/specimens/cleaned/cabr_specimen_bee_clean.csv",
  holway_combined = "data/reference/source/holway_2026/holway_v3_combined.csv"
)

# Standard ranked-name columns produced from the iNat taxon ancestry, in the
# exact order and naming the retired CSV export used (subtribe added 2026-07;
# subphylum/subclass/suborder/infraorder/epifamily added 2026-07 -- all sourced
# from the iNat ancestry walk, never guessed).
TAXON_RANK_COLUMNS <- c(
  "taxon_kingdom_name", "taxon_phylum_name", "taxon_subphylum_name",
  "taxon_class_name", "taxon_subclass_name",
  "taxon_order_name", "taxon_suborder_name", "taxon_infraorder_name",
  "taxon_superfamily_name", "taxon_family_name", "taxon_epifamily_name",
  "taxon_subfamily_name", "taxon_tribe_name", "taxon_subtribe_name",
  "taxon_genus_name", "taxon_species_name", "taxon_subspecies_name"
)

# The taxonomic hierarchy column order requested for the reference/lookup
# outputs, with metadata columns first. 19 levels (the full sub-rank set).
TAXONOMY_LEVELS <- c(
  "kingdom", "phylum", "subphylum", "class", "subclass",
  "order", "suborder", "infraorder", "superfamily",
  "family", "epifamily", "subfamily", "tribe", "subtribe",
  "genus", "subgenus", "complex", "species", "subspecies"
)
