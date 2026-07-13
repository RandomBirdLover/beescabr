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
#
# NOTE on CRS: the project working CRS (EPSG:26946) is also defined in
# spatial_utils.R as PROJECT_CRS, because that file predates this one and
# is sourced independently by the spatial diagnostics scripts. The value
# here (BEESCABR_PROJECT_CRS) is identical and is the authority for the
# new API/DB code. Do not let the two drift.
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

# ---- Coordinate reference systems --------------------------------------------
# Storage CRS for the DuckDB `location` geometry column: raw lon/lat. Kept in
# 4326 so manual ad-hoc DB queries are portable; reproject to the metric
# project CRS with ST_Transform (or in sf) when distances are needed.
STORE_CRS          <- 4326L
BEESCABR_PROJECT_CRS <- 26946L  # NAD83 / California zone 6 (meters) -- matches spatial_utils.R

# ---- DuckDB cache ------------------------------------------------------------
# One on-disk DuckDB file acts as the cache for BOTH observation objects
# (with a spatial geometry column) and taxon request objects. This replaces
# the retired CSV export entirely and the Python taxon_cache.json.
DB_CACHE_PATH <- "data/cache/inat_cache.duckdb"

# Cached flattened export frame (RDS). Flattening ~77k observations from JSON
# and resolving their taxonomy is slow, so read_observations_export() memoizes
# the result here and only rebuilds when the observation/taxon cache content
# changes (a content fingerprint decides). Delete this file or set
# BEESCABR_REFRESH_FLAT=1 to force a rebuild.
EXPORT_FLAT_CACHE <- "data/cache/export_flat.rds"

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
  taxonomy_lookup = "data/outputs/reference/bee_taxonomy_lookup.csv",
  checklist_sd_county_inat = "data/outputs/checklists/sd_county/sd_county_inat_native_bee_checklist.csv",
  checklist_point_loma_inat = "data/outputs/checklists/point_loma/pl_inat_native_bee_checklist.csv",
  checklist_cabr_inat = "data/outputs/checklists/cabr/cabr_inat_bee_checklist.csv",
  checklist_sd_county_v2 = "data/outputs/checklists/sd_county/sd_county_native_bee_checklist.csv",
  checklist_point_loma_v2 = "data/outputs/checklists/point_loma/pl_native_bee_checklist.csv",
  checklist_cabr_v2 = "data/outputs/checklists/cabr/cabr_combined_native_bee_checklist.csv",
  checklist_cabr_specimen = "data/outputs/checklists/cabr/cabr_specimen_bee_checklist.csv",
  inat_clean = "data/outputs/inat_clean/cabr_inat_bee_clean.csv",
  inat_unknown_tags = "data/outputs/inat_clean/qc/cabr_inat_bee_unknown_tags.csv",
  specimen_clean = "data/outputs/specimens/cabr_specimen_bee_record_clean.csv",
  holway_combined = "data/reference_exports/holway_2026/holway_v3_combined.csv",
  holway_decisions = "data/outputs/reference/holway_taxon_decisions.csv"
)

# Standard ranked-name columns produced from the iNat taxon ancestry, in the
# exact order and naming the retired CSV export used.
TAXON_RANK_COLUMNS <- c(
  "taxon_kingdom_name", "taxon_phylum_name", "taxon_class_name",
  "taxon_order_name", "taxon_superfamily_name", "taxon_family_name",
  "taxon_subfamily_name", "taxon_tribe_name", "taxon_genus_name",
  "taxon_species_name", "taxon_subspecies_name"
)
