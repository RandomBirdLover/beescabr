# =============================================================
# inat_observations/engine/db/observations_store.R
# beescabr pipeline -- observation object repository (DuckDB)
# Created: 2026-07-13 (API + DuckDB rewrite)
#
# Stores raw iNat observation JSON with an extracted spatial `location`
# geometry column so ad-hoc manual queries (ST_Within / ST_DWithin against
# a boundary or point) run directly on the DB without parsing JSON. Only
# the id, taxon_id, observed_on, lat/long and geometry are promoted to
# columns; everything else stays in raw_data and is reconstructed by the
# flatten layer on read.
#
# This repo is the CACHE. Populating it from the API is the ingest
# pipeline's job (pipelines/ingest_inat.R); consuming it into an
# export-shaped frame is pipelines/read_inat.R's job.
#
# Depends on: DBI, duckdb, jsonlite. Assumes spatial extension already
# loaded by store_connect().
# =============================================================

library(DBI)

# ------------------------------------------------------------
# write_observations(): bulk-upsert ONE page of raw observation objects into
# inat_observations. The whole page is serialized to a single JSON array in
# ONE jsonlite::toJSON call, handed to DuckDB as one string, and unnested +
# parsed inside DuckDB (C++). This is the key performance/robustness fix:
#
#   - ONE toJSON per page (not one per object) -- avoids a per-row R loop
#     over hundreds of large, deeply-nested iNat objects.
#   - DuckDB does the JSON parsing via unnest(CAST(arr AS JSON[])), casting
#     each element once (CTE `c`) and extracting coordinates once.
#   - Geometry via ST_Point(lon, lat) from the extracted DOUBLEs -- much
#     cheaper than re-parsing GeoJSON per row. Stored EPSG:4326 (lon/lat).
#   - Only one page (~200 objects) is ever held in memory. The caller keeps
#     a transaction open across pages and commits every N (see ingest), so
#     commits are batched WITHOUT buffering thousands of objects in R.
# ------------------------------------------------------------
write_observations <- function(con, results) {
  if (length(results) == 0) return(invisible(0L))

  page_json <- as.character(jsonlite::toJSON(results, auto_unbox = TRUE, null = "null"))
  stg <- data.frame(arr = page_json, stringsAsFactors = FALSE)

  duckdb::duckdb_register(con, "stg_obs", stg)
  on.exit(duckdb::duckdb_unregister(con, "stg_obs"), add = TRUE)

  DBI::dbExecute(con, "
    INSERT OR REPLACE INTO inat_observations
    WITH e AS (SELECT unnest(CAST(arr AS JSON[])) AS j FROM stg_obs),
    c AS (
      SELECT j,
        CAST(j->>'$.id' AS BIGINT)                            AS id,
        TRY_CAST(j->>'$.taxon.id' AS BIGINT)                 AS taxon_id,
        TRY_CAST(j->>'$.observed_on' AS DATE)                AS observed_on,
        COALESCE(TRY_CAST(j->>'$.private_geojson.coordinates[1]' AS DOUBLE), TRY_CAST(j->>'$.geojson.coordinates[1]' AS DOUBLE)) AS lat,
        COALESCE(TRY_CAST(j->>'$.private_geojson.coordinates[0]' AS DOUBLE), TRY_CAST(j->>'$.geojson.coordinates[0]' AS DOUBLE)) AS lon
      FROM e
    )
    SELECT
      id, taxon_id, observed_on, lat, lon,
      CASE WHEN lon IS NOT NULL AND lat IS NOT NULL
           THEN ST_Point(lon, lat) ELSE NULL END,
      j
    FROM c
    WHERE id IS NOT NULL;
  ")
  invisible(length(results))
}

# ------------------------------------------------------------
# write_observations_page(): the FAST ingest path. Takes the RAW, unparsed
# response body string for one /observations page, and does ALL of the work
# in DuckDB (C++): extract the results array, unnest it, insert, and report
# the page's row count + max id for the pagination cursor. R never parses the
# JSON -- eliminating the jsonlite parse + re-serialize round-trip that made
# the ingest far slower than the Python script.
#
# Returns list(n = rows inserted this page, last_id = max obs id on the page).
# Does NOT manage a transaction; the ingest loop keeps one open across pages.
# ------------------------------------------------------------
#' Write one page of API results into the cache
#'
#' The raw JSON text is handed to DuckDB to parse, rather than parsed in R --
#' an ingest is many pages and this is the difference between minutes and hours.
#'
#' @param con An open cache connection.
#' @param raw_text The page's JSON, as returned by the API.
#' @return Invisibly, how many observations were written.
write_observations_page <- function(con, raw_text) {
  stg <- data.frame(arr = raw_text, stringsAsFactors = FALSE)
  duckdb::duckdb_register(con, "stg_page", stg)
  on.exit(duckdb::duckdb_unregister(con, "stg_page"), add = TRUE)

  # Parse the response + unnest its results array ONCE into a temp table.
  DBI::dbExecute(con, "
    CREATE OR REPLACE TEMP TABLE _page AS
    SELECT unnest(CAST(arr->>'$.results' AS JSON[])) AS j FROM stg_page;
  ")

  DBI::dbExecute(con, "
    INSERT OR REPLACE INTO inat_observations
    WITH c AS (
      SELECT j,
        CAST(j->>'$.id' AS BIGINT)                            AS id,
        TRY_CAST(j->>'$.taxon.id' AS BIGINT)                 AS taxon_id,
        TRY_CAST(j->>'$.observed_on' AS DATE)                AS observed_on,
        COALESCE(TRY_CAST(j->>'$.private_geojson.coordinates[1]' AS DOUBLE), TRY_CAST(j->>'$.geojson.coordinates[1]' AS DOUBLE)) AS lat,
        COALESCE(TRY_CAST(j->>'$.private_geojson.coordinates[0]' AS DOUBLE), TRY_CAST(j->>'$.geojson.coordinates[0]' AS DOUBLE)) AS lon
      FROM _page
    )
    SELECT id, taxon_id, observed_on, lat, lon,
      CASE WHEN lon IS NOT NULL AND lat IS NOT NULL
           THEN ST_Point(lon, lat) ELSE NULL END,
      j
    FROM c
    WHERE id IS NOT NULL;
  ")

  stats <- DBI::dbGetQuery(con, "
    SELECT COUNT(*) AS n,
           CAST(MAX(CAST(j->>'$.id' AS BIGINT)) AS DOUBLE) AS last_id
    FROM _page;
  ")
  list(n = as.integer(stats$n[1]),
       last_id = if (is.na(stats$last_id[1])) NA_real_ else stats$last_id[1])
}

# ------------------------------------------------------------
# Read helpers.
# ------------------------------------------------------------
#' How many observations the cache holds
#'
#' @param con An open cache connection.
#' @return The row count.
count_observations <- function(con) {
  DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM inat_observations")$n[1]
}

# ------------------------------------------------------------
# clear_observations(): empty the cache so a FULL re-download rebuilds it from
# scratch. This is the ONLY way to drop a row whose iNat re-ID moved it out of
# the bee filter -- a plain upsert re-walk never re-returns that obs, so it can
# never be deleted otherwise. Called ONLY on a full ingest; incremental runs
# never clear. Returns the number of rows removed (for logging).
# ------------------------------------------------------------
#' Empty the observations table
#'
#' @param con An open cache connection.
#' @return Invisibly, how many rows were deleted.
clear_observations <- function(con) {
  n <- count_observations(con)
  DBI::dbExecute(con, "DELETE FROM inat_observations")
  invisible(n)
}

# Highest stored observation id -- the incremental fetch cursor. 0 when empty.
# NB: returns NUMERIC (double), never as.integer(). iNat observation ids already
# run into the hundreds of millions and will cross the 32-bit integer ceiling
# (~2.1 billion), where as.integer() silently yields NA -> id_above = NA -> the
# incremental cursor breaks and ingest stops. Doubles hold whole numbers exactly
# to 2^53, and the ingest consumer formats this with sprintf("%.0f", ...), so
# there is no precision or scientific-notation loss.
max_observation_id <- function(con) {
  v <- DBI::dbGetQuery(con, "SELECT MAX(id) AS m FROM inat_observations")$m[1]
  if (is.na(v)) 0 else as.numeric(v)
}

# Return raw observation JSON strings (id + raw_data) for the flatten layer.
read_observations_raw <- function(con) {
  DBI::dbGetQuery(con, "SELECT id, raw_data FROM inat_observations ORDER BY id")
}
