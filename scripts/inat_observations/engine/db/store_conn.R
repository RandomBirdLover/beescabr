# =============================================================
# inat_observations/engine/db/store_conn.R
# beescabr pipeline -- DuckDB connection + schema bootstrap
# Created: 2026-07-13 (API + DuckDB rewrite)
#
# Owns exactly one thing: opening a DuckDB connection with the spatial +
# json extensions loaded, and guaranteeing the cache schema exists. The
# two repositories (observations_store.R, taxon_store.R) and the decision
# store build on the connection this returns; none of them re-run DDL.
#
# The DuckDB file backs BOTH caches (observation objects with a spatial
# geometry column, and taxon request objects), replacing the retired CSV
# export and the Python taxon_cache.json.
#
# Depends on: DBI, duckdb. Sources config.R for DB_CACHE_PATH.
# =============================================================

library(DBI)

if (!exists("DB_CACHE_PATH")) source("scripts/config.R")

# ------------------------------------------------------------
# store_connect(): open (creating if needed) the cache DB, load extensions,
# ensure schema. Returns a DBIConnection. Caller must store_disconnect().
# ------------------------------------------------------------
#' Open the DuckDB observation cache
#'
#' @param path The database file. Its folder is created if absent.
#' @param read_only Open read-only, so a reader cannot block a writer.
#' @return An open connection. Close it with `store_disconnect()`.
store_connect <- function(path = DB_CACHE_PATH, read_only = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only)

  # spatial provides the GEOMETRY type + ST_* functions used for the
  # observation `location` column and for manual distance queries. json is
  # bundled in modern DuckDB but we load defensively.
  tryCatch({
    DBI::dbExecute(con, "INSTALL spatial;")
    DBI::dbExecute(con, "LOAD spatial;")
  }, error = function(e) {
    stop("Failed to load DuckDB spatial extension (needed for the geometry ",
         "column). Ensure network access on first run so DuckDB can fetch it. ",
         "Original error: ", conditionMessage(e))
  })
  tryCatch({
    DBI::dbExecute(con, "INSTALL json;")
    DBI::dbExecute(con, "LOAD json;")
  }, error = function(e) invisible(NULL))  # json is usually built-in

  if (!read_only) ensure_schema(con)
  con
}

#' Close a cache connection and shut the database down
#'
#' @param con The connection to close.
#' @return Invisibly, nothing.
store_disconnect <- function(con) {
  DBI::dbDisconnect(con, shutdown = TRUE)
}

# ------------------------------------------------------------
# ensure_schema(): idempotent DDL. Geometry stored in EPSG:4326 (raw
# lon/lat) by convention -- reproject with ST_Transform in manual queries.
# ------------------------------------------------------------
ensure_schema <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS inat_observations (
      id          BIGINT PRIMARY KEY,
      taxon_id    BIGINT,
      observed_on DATE,
      latitude    DOUBLE,
      longitude   DOUBLE,
      location    GEOMETRY,
      raw_data    JSON
    );
  ")

  # Cache keyed by a string so both id-lookups ('id:123') and name-searches
  # ('name:Melissodes') share one table, like the Python taxon_cache dict.
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS taxon_cache (
      cache_key   VARCHAR PRIMARY KEY,
      taxon_id    BIGINT,
      raw_data    JSON,
      fetched_at  TIMESTAMP
    );
  ")

  # Persisted manual disambiguation decisions from the Holway reference
  # builder, so interactive reruns never re-prompt for an already-decided
  # search term.
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS holway_decisions (
      search_term     VARCHAR PRIMARY KEY,
      chosen_taxon_id BIGINT,
      action          VARCHAR,
      decided_at      TIMESTAMP
    );
  ")
  invisible(con)
}
