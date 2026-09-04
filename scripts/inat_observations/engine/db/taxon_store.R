# =============================================================
# inat_observations/engine/db/taxon_store.R
# beescabr pipeline -- taxon request cache repository (DuckDB)
# Created: 2026-07-13 (API + DuckDB rewrite)
#
# Caches raw /taxa responses keyed by a string, so a taxon (or a name
# search) is fetched from iNat at most once across the whole pipeline --
# the DuckDB replacement for the Python taxon_cache.json. Keying by string
# lets id-lookups ('id:632955') and name-searches ('name:Melissodes')
# share one table.
#
# Pure key helpers (taxon_cache_key_*) are unit-tested; the DB functions
# are thin DBI wrappers.
#
# Depends on: DBI, duckdb, jsonlite.
# =============================================================

library(DBI)

#' Cache key for a taxon looked up by id
#'
#' @param id iNaturalist taxon id.
#' @return The key, `"id:<n>"`.
taxon_cache_key_id   <- function(id)   paste0("id:", as.integer(id))
#' Cache key for a taxon looked up by name
#'
#' @param name The name searched for; lower-cased and trimmed so spelling
#'   variants share one cache entry.
#'
#' @return The key, `"name:<name>"`.
taxon_cache_key_name <- function(name) paste0("name:", tolower(trimws(name)))

# Return parsed JSON (a nested list) for a cache key, or NULL on miss.
taxon_cache_get <- function(con, cache_key) {
  res <- DBI::dbGetQuery(
    con, "SELECT raw_data FROM taxon_cache WHERE cache_key = ?",
    params = list(cache_key)
  )
  if (nrow(res) == 0) return(NULL)
  jsonlite::fromJSON(res$raw_data[1], simplifyVector = FALSE)
}

# Upsert a raw taxon/search response (an R list) under a cache key.
taxon_cache_put <- function(con, cache_key, taxon_id, value) {
  raw_json <- jsonlite::toJSON(value, auto_unbox = TRUE, null = "null")
  DBI::dbExecute(
    con,
    "INSERT OR REPLACE INTO taxon_cache (cache_key, taxon_id, raw_data, fetched_at)
     VALUES (?, ?, ?, now())",
    params = list(cache_key,
                  if (is.null(taxon_id) || is.na(taxon_id)) NA_integer_ else as.integer(taxon_id),
                  raw_json)
  )
  invisible(cache_key)
}

taxon_cache_count <- function(con) {
  DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM taxon_cache")$n[1]
}
