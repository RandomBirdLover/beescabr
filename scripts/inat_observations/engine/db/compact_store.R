# =============================================================
# inat_observations/engine/db/compact_store.R
# beescabr -- reclaim space in an observation cache without losing a row.
#
# DuckDB never hands space back to the filesystem on its own. An updated row
# leaves its old copy behind as a dead block, and the incremental ingest updates
# rows on every run, so the file only grows. The bee cache reached 31 GB holding
# 78,578 observations; copying it into a fresh file brought it to 7.9 GB with
# everything intact. Most of that is re-compression -- DuckDB packs data far
# better writing it in one pass than accumulating it over years -- and the rest
# is the dead blocks.
#
# NOTHING IS EVER PRUNED. The temptation is to drop the big JSON keys nobody
# reads (`identifications` is 92% of the payload), but that JSON is a snapshot of
# what an identifier saw at the time, and 414 of its 918 taxa are not in
# taxon_cache, so it cannot be rebuilt from a join. Compaction copies every row
# and every value; it only stops storing them wastefully.
#
# The copy is verified against the original BEFORE it replaces it, and the
# original is only removed once that passes.
# =============================================================

DB_COMPACT_THRESHOLD <- 0.20   # reclaim when this share of the file is dead space
DB_COMPACT_MIN_BLOCKS <- 2000  # ignore small files: rewriting them costs more than it saves

# db_should_compact(): PURE. Worth rewriting the file?
db_should_compact <- function(total_blocks, free_blocks,
                              threshold = DB_COMPACT_THRESHOLD,
                              min_blocks = DB_COMPACT_MIN_BLOCKS) {
  total_blocks <- suppressWarnings(as.numeric(total_blocks))
  free_blocks  <- suppressWarnings(as.numeric(free_blocks))
  if (!isTRUE(total_blocks > 0) || is.na(free_blocks)) return(FALSE)
  if (total_blocks < min_blocks) return(FALSE)
  (free_blocks / total_blocks) >= threshold
}

.dbc_fingerprint <- function(con, catalog, tbl) {
  dbGetQuery(con, sprintf(
    "SELECT count(*) AS n,
            CAST(COALESCE(bit_xor(hash(CAST(x AS VARCHAR))), 0) AS VARCHAR) AS h
     FROM (SELECT CAST(COLUMNS(*) AS VARCHAR) FROM %s.%s) x", catalog, tbl))
}

# every table must match on row count AND an order-independent hash of every value
.dbc_verify <- function(con, tabs) {
  for (t in tabs) {
    a <- .dbc_fingerprint(con, "src", t)
    b <- .dbc_fingerprint(con, "dst", t)
    if (!identical(a$n, b$n) || !identical(a$h, b$h)) return(FALSE)
  }
  TRUE
}

# db_compact(): rewrite `path` as a fresh file. Returns list(ok, before_mb, after_mb).
# On any failure the original is left exactly as it was.
db_compact <- function(path, verify_fn = .dbc_verify, say = message) {
  if (!requireNamespace("duckdb", quietly = TRUE) || !file.exists(path))
    return(list(ok = FALSE, before_mb = NA, after_mb = NA))
  before <- file.info(path)$size
  tmp <- paste0(path, ".compacting")
  if (file.exists(tmp)) unlink(tmp)
  ok <- FALSE
  con <- NULL
  tryCatch({
    con <- DBI::dbConnect(duckdb::duckdb())
    try(DBI::dbExecute(con, "INSTALL spatial; LOAD spatial;"), silent = TRUE)
    DBI::dbExecute(con, sprintf("ATTACH '%s' AS src (READ_ONLY)", path))
    DBI::dbExecute(con, sprintf("ATTACH '%s' AS dst", tmp))
    DBI::dbExecute(con, "COPY FROM DATABASE src TO dst")
    tabs <- DBI::dbGetQuery(con,
      "SELECT table_name FROM information_schema.tables WHERE table_catalog = 'src'")$table_name
    ok <- isTRUE(verify_fn(con, tabs))
  }, error = function(e) say("  compact: ", conditionMessage(e)))
  if (!is.null(con)) try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)

  if (!ok || !file.exists(tmp)) { unlink(tmp); return(list(ok = FALSE, before_mb = before/1048576, after_mb = NA)) }
  bak <- paste0(path, ".bak")
  if (!file.rename(path, bak))          { unlink(tmp); return(list(ok = FALSE, before_mb = before/1048576, after_mb = NA)) }
  if (!file.rename(tmp, path))          { file.rename(bak, path); unlink(tmp)
                                          return(list(ok = FALSE, before_mb = before/1048576, after_mb = NA)) }
  after <- file.info(path)$size
  unlink(bak)                            # only now, with the new file in place and verified
  list(ok = TRUE, before_mb = before/1048576, after_mb = after/1048576)
}

# db_compact_if_needed(): the step that runs after an ingest. Asks the file how
# much dead space it carries (instant) and only rewrites when it is worth it.
db_compact_if_needed <- function(path, threshold = DB_COMPACT_THRESHOLD, say = message) {
  if (!requireNamespace("duckdb", quietly = TRUE) || !file.exists(path)) return(invisible(NULL))
  sz <- tryCatch({
    con <- DBI::dbConnect(duckdb::duckdb(), path, read_only = TRUE)
    on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
    DBI::dbGetQuery(con, "CALL pragma_database_size()")
  }, error = function(e) NULL)
  if (is.null(sz) || !nrow(sz)) return(invisible(NULL))
  if (!db_should_compact(sz$total_blocks[1], sz$free_blocks[1], threshold)) return(invisible(NULL))
  say(sprintf("  compacting %s (%.0f%% dead space)...", basename(path),
              100 * sz$free_blocks[1] / sz$total_blocks[1]))
  r <- db_compact(path, say = say)
  if (isTRUE(r$ok))
    say(sprintf("  compacted %s: %.1f GB -> %.1f GB", basename(path),
                r$before_mb/1024, r$after_mb/1024))
  invisible(r)
}
