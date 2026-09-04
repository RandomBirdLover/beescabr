# =============================================================
# inat_observations/engine/pipelines/ingest_inat.R
# beescabr pipeline -- populate the DuckDB observation cache from the API
# Rewritten: 2026-07-13 (raw-body ingest; the fast path)
#
# THE ONE PLACE that fetches observations. Both taxonomy_lookup_build.R and
# inat_bee_clean.R depend on the cache this fills, but neither fetches -- so a
# manual DB query, the lookup build, and the clean script all see identical data
# refreshed once. Replaces the retired CSV export.
#
# Speed design (why this matches/beats the Python script):
#   - The raw response string for each page is handed straight to DuckDB,
#     which parses + inserts + reports the pagination cursor in C++. R never
#     builds nested lists from the JSON, and never re-serializes it. That
#     parse->serialize->parse round-trip in R was the real bottleneck.
#   - Only one page (~200 objects, as a single string) is in memory at a time.
#   - One transaction stays open across `commit_every` pages, so commits are
#     batched but the incremental id cursor stays durable if a run is stopped.
#   - The throttle between requests is configurable (config INAT_THROTTLE_SEC).
#
# Incremental by default (id_above cursor from the highest stored id).
#
# Depends on: api/inat_http.R, db/observations_store.R, config.R.
# =============================================================

if (!exists("inat_request_text"))     source("scripts/inat_observations/engine/api/inat_http.R")
if (!exists("write_observations_page")) source("scripts/inat_observations/engine/db/observations_store.R")
if (!exists("TAXON_ANTHOPHILA"))      source("scripts/config.R")

ingest_observations <- function(con,
                                place_id = PLACE_SD_COUNTY_BUFFER,
                                taxon_id = TAXON_ANTHOPHILA,
                                without_taxon_id = TAXON_APIS_MELLIFERA,
                                incremental = TRUE,
                                state_path = INGEST_STATE_PATH,
                                extra_query = list(),
                                per_page = 200L,
                                commit_every = 25L,
                                throttle = INAT_THROTTLE_SEC,
                                request_text_fn = inat_request_text,
                                sleep_fn = Sys.sleep,
                                verbose = TRUE) {

  base_query <- c(list(
    place_id         = place_id,
    taxon_id         = taxon_id,
    without_taxon_id = without_taxon_id,
    order_by         = "id",
    order            = "asc",
    per_page         = per_page
  ), extra_query)

  id_above <- as.numeric(if (incremental) max_observation_id(con) else 0L)
  if (verbose)
    message(sprintf("Ingest: place_id=%s taxon_id=%s (exclude %s) | %s from id_above=%.0f | %d/page, commit every %d",
                    place_id, taxon_id, without_taxon_id,
                    if (incremental) "incremental" else "full", id_above, per_page, commit_every))

  # FULL rebuild only (BEESCABR_FULL_INGEST=1 -> incremental=FALSE): empty the cache
  # first so the fresh download can't leave stale rows behind. An obs re-IDed OUT of
  # the bee filter is never re-returned by the API, so a plain upsert re-walk can't
  # delete it -- only starting from empty can. Normal incremental runs never clear.
  # If a full rebuild is interrupted the cache is left partial; just run it again.
  if (!incremental) {
    removed <- clear_observations(con)
    if (verbose) message(sprintf("Full rebuild: cleared %d cached observations; re-downloading everything.", removed))
  }

  n_written <- 0L; in_txn <- FALSE
  begin_txn  <- function() if (!in_txn) { DBI::dbBegin(con);  in_txn <<- TRUE }
  commit_txn <- function() if (in_txn)  { DBI::dbCommit(con); in_txn <<- FALSE }
  on.exit(if (in_txn) tryCatch(DBI::dbRollback(con), error = function(e) NULL), add = TRUE)

  # One id_above-cursor pass over a query; used for both the new-obs pass and
  # the updated_since (re-ID) pass. Returns rows written by this pass.
  run_pass <- function(pass_query, start_id, label) {
    id_above <- as.numeric(start_id); pages <- 0L; since_commit <- 0L; written <- 0L
    repeat {
      q <- c(pass_query, list(id_above = sprintf("%.0f", id_above)))
      stats <- { begin_txn(); write_observations_page(con, request_text_fn("observations", query = q)) }
      if (stats$n == 0L) break
      written <- written + stats$n; id_above <- stats$last_id
      pages <- pages + 1L; since_commit <- since_commit + 1L
      if (verbose && pages %% 10L == 0L)
        message(sprintf("  [%s] page %d: %d rows (id up to %.0f)", label, pages, written, id_above))
      if (since_commit >= commit_every) { commit_txn(); since_commit <- 0L
        if (verbose) message(sprintf("  [%s] committed -> %d rows", label, written)) }
      if (.page_done(stats$n, per_page)) break
      if (!is.null(throttle) && throttle > 0) sleep_fn(throttle)
    }
    commit_txn()
    written
  }

  # Capture the cutoff BEFORE fetching, not after. An observation edited on iNat
  # WHILE this run is fetching would otherwise carry updated_at < an end-of-run stamp
  # yet be missed (its page already passed) -> lost from every future refresh. Stamping
  # the START time makes the next run's updated_since span this run's whole fetch window
  # (over-inclusive, but the INSERT-OR-REPLACE upsert makes any re-pull idempotent).
  run_started <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

  # Pass 1: brand-new observations (id above the highest we hold).
  n_written <- n_written + run_pass(base_query, id_above, "new")

  # Pass 2: re-identified/edited observations since our last ingest. iNat's
  # updated_since returns anything changed after that time; upsert catches the
  # re-IDs. Skipped on a full re-walk (which already re-pulls everything).
  last_ts <- if (incremental && file.exists(state_path)) trimws(readLines(state_path, warn = FALSE)[1]) else NA
  if (!is.na(last_ts) && nzchar(last_ts)) {
    if (verbose) message("Ingest: re-pulling observations updated since ", last_ts, " ...")
    n_written <- n_written + run_pass(c(base_query, list(updated_since = last_ts)), 0L, "updated")
  }

  # Persist the RUN-START cutoff captured above (before any fetch), so the next
  # refresh's updated_since re-pulls anything edited during this run too.
  dir.create(dirname(state_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(run_started, state_path)

  if (verbose)
    message(sprintf("Ingest complete: %d rows written. Cache holds %d observations.",
                    n_written, count_observations(con)))
  invisible(n_written)
}
