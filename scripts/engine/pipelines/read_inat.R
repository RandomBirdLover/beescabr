# =============================================================
# pipelines/read_inat.R
# beescabr pipeline -- consume the cache into an export-shaped data frame
# Created: 2026-07-13 (API + DuckDB rewrite)
#
# The read counterpart to ingest_inat.R. Turns cached observation objects
# into a data frame with the SAME columns the retired CSV export had
# (id, user_login, coords, tag_list, quality, taxon_*_name hierarchy, and
# field:* obs-fields), so taxonomy_lookup_build.R and inat_bee_clean.R only
# had to swap their input source, not their logic.
#
# Two-part assembly:
#   1. flatten each stored observation's raw JSON -> core + field:* columns
#   2. resolve the ranked taxonomy for the unique taxon ids (cached
#      /taxa/{id}) and join it on, filling taxon_kingdom_name ... taxon_subspecies_name
#      that the /observations payload does not carry inline.
#
# Depends on: db/observations_store.R, api/inat_flatten.R, api/inat_cache.R,
# config.R.
# =============================================================

if (!exists("read_observations_raw")) source("scripts/engine/db/observations_store.R")
if (!exists("flatten_observation"))   source("scripts/engine/api/inat_flatten.R")
if (!exists("resolve_taxonomy"))       source("scripts/engine/api/inat_cache.R")
if (!exists("TAXON_RANK_COLUMNS"))     source("scripts/config.R")

library(dplyr)

# In-process memo of the last-built export frame, keyed by content signature.
# Lets the two callers within one run_pipeline run (checklists + clean) share
# a single flatten instead of doing it twice.
.export_cache_env <- new.env(parent = emptyenv())

# ------------------------------------------------------------
# .export_signature(): a cheap content fingerprint of everything the export
# frame depends on -- the observation cache AND the taxon cache. Uses an
# order-independent xor of per-row content hashes (computed in DuckDB), so it
# changes iff a row is added, removed, or edited. This is what decides whether
# the cached flatten is still valid.
# ------------------------------------------------------------
.export_signature <- function(con) {
  q <- function(tbl) DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) AS n, CAST(COALESCE(bit_xor(hash(CAST(raw_data AS VARCHAR))), 0) AS VARCHAR) AS h FROM %s", tbl))
  o <- q("inat_observations"); t <- q("taxon_cache")
  paste(o$n[1], o$h[1], t$n[1], t$h[1], sep = "|")
}

# ------------------------------------------------------------
# read_observations_export(): build the export-shaped frame from the cache.
# `resolve_taxa = TRUE` joins the ranked taxonomy. The full (taxonomy-resolved)
# frame is CACHED to `cache_path` (RDS) keyed by the content signature: if the
# observation/taxon caches are unchanged, the flatten is skipped entirely and
# the cached frame is returned. Set resolve_taxa = FALSE for a fast, uncached
# coordinates/tags-only read.
# ------------------------------------------------------------
read_observations_export <- function(con, resolve_taxa = TRUE,
                                     request_fn = inat_request, verbose = TRUE,
                                     use_cache = TRUE, cache_path = EXPORT_FLAT_CACHE) {
  if (Sys.getenv("BEESCABR_REFRESH_FLAT", "0") == "1") use_cache <- FALSE
  cacheable <- resolve_taxa && use_cache
  sig <- if (cacheable) .export_signature(con) else NULL

  if (cacheable) {
    # 1) in-memory memo (same run, second caller)
    if (identical(.export_cache_env$sig, sig) && !is.null(.export_cache_env$frame)) {
      if (verbose) message("Export frame: cache hit (in-memory) -- skipping flatten.")
      return(.export_cache_env$frame)
    }
    # 2) on-disk cache (across runs / skip-ingest)
    if (file.exists(cache_path)) {
      cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)
      if (!is.null(cached) && identical(attr(cached, "signature"), sig)) {
        if (verbose) message(sprintf("Export frame: cache hit (disk, %d rows) -- skipping flatten.", nrow(cached)))
        .export_cache_env$sig <- sig; .export_cache_env$frame <- cached
        return(cached)
      }
    }
    if (verbose) message("Export frame: inputs changed or no cache -- rebuilding...")
  }

  raw <- read_observations_raw(con)
  if (nrow(raw) == 0) {
    warning("Observation cache is empty -- run ingest_observations() first.")
    return(tibble())
  }
  if (verbose) message(sprintf("Flattening %d cached observations...", nrow(raw)))

  core <- dplyr::bind_rows(lapply(raw$raw_data, function(j) {
    flatten_observation(jsonlite::fromJSON(j, simplifyVector = FALSE))
  }))

  if (!resolve_taxa) return(core)

  if (verbose) message("Resolving ranked taxonomy from taxon cache...")
  tax_map <- resolve_taxonomy(con, core$taxon_id, request_fn = request_fn, verbose = verbose)

  out <- dplyr::left_join(core, tax_map, by = "taxon_id")

  # Guarantee every export rank column exists even if no observation
  # resolved that rank (keeps downstream select()/rename() from erroring).
  for (col in TAXON_RANK_COLUMNS) {
    if (!col %in% names(out)) out[[col]] <- NA_character_
  }

  if (cacheable) {
    attr(out, "signature") <- sig
    .export_cache_env$sig <- sig; .export_cache_env$frame <- out
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    tryCatch(saveRDS(out, cache_path),
             error = function(e) warning("could not write export cache: ", conditionMessage(e)))
  }
  out
}
