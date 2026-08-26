# =============================================================
# api/inat_cache.R
# beescabr pipeline -- cache-first taxon access + taxonomy resolution
# Created: 2026-07-13 (API + DuckDB rewrite)
#
# The only module that knows about BOTH the API transport and the DuckDB
# taxon cache. Everything above it (checklist build, export read) asks for
# a taxon or a taxonomy map and neither knows nor cares whether it came
# from the network or the cache. Fetches on miss, writes through, returns.
#
# resolve_taxonomy() unifies what used to be two separate things in the old
# code: STEP 1's ranked-name hierarchy AND STEP 4's subgenus/complex fetch.
# Both now come from one cached /taxa/{id} call per taxon via parse_taxon_ranks().
#
# Depends on: api/inat_http.R, api/inat_flatten.R, db/taxon_store.R.
# =============================================================

if (!exists("inat_fetch_taxon_by_id")) source("scripts/inat_observations/engine/api/inat_http.R")
if (!exists("parse_taxon_ranks"))       source("scripts/inat_observations/engine/api/inat_flatten.R")
if (!exists("taxon_cache_get"))          source("scripts/inat_observations/engine/db/taxon_store.R")

# ------------------------------------------------------------
# get_taxon_by_id(): return one taxon object (nested list), cache-first.
# ------------------------------------------------------------
get_taxon_by_id <- function(con, id, request_fn = inat_request, verbose = FALSE) {
  key <- taxon_cache_key_id(id)
  hit <- taxon_cache_get(con, key)
  if (!is.null(hit)) {
    if (verbose) message("  cache hit: taxon ", id)
    return(hit)
  }
  results <- inat_fetch_taxon_by_id(id, request_fn = request_fn)
  taxon <- if (length(results) > 0) results[[1]] else NULL
  if (!is.null(taxon)) taxon_cache_put(con, key, taxon$id %||% id, taxon)
  if (verbose) message("  fetched taxon ", id)
  taxon
}

# ------------------------------------------------------------
# get_taxa_by_name(): return the list of candidate taxa for a name search,
# cache-first. Opportunistically caches each candidate by its own id too,
# so a later get_taxon_by_id() for one of them is a hit.
# ------------------------------------------------------------
get_taxa_by_name <- function(con, name, request_fn = inat_request, verbose = FALSE) {
  key <- taxon_cache_key_name(name)
  hit <- taxon_cache_get(con, key)
  if (!is.null(hit)) {
    if (verbose) message("  cache hit: name '", name, "'")
    return(hit)
  }
  results <- inat_fetch_taxa_by_name(name, request_fn = request_fn)
  taxon_cache_put(con, key, NA_integer_, results)
  # Cache a candidate by id ONLY if it carries ancestors and isn't already
  # cached. The /taxa?q= search endpoint omits ancestors, so caching those by id
  # would clobber the full ancestor-bearing objects the observation flatten
  # (resolve_taxonomy -> parse_taxon_ranks) needs, blanking out genus/family.
  for (t in results) {
    if (!is.null(t$id) && !is.null(t$ancestors) && length(t$ancestors) > 0 &&
        is.null(taxon_cache_get(con, taxon_cache_key_id(t$id)))) {
      taxon_cache_put(con, taxon_cache_key_id(t$id), t$id, t)
    }
  }
  if (verbose) message("  fetched name search '", name, "' (", length(results), " results)")
  results
}

# iNat /taxa/{ids} accepts at most this many ids per request.
TAXA_BATCH_SIZE <- 30L

# ------------------------------------------------------------
# prefetch_taxa(): fill the taxon cache for a set of ids using BATCHED
# requests (up to TAXA_BATCH_SIZE ids each) with a throttle between batches.
# Only ids not already cached are fetched. This replaces the old one-request-
# per-taxon behavior that was getting rate limited -- ~30x fewer requests,
# plus a courteous pause between them. Returns the number of taxa fetched.
# ------------------------------------------------------------
prefetch_taxa <- function(con, taxon_ids, request_fn = inat_request,
                          throttle = INAT_THROTTLE_SEC, sleep_fn = Sys.sleep,
                          verbose = TRUE) {
  taxon_ids <- unique(taxon_ids[!is.na(taxon_ids)])
  uncached <- taxon_ids[vapply(taxon_ids,
    function(id) is.null(taxon_cache_get(con, taxon_cache_key_id(id))), logical(1))]
  if (length(uncached) == 0) return(invisible(0L))

  batches <- split(uncached, ceiling(seq_along(uncached) / TAXA_BATCH_SIZE))
  if (verbose) message(sprintf("  taxonomy: fetching %d uncached taxa in %d batch(es) of <=%d",
                               length(uncached), length(batches), TAXA_BATCH_SIZE))
  n <- 0L
  for (i in seq_along(batches)) {
    results <- inat_fetch_taxa_by_ids(batches[[i]], request_fn = request_fn)
    for (t in results) if (!is.null(t$id)) taxon_cache_put(con, taxon_cache_key_id(t$id), t$id, t)
    n <- n + length(results)
    if (verbose && (i %% 5 == 0 || i == length(batches)))
      message(sprintf("    batch %d/%d (%d taxa cached)", i, length(batches), n))
    if (i < length(batches) && !is.null(throttle) && throttle > 0) sleep_fn(throttle)
  }
  invisible(n)
}

# ------------------------------------------------------------
# resolve_taxonomy(): build the taxon_id -> ranked-name (+ subgenus /
# complex / complex_taxon_id) map for a set of taxon ids. It first BATCH-
# prefetches every uncached taxon (few requests), then reads each taxon
# straight from the cache -- so this makes NO per-taxon API calls. A taxon
# the batch didn't return (inactive/invalid id) is left with NA ranks rather
# than triggering an extra single request.
# ------------------------------------------------------------
resolve_taxonomy <- function(con, taxon_ids, request_fn = inat_request,
                             throttle = INAT_THROTTLE_SEC, verbose = TRUE) {
  taxon_ids <- unique(taxon_ids[!is.na(taxon_ids)])
  if (length(taxon_ids) == 0) {
    return(parse_taxon_ranks(list(id = NA_integer_))[0, ])
  }
  prefetch_taxa(con, taxon_ids, request_fn = request_fn, throttle = throttle, verbose = verbose)

  rows <- vector("list", length(taxon_ids))
  for (i in seq_along(taxon_ids)) {
    taxon <- taxon_cache_get(con, taxon_cache_key_id(taxon_ids[[i]]))  # cache-only
    rows[[i]] <- if (is.null(taxon)) parse_taxon_ranks(list(id = taxon_ids[[i]]))
                 else parse_taxon_ranks(taxon)
  }
  dplyr::bind_rows(rows)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
