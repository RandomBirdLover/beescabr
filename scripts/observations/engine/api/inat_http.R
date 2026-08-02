# =============================================================
# api/inat_http.R
# beescabr pipeline -- iNaturalist API transport
# Rewritten: 2026-07-13 (raw-body path for fast observation ingest)
#
# Single responsibility: talk HTTP to the iNat API. User-agent, timeout,
# retry/backoff on transient errors.
#
# Two body modes:
#   inat_request()       parses JSON -> R list. Used for taxa (small payloads).
#   inat_request_text()  returns the RAW response string, unparsed. Used for
#                        the observation ingest, where the ~10 MB/page payload
#                        is handed straight to DuckDB to parse in C++ -- R
#                        never builds nested lists from it (that jsonlite parse,
#                        plus re-serialization, was the ingest's real slowdown
#                        versus the Python script).
#
# Depends on: httr2. Sources config.R for defaults.
# =============================================================

library(httr2)

if (!exists("INAT_BASE_URL")) source("scripts/config.R")
# optional OAuth sign-in (adds true coords for records you're trusted with); no-op if unconfigured
if (!exists("inat_auth_enabled")) source("scripts/observations/engine/api/inat_auth.R")

# Build a configured request (UA + timeout + retry) for a path/query.
.inat_build_request <- function(path, query = list(),
                                base_url = INAT_BASE_URL,
                                user_agent = INAT_USER_AGENT,
                                timeout = 60, max_tries = 6) {
  url <- paste0(sub("/+$", "/", base_url), sub("^/+", "", path))
  req <- request(url) |>
    req_user_agent(user_agent) |>
    req_timeout(timeout) |>
    req_retry(
      max_tries    = max_tries,
      is_transient = function(resp) resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L),
      backoff      = function(i) min(60, 5 * 2^(i - 1))
    )
  query <- query[!vapply(query, function(v) is.null(v) || length(v) == 0, logical(1))]
  if (length(query) > 0) req <- req_url_query(req, !!!query)
  # authenticate when configured, so trusted/own records return true coordinates
  tok <- tryCatch(if (exists("inat_auth_enabled") && inat_auth_enabled()) inat_auth_token() else NULL,
                  error = function(e) NULL)
  if (!is.null(tok) && nzchar(tok)) req <- req_headers(req, Authorization = paste("Bearer", tok))
  req
}

# Parsed-JSON GET (taxa).
inat_request <- function(path, query = list(), perform = req_perform, ...) {
  resp <- perform(.inat_build_request(path, query, ...))
  resp_body_json(resp)
}

# Raw-string GET (observations ingest) -- no R-side JSON parsing.
inat_request_text <- function(path, query = list(), perform = req_perform, ...) {
  resp <- perform(.inat_build_request(path, query, ...))
  resp_body_string(resp)
}

# ------------------------------------------------------------
# Pure pagination-control helper (unit-tested). iNat pages stably by
# ascending id via id_above; a page shorter than per_page is the last one.
# ------------------------------------------------------------
.page_done <- function(n, per_page) n == 0L || n < per_page

# ------------------------------------------------------------
# Thin taxon endpoints used by the cache layer (parsed JSON).
# ------------------------------------------------------------
inat_fetch_taxon_by_id <- function(id, request_fn = inat_request) {
  resp <- request_fn(paste0("taxa/", id), query = list(all_names = "true"))
  resp$results
}

inat_fetch_taxa_by_name <- function(name, request_fn = inat_request) {
  resp <- request_fn("taxa", query = list(q = name, all_names = "true"))
  resp$results
}

# Fetch MANY taxa in one request. iNat's /taxa/{ids} accepts a comma-separated
# list (max 30 ids), returning full per-taxon detail incl. ancestors. This is
# the key to not hammering the API during taxonomy resolution.
inat_fetch_taxa_by_ids <- function(ids, request_fn = inat_request) {
  path <- paste0("taxa/", paste(ids, collapse = ","))
  resp <- request_fn(path, query = list(per_page = 30, all_names = "true"))
  resp$results
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
