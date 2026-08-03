# =============================================================
# reference/enrich_lookups.R
# beescabr -- SHARED enrichment resolvers, run at DATA-CLEANING time (not analysis).
#
# One place that turns online reference lookups into cached, bakeable columns:
#   * IUCN Red List category  per bee species   (rredlist, needs a free token)
#   * common name             per plant genus   (public iNaturalist taxa API, no token)
#
# The clean scripts + checklist builders call the enrich_*() helpers to BAKE these
# straight into their output tables, so the analysis layer just reads columns and never
# touches the network. Both resolvers are:
#   * INCREMENTAL -- only species/genera missing from the cache are fetched (fast reruns);
#     pass force = TRUE to re-check everything (the standalone refresh_* tools do this).
#   * OFFLINE-SAFE -- no token / no internet -> the cache is used as-is and NEVER clobbered;
#     unresolved rows fall back to "NE" / the Latin genus, so cleaning always completes.
#
# Caches (also the single fetch ledger, shared across the two bee tables):
#   data/checklists/iucn/iucn_status.csv           (scientific_name, iucn_code, ...)
#   data/checklists/plants/plant_genus_common.csv  (genus, common_name, source, ...)
#
# Depends on: dplyr, stringr; httr2 (common names); rredlist (IUCN, optional). + config.R.
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr) })
if (!exists("PATHS")) source("scripts/config.R")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a)) ||
                            (is.character(a) && length(a) == 1 && !nzchar(a))) b else a

# =====================================================================================
# IUCN Red List status  (per bee species)
# =====================================================================================
IUCN_DIR        <- "data/checklists/iucn"
IUCN_CACHE_FILE <- file.path(IUCN_DIR, "iucn_status.csv")
IUCN_SECRET     <- "data/secrets/iucn_api.env"          # gitignored -- paste token here
IUCN_CODE_NAME  <- c(EX = "Extinct", EW = "Extinct in the Wild", RE = "Regionally Extinct",
                     CR = "Critically Endangered", EN = "Endangered", VU = "Vulnerable",
                     NT = "Near Threatened", LC = "Least Concern", DD = "Data Deficient",
                     NE = "Not Evaluated", LR = "Lower Risk")
# a few species IUCN assesses under a different accepted name (synonyms)
IUCN_SYNONYM    <- c("Bombus sonorus"      = "Bombus pensylvanicus",
                     "Bombus californicus" = "Bombus fervidus")
.iucn_name_of <- function(code) { n <- unname(IUCN_CODE_NAME[toupper(code)]); ifelse(is.na(n), code, n) }

.iucn_token <- function() {
  k <- Sys.getenv("IUCN_REDLIST_KEY")
  if (nzchar(k)) return(k)
  if (!file.exists(IUCN_SECRET)) return("")
  ln <- readLines(IUCN_SECRET, warn = FALSE); ln <- ln[grepl("^\\s*IUCN_REDLIST_KEY\\s*=", ln)]
  if (!length(ln)) return("")
  trimws(gsub('^["\']|["\']$', "", trimws(sub("^\\s*IUCN_REDLIST_KEY\\s*=\\s*", "", ln[length(ln)]))))
}

.iucn_read_cache <- function() {
  if (!file.exists(IUCN_CACHE_FILE))
    return(data.frame(scientific_name = character(0), iucn_code = character(0),
                      iucn_category = character(0), assessment_year = character(0),
                      source = character(0), retrieved_on = character(0), stringsAsFactors = FALSE))
  # colClasses="character" so a numeric-looking assessment_year isn't read back as <integer>
  # (that mismatched the fetched rows' type and broke bind_rows during an incremental refresh)
  d <- read.csv(IUCN_CACHE_FILE, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  for (c in c("scientific_name","iucn_code","iucn_category","assessment_year","source","retrieved_on"))
    if (!c %in% names(d)) d[[c]] <- NA_character_
  d
}

# latest global category for one species (NE if not assessed / not found / offline)
.iucn_fetch_one <- function(binom, key) {
  q    <- if (binom %in% names(IUCN_SYNONYM)) unname(IUCN_SYNONYM[binom]) else binom
  res  <- tryCatch(rredlist::rl_species_latest(genus = word(q, 1), species = word(q, 2),
                                               key = key, parse = TRUE),
                   error = function(e) NULL)
  if (is.null(res)) return(list(code = "NE", year = NA_character_, note = ""))
  code <- tryCatch(res$red_list_category$code, error = function(e) NULL)
  year <- tryCatch(res$year_published %||% res$assessment_date, error = function(e) NA)
  code <- toupper(code %||% "NE")
  list(code = code, year = as.character(year %||% ""),
       note = if (q != binom && code != "NE") sprintf(" (as %s)", q) else "")
}

# Resolve IUCN status for a vector of "Genus species" names. Returns the FULL cache-backed
# table (data.frame). Incremental + offline-safe; writes the cache when it learns something.
resolve_iucn <- function(species, force = FALSE, verbose = TRUE) {
  species <- unique(str_squish(species))
  species <- species[!is.na(species) & grepl(" ", species)]          # species-rank binomials only
  cache   <- .iucn_read_cache()
  need    <- if (force) species else setdiff(species, cache$scientific_name)
  key     <- .iucn_token()
  can_net <- length(need) > 0 && nzchar(key) && requireNamespace("rredlist", quietly = TRUE)

  if (length(need) > 0 && !can_net && verbose)
    message(sprintf("  IUCN: %d species unresolved (no token / rredlist missing) -- using cache only, not fetching.",
                    length(need)))

  if (can_net) {
    if (verbose) message(sprintf("  IUCN: fetching %d species from the Red List (v4)...", length(need)))
    fetched <- lapply(seq_along(need), function(i) {
      r <- .iucn_fetch_one(need[i], key)
      if (verbose && i %% 10 == 0) message(sprintf("    ...%d/%d", i, length(need)))
      Sys.sleep(0.34)                                                # ~3 req/s, courteous
      data.frame(scientific_name = need[i], iucn_code = r$code,
                 iucn_category = paste0(.iucn_name_of(r$code), r$note),
                 assessment_year = as.character(r$year), source = "IUCN Red List API v4 (rredlist)",
                 retrieved_on = as.character(Sys.Date()), stringsAsFactors = FALSE)
    })
    new <- do.call(rbind, fetched)
    old_assessed <- sum(toupper(cache$iucn_code) != "NE", na.rm = TRUE)
    # offline guard applies only to a FULL (force) re-pull: if EVERY species came back NE
    # yet the cache held real assessments, the API was almost certainly unreachable, so keep
    # the cache rather than wipe it. In INCREMENTAL mode we always write -- existing assessed
    # rows are preserved by the merge, and caching the genuinely-NE new species stops them
    # from being re-queried on every run (a single unassessed species used to re-fetch forever).
    if (force && sum(new$iucn_code != "NE") == 0 && old_assessed > 0) {
      if (verbose) message("  IUCN: full re-pull returned no assessments (offline?) -- keeping existing cache.")
    } else {
      merged <- if (force) new else bind_rows(cache[!cache$scientific_name %in% new$scientific_name, , drop = FALSE], new)
      merged <- merged[order(merged$scientific_name), , drop = FALSE]
      dir.create(IUCN_DIR, recursive = TRUE, showWarnings = FALSE)
      write.csv(merged, IUCN_CACHE_FILE, row.names = FALSE)
      cache <- merged
      if (verbose) message(sprintf("  IUCN: cache now holds %d species.", nrow(cache)))
    }
  }
  cache
}

# Add iucn_code / iucn_category / assessment_year columns to a cleaned bee table.
# Only species/subspecies rows are looked up; coarser rows get "NE". Never errors out the
# caller: any failure leaves the table unchanged-but-columned (all NE).
enrich_iucn_columns <- function(df, species_col = "scientific_name", rank_col = "taxon_rank") {
  if (!species_col %in% names(df)) return(df)
  sp_rows <- if (rank_col %in% names(df)) tolower(str_squish(df[[rank_col]])) %in% c("species", "subspecies")
             else rep(TRUE, nrow(df))
  binoms  <- ifelse(sp_rows, str_squish(df[[species_col]]), NA_character_)
  tab <- tryCatch(resolve_iucn(binoms[!is.na(binoms)]),
                  error = function(e) { message("  !! IUCN enrichment skipped: ", conditionMessage(e)); NULL })
  code_map <- if (is.null(tab)) character(0) else setNames(toupper(tab$iucn_code), tab$scientific_name)
  cat_map  <- if (is.null(tab)) character(0) else setNames(tab$iucn_category,        tab$scientific_name)
  yr_map   <- if (is.null(tab)) character(0) else setNames(tab$assessment_year,      tab$scientific_name)
  df$iucn_code       <- ifelse(is.na(binoms), "NE", unname(code_map[binoms] %||% NA)); df$iucn_code[is.na(df$iucn_code)] <- "NE"
  df$iucn_category   <- ifelse(is.na(binoms), "Not Evaluated", unname(cat_map[binoms]));  df$iucn_category[is.na(df$iucn_category)] <- "Not Evaluated"
  df$iucn_assessment_year <- ifelse(is.na(binoms), NA_character_, unname(yr_map[binoms]))
  df
}

# =====================================================================================
# Plant-genus common names  (public iNaturalist taxa API, no token)
# =====================================================================================
PGC_DIR        <- "data/checklists/plants"
PGC_CACHE_FILE <- file.path(PGC_DIR, "plant_genus_common.csv")

.pgc_read_cache <- function() {
  if (!file.exists(PGC_CACHE_FILE))
    return(data.frame(genus = character(0), common_name = character(0),
                      source = character(0), retrieved_on = character(0), stringsAsFactors = FALSE))
  read.csv(PGC_CACHE_FILE, stringsAsFactors = FALSE, check.names = FALSE)
}

# local seed: names the in-park plant taxonomy files already carry (via plant_names.R)
.pgc_local_map <- function() {
  if (!exists(".plant_common_map")) {
    pn <- "scripts/analysis/plant_names.R"
    if (file.exists(pn)) suppressWarnings(try(source(pn), silent = TRUE))
  }
  if (exists(".plant_common_map")) tryCatch(.plant_common_map(), error = function(e) character(0)) else character(0)
}

.pgc_api_reachable <- function() {
  isTRUE(tryCatch({
    httr2::request(paste0(INAT_BASE_URL, "taxa")) |>
      httr2::req_url_query(q = "Salvia", rank = "genus", per_page = 1) |>
      httr2::req_user_agent(INAT_USER_AGENT) |> httr2::req_timeout(12) |>
      httr2::req_error(is_error = function(resp) FALSE) |> httr2::req_perform()
    TRUE
  }, error = function(e) FALSE))
}

.pgc_fetch_one <- function(g) {
  r <- tryCatch(
    httr2::request(paste0(INAT_BASE_URL, "taxa")) |>
      httr2::req_url_query(q = g, rank = "genus", per_page = 5) |>
      httr2::req_user_agent(INAT_USER_AGENT) |> httr2::req_timeout(30) |>
      httr2::req_retry(max_tries = 4,
                       is_transient = function(resp) httr2::resp_status(resp) %in% c(429L,500L,502L,503L,504L),
                       backoff = function(i) min(30, 3 * 2^(i - 1))) |>
      httr2::req_perform() |> httr2::resp_body_json(),
    error = function(e) NULL)
  if (is.null(r) || is.null(r$results)) return(NA_character_)
  for (t in r$results) if (identical(t$name %||% "", g) && identical(t$rank %||% "", "genus"))
    return(t$preferred_common_name %||% NA_character_)
  NA_character_
}

# Resolve a common name for each plant genus. Returns a named vector (genus -> common name,
# Title-cased; NA where none). Incremental + offline-safe (never shrinks a good cache).
resolve_plant_common <- function(genera, force = FALSE, verbose = TRUE) {
  genera  <- unique(str_squish(genera)); genera <- genera[!is.na(genera) & genera != ""]
  local   <- .pgc_local_map()                                   # RAW-spelling local seed
  cache   <- .pgc_read_cache()
  cached_map <- setNames(str_squish(cache$common_name), str_squish(cache$genus))
  cached_map <- cached_map[!is.na(cached_map) & nzchar(cached_map)]
  have    <- union(names(cached_map), names(local)[!is.na(local) & nzchar(local %||% "")])
  need    <- if (force) genera else setdiff(genera, have)

  fetched <- character(0)
  if (length(need) > 0 && requireNamespace("httr2", quietly = TRUE) && .pgc_api_reachable()) {
    if (verbose) message(sprintf("  plant common names: fetching %d genera from iNaturalist...", length(need)))
    for (i in seq_along(need)) {
      cn <- .pgc_fetch_one(need[i]); if (!is.na(cn) && nzchar(cn)) fetched[need[i]] <- cn
      if (verbose && i %% 10 == 0) message(sprintf("    ...%d/%d", i, length(need)))
      Sys.sleep(0.5)
    }
    if (verbose) message(sprintf("  plant common names: fetched %d new.", length(fetched)))
  } else if (length(need) > 0 && verbose) {
    message(sprintf("  plant common names: %d genera unresolved (offline/blocked) -- using cache + local seed.", length(need)))
  }

  # assemble & persist the cache: prior cache + local seed + newly fetched (fetched wins on refresh)
  acc <- list()
  for (g in names(cached_map)) acc[[g]] <- list(v = unname(cached_map[g]), s = "cache (prior fetch)")
  for (g in names(local)) if (!is.na(local[g]) && nzchar(local[g] %||% "")) acc[[g]] <- list(v = unname(local[g]), s = "in-park plant taxonomy")
  for (g in names(fetched)) acc[[g]] <- list(v = unname(fetched[g]), s = "iNaturalist taxa API")
  if (length(acc) > 0) {
    out <- data.frame(genus = names(acc),
                      common_name = vapply(acc, function(x) x$v, character(1)),
                      source = vapply(acc, function(x) x$s, character(1)),
                      retrieved_on = as.character(Sys.Date()), stringsAsFactors = FALSE)
    out <- out[order(out$genus), , drop = FALSE]
    if (nrow(out) >= nrow(cache)) {                              # never shrink a good cache
      dir.create(PGC_DIR, recursive = TRUE, showWarnings = FALSE)
      write.csv(out, PGC_CACHE_FILE, row.names = FALSE)
    }
  }

  # final map for the requested genera (Title-cased), local + cache + fetched
  full <- c(setNames(unname(local[!is.na(local)]), names(local)[!is.na(local)]),
            cached_map, fetched)
  full <- full[!duplicated(names(full), fromLast = TRUE)]        # fetched/cache override earlier
  tc <- function(x) str_to_title(str_squish(x))
  setNames(ifelse(genera %in% names(full), tc(full[genera]), NA_character_), genera)
}

# Add a plant_genus_common column (genus-level common name) to a cleaned plant table.
enrich_plant_common_column <- function(df, genus_col = "plant_genus") {
  if (!genus_col %in% names(df)) { gc <- if ("genus" %in% names(df)) "genus" else return(df); genus_col <- gc }
  m <- tryCatch(resolve_plant_common(df[[genus_col]]),
                error = function(e) { message("  !! common-name enrichment skipped: ", conditionMessage(e)); NULL })
  df$plant_genus_common <- if (is.null(m)) NA_character_ else unname(m[str_squish(df[[genus_col]])])
  df
}
