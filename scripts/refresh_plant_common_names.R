# =============================================================
# scripts/refresh_plant_common_names.R
# beescabr -- pull the common name of every PLANT GENUS the bees were recorded on, and
# cache it, so the figures + field guides can show reader-friendly common names (e.g.
# "California Poppies (Eschscholzia)") instead of the bare Latin genus.
#
# This is a DATA-REFRESH step, NOT part of run_all_analysis.R (which stays offline). It
# fills the genera the two local plant-taxonomy files don't already name, by asking the
# public iNaturalist taxa API for each genus's preferred English common name. It needs
# internet but NO token (common names are public) -- so it does NOT trigger the OAuth
# sign-in used elsewhere; it is a plain, courteous GET.
#
#   From the repo root:  Rscript scripts/refresh_plant_common_names.R
#
# Writes data/checklists/plants/plant_genus_common.csv (genus, common_name, source,
# retrieved_on). scripts/analysis/plant_names.R reads it automatically (its entries
# override the local seed). Offline-safe: if the API is unreachable it still (re)writes
# the local-seed cache and keeps any previously fetched names.
#
# Depends on: httr2, dplyr, stringr (+ config.R and the plant_names.R seed helpers).
# =============================================================

suppressPackageStartupMessages({ library(httr2); library(dplyr); library(stringr) })
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("plant_common_name")) source("scripts/analysis/plant_names.R")   # reuse the local-seed reader

OUT_DIR    <- "data/checklists/plants"; dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
CACHE_FILE <- file.path(OUT_DIR, "plant_genus_common.csv")
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 ||
                            (length(a) == 1 && is.na(a))) b else a

# ---- the plant genera we actually need labels for: every plant_genus on a bee record ----
grab_genera <- function(path) {
  if (!file.exists(path)) return(character(0))
  d <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"plant_genus" %in% names(d)) return(character(0))
  g <- str_squish(d$plant_genus); unique(g[!is.na(g) & g != ""])
}
genera <- sort(unique(c(grab_genera(PATHS$specimen_clean),
                        grab_genera(PATHS$inat_clean),
                        grab_genera(PATHS$inat_plant_clean))))
message(sprintf("Plant genera needing a common-name label: %d", length(genera)))

# ---- local seed: names the two in-park taxonomy files already carry --------------------
# Use the module's merged local map (RAW source spelling; the module title-cases on read).
# The cache file may not exist yet, so this reflects only the local taxonomy files here.
.local_map <- tryCatch(.plant_common_map(), error = function(e) character(0))
seed_raw <- setNames(rep(NA_character_, length(genera)), genera)
for (g in genera) if (!is.na(.local_map[g]) && nzchar(.local_map[g] %||% "")) seed_raw[g] <- unname(.local_map[g])
have_local <- names(seed_raw)[!is.na(seed_raw)]
message(sprintf("  named locally (no fetch needed): %d", length(have_local)))

# ---- previously fetched names (don't lose them on a later offline run) -----------------
prev <- if (file.exists(CACHE_FILE))
  read.csv(CACHE_FILE, stringsAsFactors = FALSE, check.names = FALSE) else
  data.frame(genus = character(0), common_name = character(0),
             source = character(0), retrieved_on = character(0))
prev_map <- setNames(str_squish(prev$common_name), str_squish(prev$genus))

# ---- fetch a genus's preferred common name from the PUBLIC iNat taxa API ----------------
fetch_common <- function(g) {
  r <- tryCatch(
    request(paste0(INAT_BASE_URL, "taxa")) |>
      req_url_query(q = g, rank = "genus", per_page = 5) |>
      req_user_agent(INAT_USER_AGENT) |>
      req_timeout(30) |>
      req_retry(max_tries = 4,
                is_transient = function(resp) resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L),
                backoff = function(i) min(30, 3 * 2^(i - 1))) |>
      req_perform() |> resp_body_json(),
    error = function(e) NULL)
  if (is.null(r) || is.null(r$results)) return(NA_character_)
  for (t in r$results) {
    nm <- t$name %||% ""; rk <- t$rank %||% ""
    if (identical(nm, g) && identical(rk, "genus"))
      return(t$preferred_common_name %||% NA_character_)
  }
  NA_character_
}

# only fetch genera we have NO local name AND no cached name for
need <- setdiff(genera, c(have_local, names(prev_map)[!is.na(prev_map) & prev_map != ""]))

# Preflight: one quick probe so an offline/blocked run skips the network entirely and
# just (re)writes the local-seed cache, instead of retrying every genus for minutes.
api_reachable <- function() {
  ok <- tryCatch({
    request(paste0(INAT_BASE_URL, "taxa")) |>
      req_url_query(q = "Salvia", rank = "genus", per_page = 1) |>
      req_user_agent(INAT_USER_AGENT) |> req_timeout(12) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    TRUE
  }, error = function(e) FALSE)
  isTRUE(ok)
}
if (length(need) > 0 && !api_reachable()) {
  message("  iNaturalist API not reachable -- writing local-seed cache only (run again online to fill the rest).")
  need <- character(0)
}
message(sprintf("  querying iNaturalist for %d genera without a local/cached name...", length(need)))
fetched <- character(0)
for (i in seq_along(need)) {
  g  <- need[i]
  cn <- fetch_common(g)
  if (!is.na(cn) && nzchar(cn)) fetched[g] <- cn
  if (i %% 10 == 0) message(sprintf("    ...%d/%d", i, length(need)))
  Sys.sleep(0.5)                                            # courteous to the public API
}
message(sprintf("  fetched %d new common name(s) from iNaturalist.", length(fetched)))

# ---- assemble the cache: local seed + previously cached + newly fetched ----------------
final <- character(0)
put <- function(m, g, v, src) { if (!is.na(v) && nzchar(v)) { m$common[[g]] <- v; m$src[[g]] <- src }; m }
acc <- list(common = list(), src = list())
for (g in names(prev_map))   acc <- put(acc, g, unname(prev_map[g]),   "cache (prior fetch)")
for (g in have_local)        acc <- put(acc, g, unname(seed_raw[g]),   "in-park plant taxonomy")
for (g in names(fetched))    acc <- put(acc, g, unname(fetched[g]),    "iNaturalist taxa API")

genus_v <- names(acc$common)
out <- data.frame(
  genus        = genus_v,
  common_name  = unlist(acc$common[genus_v]),
  source       = unlist(acc$src[genus_v]),
  retrieved_on = as.character(Sys.Date()),
  stringsAsFactors = FALSE) |>
  arrange(genus)

# Don't shrink a good cache on an offline run: only write if we have at least as many
# named genera as before (local seed alone guarantees this on the very first run).
if (nrow(out) == 0 && nrow(prev) > 0) {
  message("No names resolved (offline?) -- keeping the existing cache untouched.")
} else {
  write.csv(out, CACHE_FILE, row.names = FALSE)
  n_missing <- length(setdiff(genera, out$genus))
  message(sprintf("Wrote %s  (%d genera named; %d still unnamed -> shown as Latin only)",
                  CACHE_FILE, nrow(out), n_missing))
  if (n_missing > 0)
    message("  Unnamed (no common name in iNaturalist either): ",
            paste(setdiff(genera, out$genus), collapse = ", "))
}
