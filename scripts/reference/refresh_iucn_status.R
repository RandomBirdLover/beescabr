# =============================================================
# scripts/reference/refresh_iucn_status.R
# beescabr -- "force a full IUCN refresh" tool. Runs standalone, OR inside the pipeline
# via BEESCABR_REFRESH=1 (run_pipeline.R sources it as an optional online pre-step).
#
# The IUCN Red List category for every bee species is now baked into the cleaned bee tables
# AT DATA-CLEANING TIME (specimen_bee_clean.R + inat_bee_clean.R call enrich_iucn_columns()
# from scripts/reference/enrich_lookups.R, which fetches once, caches, and is offline-safe).
# So you normally do NOT need to run this -- a normal pipeline run keeps IUCN current for any
# NEW species incrementally.
#
# Run this only to RE-CHECK species already in the cache (IUCN publishes a couple of updates a
# year, so an existing NE / LC could change). It forces a full re-query of every species and
# rewrites data/checklists/iucn/iucn_status.csv. Needs internet + a free token (see
# data/secrets/iucn_api.env or the IUCN_REDLIST_KEY env var; free at https://api.iucnredlist.org).
#
#   From the repo root:  Rscript scripts/reference/refresh_iucn_status.R
#   Or inside the pipeline:  BEESCABR_REFRESH=1 Rscript scripts/run_pipeline.R
#
# Depends on: dplyr, stringr, rredlist (+ config.R + reference/enrich_lookups.R).
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr) })
if (!exists("PATHS"))         source("scripts/config.R")
if (!exists("resolve_iucn"))  source("scripts/reference/enrich_lookups.R")

# species universe: every species/subspecies binomial in the two cleaned bee tables
grab <- function(path) {
  if (!file.exists(path)) return(character(0))
  d <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("scientific_name", "taxon_rank") %in% names(d))) return(character(0))
  sp <- str_squish(d$scientific_name[tolower(str_squish(d$taxon_rank)) %in% c("species", "subspecies")])
  unique(sp[!is.na(sp) & grepl(" ", sp)])
}
species <- sort(unique(c(grab(PATHS$specimen_clean), grab(PATHS$inat_clean))))
message(sprintf("Forcing a full IUCN re-check of %d bee species...", length(species)))

tab <- resolve_iucn(species, force = TRUE)
thr <- tab$scientific_name[toupper(tab$iucn_code) %in% c("CR", "EN", "VU", "NT")]
message(sprintf("Done: %d species in the cache; %d threatened/near-threatened.", nrow(tab), length(thr)))
if (length(thr)) message("  Flagged: ", paste(sort(thr), collapse = ", "))

# When run on its own, rebuild the species field guide so the refreshed column shows up.
FIELD_GUIDE <- "scripts/analysis/bee_field_guide.R"
if (!exists("RUNNING_ALL") && file.exists(FIELD_GUIDE)) {
  message("Rebuilding the field guide with the refreshed IUCN column...")
  source(FIELD_GUIDE)
}
