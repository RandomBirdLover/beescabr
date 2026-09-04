# =============================================================
# scripts/reference/refresh_plant_common_names.R
# beescabr  [iNaturalist REST API v1 -- see INAT_API_VERSION in config.R] -- "force a full plant common-name refresh" tool. Runs standalone, OR inside the
# pipeline via BEESCABR_REFRESH=1 (run_data_cleaning_pipeline.R sources it as an optional online pre-step).
#
# The genus-level common name for every plant a bee was recorded on is now baked into the
# cleaned plant table AT DATA-CLEANING TIME (inat_plant_clean.R calls enrich_plant_common_column()
# from scripts/reference/enrich_lookups.R, which fetches once from the public iNaturalist taxa
# API, caches, and is offline-safe). So you normally do NOT need to run this -- a normal pipeline
# run fills any NEW genus incrementally.
#
# Run this only to RE-CHECK genera already cached (e.g. iNat added a preferred common name that
# was missing before). It forces a full re-query and rewrites
# data/checklists/plants/plant_genus_common_generated.csv. Needs internet; NO token.
#
#   From the repo root:  Rscript scripts/reference/refresh_plant_common_names.R
#   Or inside the pipeline:  BEESCABR_REFRESH=1 Rscript scripts/run_data_cleaning_pipeline.R
#
# Depends on: httr2, dplyr, stringr (+ config.R + reference/enrich_lookups.R).
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr) })
if (!exists("PATHS"))                  source("scripts/config.R")
if (!exists("resolve_plant_common"))   source("scripts/reference/enrich_lookups.R")

# every plant genus on a bee/plant record across the cleaned tables
grab_genera <- function(path) {
  if (!file.exists(path)) return(character(0))
  d <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"plant_genus" %in% names(d)) return(character(0))
  g <- str_squish(d$plant_genus); unique(g[!is.na(g) & g != ""])
}
genera <- sort(unique(c(grab_genera(PATHS$specimen_clean),
                        grab_genera(PATHS$inat_clean),
                        grab_genera(PATHS$inat_plant_clean))))
message(sprintf("Forcing a full common-name re-check of %d plant genera...", length(genera)))

m <- resolve_plant_common(genera, force = TRUE)
named <- sum(!is.na(m)); n_missing <- sum(is.na(m))
message(sprintf("Done: %d genera named; %d still unnamed -> shown as Latin only.", named, n_missing))
if (n_missing > 0)
  message("  Unnamed: ", paste(names(m)[is.na(m)], collapse = ", "))
