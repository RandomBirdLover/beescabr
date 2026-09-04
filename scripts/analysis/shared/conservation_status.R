# =============================================================
# analysis/shared/conservation_status.R   (MODULE -- not a standalone figure)
#
# Single source of truth for bee conservation status across the pipeline. Reads the
# IUCN Red List cache (data/checklists/iucn/iucn_status_generated.csv, written by
# scripts/refresh_iucn_status.R) and exposes the lookups used by BOTH field guides AND
# the rare-species plant figure -- so all three flag the SAME species from the SAME
# live data, with no hardcoded threatened list anywhere downstream.
#
# If the cache is missing (refresh never run), it falls back to the known at-risk bumble
# bees so every consumer still renders. Loaded once at the top of run_all_analysis_pipeline.R and
# self-sourced by each consumer when run standalone. Defines functions only; writes nothing.
#
# Depends on: stringr.
# =============================================================

suppressPackageStartupMessages({ library(stringr) })

IUCN_CACHE_FILE   <- "data/checklists/iucn/iucn_status_generated.csv"
IUCN_THREAT_CODES <- c("CR", "EN", "VU")            # IUCN "threatened"
IUCN_ATRISK_CODES <- c("CR", "EN", "VU", "NT")      # threatened + near threatened

# used ONLY when the IUCN cache does not exist yet (refresh never run)
.iucn_fallback <- data.frame(
  scientific_name = c("Bombus crotchii", "Bombus sonorus", "Bombus californicus"),
  iucn_code       = c("EN", "VU", "VU"),
  iucn_category   = c("Endangered", "Vulnerable (as Bombus pensylvanicus)",
                      "Vulnerable (as Bombus fervidus)"),
  stringsAsFactors = FALSE)

# ---- the single reader (cache if present, else the fallback) -----------------
iucn_table <- function() {
  if (file.exists(IUCN_CACHE_FILE)) {
    d <- read.csv(IUCN_CACHE_FILE, stringsAsFactors = FALSE, check.names = FALSE)
    data.frame(scientific_name = str_squish(d$scientific_name),
               iucn_code       = toupper(str_squish(d$iucn_code)),
               iucn_category   = str_squish(d$iucn_category),
               stringsAsFactors = FALSE)
  } else {
    .iucn_fallback
  }
}
#' TRUE when the IUCN status cache has been fetched
#'
#' @return `TRUE` if the cache file exists. Callers use this to skip the IUCN
#'   columns entirely rather than render a table of "NE".
iucn_cache_exists <- function() file.exists(IUCN_CACHE_FILE)

# ---- vectorised lookups over a character vector of "Genus species" ------------
#' IUCN red-list code for each species
#'
#' @param sp Character vector of "Genus species" names.
#' @return The IUCN code (`LC`, `NT`, `VU`, `EN`, `CR`, `DD`), or `"NE"` for a
#'   species the red list has not evaluated. Same length as `sp`.
iucn_code_of <- function(sp) {
  out <- unname(setNames(iucn_table()$iucn_code, iucn_table()$scientific_name)[sp])
  out[is.na(out)] <- "NE"; out
}
#' IUCN red-list category, spelled out for a reader
#'
#' @param sp Character vector of "Genus species" names.
#' @return The category name ("Endangered"), or `"Not Evaluated"` when absent.
iucn_name_of <- function(sp) {
  out <- unname(setNames(iucn_table()$iucn_category, iucn_table()$scientific_name)[sp])
  out[is.na(out)] <- "Not Evaluated"; out
}
# IUCN flag label, e.g. "IUCN Endangered"  ("" = not at-risk)
conservation_label <- function(sp) {
  code <- iucn_code_of(sp); nm <- iucn_name_of(sp)
  ifelse(code %in% IUCN_ATRISK_CODES, paste0("IUCN ", nm), "")
}

# species in the given IUCN categories (default: at-risk CR/EN/VU/NT), as a data.frame
flagged_species <- function(codes = IUCN_ATRISK_CODES) {
  t <- iucn_table(); t[t$iucn_code %in% codes, , drop = FALSE]
}
