# =============================================================
# analysis/plant_names.R   (MODULE -- not a standalone figure)
#
# Single source of truth for PLANT-GENUS COMMON NAMES across the pipeline, so every
# figure and field-guide table can show the reader-friendly common name of a plant
# genus (what the science technician asked for) instead of the bare Latin genus.
#
# It resolves a scientific plant genus (e.g. "Eriogonum") to a label like
#   "Wild Buckwheats (Eriogonum)"
# -- common name first, Latin kept in parentheses as the scientific reference. A
# genus with no known common name anywhere falls back to just the Latin ("Sairocarpus").
#
# Sources, merged (later wins), so it works offline AND improves when refreshed:
#   1. data/reference/cabr_plant_taxonomy_lookup.csv  (in-park plant taxonomy; genus rows)
#   2. data/observations/reference/cabr_inat_plant_all_taxa.csv (all in-park plant taxa)
#   3. data/checklists/plants/plant_genus_common.csv  (the refreshable cache written by
#      scripts/refresh_plant_common_names.R -- fills genera the two local files miss, via
#      the iNaturalist taxa API). Authoritative: its entries override the local seed.
#
# Loaded once at the top of run_all_analysis.R and self-sourced by each consumer when
# run standalone. Defines functions only; writes nothing.
#
# Depends on: stringr (+ config.R for PATHS).
# =============================================================

suppressPackageStartupMessages({ library(stringr) })
if (!exists("PATHS")) source("scripts/config.R")

PLANT_COMMON_CACHE_FILE <- "data/checklists/plants/plant_genus_common.csv"

# --- read genus -> raw common-name pairs from one "taxonomy" style CSV ---------
# Handles either a `rank` or a `taxon_rank` column; keeps only genus rows that have a
# non-empty common name. Returns a named character vector (genus = common_name).
.plant_genus_pairs <- function(path) {
  if (is.null(path) || !file.exists(path)) return(character(0))
  d <- tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
                error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(character(0))
  rank <- if ("rank" %in% names(d)) d$rank else if ("taxon_rank" %in% names(d)) d$taxon_rank else rep(NA, nrow(d))
  gen  <- if ("genus" %in% names(d)) str_squish(d$genus) else rep(NA_character_, nrow(d))
  cn   <- if ("common_name" %in% names(d)) str_squish(d$common_name) else rep(NA_character_, nrow(d))
  keep <- tolower(str_squish(rank)) == "genus" & !is.na(gen) & gen != "" & !is.na(cn) & cn != ""
  keep[is.na(keep)] <- FALSE
  if (!any(keep)) return(character(0))
  v <- cn[keep]; names(v) <- gen[keep]
  v[!duplicated(names(v))]                                  # first row wins within a file
}

# --- the merged lookup: local seed first, refreshable cache overrides ----------
.plant_common_map <- function() {
  m <- character(0)
  add <- function(m, more) { for (g in names(more)) m[[g]] <- unname(more[g]); m }
  m <- add(m, .plant_genus_pairs(PATHS$plant_all_taxa))               # broad in-park taxa
  m <- add(m, .plant_genus_pairs(PATHS$plant_taxonomy_lookup))        # curated in-park lookup
  m <- add(m, .plant_genus_pairs(PLANT_COMMON_CACHE_FILE))            # refreshed cache (authoritative)
  unlist(m)
}

# --- readable Title Case (small connector words stay lowercase unless first) ---
.plant_title_case <- function(x) {
  small <- c("and","or","of","the","a","an","in","on","to","with","de")
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(s)) return(s)
    w <- strsplit(s, "\\s+")[[1]]
    w <- vapply(seq_along(w), function(i) {
      wi <- w[i]
      if (i > 1 && tolower(wi) %in% small) tolower(wi)
      else paste0(toupper(substr(wi, 1, 1)), substr(wi, 2, nchar(wi)))
    }, character(1))
    paste(w, collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

# ---- PUBLIC: common name of a plant genus (Title Case), NA if none known ------
# Vectorised over a character vector of scientific genera.
plant_common_name <- function(genus) {
  g   <- str_squish(as.character(genus))
  map <- .plant_common_map()
  raw <- unname(map[g])
  out <- ifelse(is.na(raw) | raw == "", NA_character_, .plant_title_case(raw))
  out[is.na(g) | g == ""] <- NA_character_
  out
}

# ---- PUBLIC: figure/table label: "Common Name (Genus)"; just "Genus" if none --
# `sci_wrap` optionally wraps the Latin part (e.g. "<i>%s</i>" for HTML italics).
plant_label <- function(genus, sci_wrap = "%s") {
  g  <- str_squish(as.character(genus))
  cn <- plant_common_name(g)
  sci <- sprintf(sci_wrap, g)
  ifelse(is.na(cn), sci, paste0(cn, " (", sci, ")"))
}

# ---- PUBLIC: convenience -- relabel a vector, returning a named lookup ---------
# handy for base-R plots: labels <- plant_label_map(rownames(M)); axis(.., labels[rn])
plant_label_map <- function(genus, sci_wrap = "%s") {
  g <- unique(str_squish(as.character(genus)))
  setNames(plant_label(g, sci_wrap), g)
}
