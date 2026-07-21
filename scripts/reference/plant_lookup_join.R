# =============================================================
# reference/plant_lookup_join.R
# beescabr -- attach a flower's plant taxon_id + in-park flag from the lookup
# Created: 2026-07-21
#
# attach_flower_ids(df): given a cleaned bee/specimen table with a flower_visited
# column (a plant name), joins it to cabr_plant_taxonomy_lookup.csv by
# scientific_name and adds:
#   flower_taxon_id  -- the plant's iNat taxon_id
#   flower_in_park   -- TRUE/FALSE: is that plant in the park (from the lookup)
# Unmatched names (e.g. a free-text iNat entry with no canonical row) -> NA.
# The lookup is a stable reference; if it doesn't exist yet the columns are added
# as NA so the schema is stable either way.
# =============================================================

suppressWarnings(suppressMessages({library(dplyr); library(readr)}))

local({
  sdir <- "scripts"
  for (cand in c("scripts", "../scripts", "../../scripts", "../../../scripts"))
    if (dir.exists(cand)) { sdir <- cand; break }
  if (!exists("PATHS")) source(file.path(sdir, "config.R"))
})

# roll a plant name up to AT MOST species so a subspecies/variety flower matches
# the lookup's species row (the lookup is genus+species only): a trinomial+ folds to
# its binomial; a binomial, genus, or "Genus sp." is left alone. NA/blank -> NA.
.aff_roll_to_species <- function(name) {
  x <- trimws(gsub("\\s+", " ", as.character(name)))
  vapply(x, function(s) {
    if (is.na(s) || s == "") return(NA_character_)
    w <- strsplit(s, " ", fixed = TRUE)[[1]]
    if (length(w) >= 3 && !(tolower(w[2]) %in% c("sp", "sp.", "spp", "spp.", "x", "×"))) paste(w[1], w[2]) else s
  }, character(1), USE.NAMES = FALSE)
}

attach_flower_ids <- function(df, lookup_path = NULL) {
  if (is.null(lookup_path))
    lookup_path <- if (exists("PATHS") && !is.null(PATHS$plant_taxonomy_lookup)) PATHS$plant_taxonomy_lookup
                   else "data/reference/cabr_plant_taxonomy_lookup.csv"
  if (!"flower_taxon_id" %in% names(df)) df$flower_taxon_id <- NA_character_
  if (!"flower_in_park"  %in% names(df)) df$flower_in_park  <- NA
  if (is.null(lookup_path) || !file.exists(lookup_path) || !"flower_visited" %in% names(df)) return(df)
  lk <- suppressWarnings(suppressMessages(read_csv(lookup_path, show_col_types = FALSE, col_types = cols(.default = "c"))))
  if (!all(c("scientific_name", "taxon_id", "in_cabr_park_at_all") %in% names(lk))) return(df)
  # species rows first, then genus, so a name that is both keys to the finer row
  ord <- order(match(tolower(lk$rank %||% rep("", nrow(lk))), c("species", "genus")), na.last = TRUE)
  lk  <- lk[ord, , drop = FALSE]
  lkn <- tolower(trimws(lk$scientific_name))
  fv  <- tolower(trimws(as.character(df$flower_visited)))
  m   <- match(fv, lkn)                                          # exact name
  mr  <- match(tolower(.aff_roll_to_species(df$flower_visited)), lkn)   # subspecies -> its species row
  m   <- ifelse(is.na(m), mr, m)
  df$flower_taxon_id <- lk$taxon_id[m]
  df$flower_in_park  <- ifelse(is.na(m), NA, toupper(trimws(lk$in_cabr_park_at_all[m])) == "TRUE")
  df
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
