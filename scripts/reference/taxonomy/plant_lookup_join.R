# =============================================================
# reference/taxonomy/plant_lookup_join.R
# beescabr -- attach a flower's plant taxon_id + in-park flag from the lookup
# Created: 2026-07-21
#
# attach_flower_ids(df): given a cleaned bee/specimen table with a flower_visited
# column (a plant name), joins it to cabr_plant_taxonomy_lookup_generated.csv by
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

# plant_name_parts(x): split a plant name into genus + FULL-BINOMIAL species. Vectorized.
#   "Encelia californica"         -> genus "Encelia",  species "Encelia californica"
#   "Isocoma menziesii sedoides"  -> genus "Isocoma",  species "Isocoma menziesii"  (rolled to binomial)
#   "Madia" / "Madia sp."         -> genus "Madia",    species NA  (no species named)
#   "Cactaceae" (family)          -> genus NA,         species NA  (not a genus)
plant_name_parts <- function(x, taxon_id = NULL, lookup_path = NULL) {
  x  <- trimws(gsub("\\s+", " ", as.character(x)))
  gs <- lapply(x, function(s) {
    if (is.na(s) || s == "") return(c(NA_character_, NA_character_))
    w <- strsplit(s, " ", fixed = TRUE)[[1]]
    if (grepl("aceae$", w[1], ignore.case = TRUE)) return(c(NA_character_, NA_character_))  # family, not a genus
    sp <- if (length(w) >= 2 && !(tolower(w[2]) %in% c("sp", "sp.", "spp", "spp.", "x", "×", "cf", "cf.", "aff", "aff.")))
            paste(w[1], w[2]) else NA_character_
    c(w[1], sp)
  })
  out <- list(plant_genus   = vapply(gs, `[`, character(1), 1),
              plant_species = vapply(gs, `[`, character(1), 2))
  # Word 1 of a name is only a genus if the reference table says so. "Angiospermae",
  # "Faboideae", "Madieae" and ten more are real taxa ABOVE genus; they do not end in
  # "aceae", so the test above misses them and they landed in a column called
  # plant_genus. The lookup records each at its true rank with a BLANK genus column.
  #
  # The test is that BLANK GENUS COLUMN, not rank == "genus". Rank would also blank
  # every species row, and would throw away section/subgenus names (Trachynia ->
  # Brachypodium, Cepa -> Allium) whose genus the lookup already knows.
  #
  # Joined on taxon_id, never on the name we just split (CLAUDE.md, Taxon identity).
  # Fails OPEN: an id that is blank, or absent from the lookup, keeps its answer --
  # a stale lookup must not silently blank real genera.
  if (!is.null(taxon_id) && length(taxon_id) == length(x)) {
    lp <- if (!is.null(lookup_path)) lookup_path
          else if (exists("PATHS")) PATHS$plant_taxonomy_lookup else NULL
    lk <- if (!is.null(lp) && file.exists(lp))
            tryCatch(suppressWarnings(suppressMessages(
              read_csv(lp, show_col_types = FALSE, col_types = cols(.default = "c")))),
              error = function(e) NULL) else NULL
    if (!is.null(lk) && nrow(lk) && all(c("taxon_id", "genus") %in% names(lk))) {
      id <- trimws(as.character(taxon_id)); id[id == "" | id == "NA"] <- NA_character_
      m  <- match(id, trimws(as.character(lk$taxon_id)))
      m[is.na(id)] <- NA_integer_                   # match(NA, x) finds NA -- never allow it
      above <- !is.na(m) & (is.na(lk$genus[m]) | trimws(lk$genus[m]) == "")
      out$plant_genus[above]   <- NA_character_
      out$plant_species[above] <- NA_character_
    }
  }
  out
}

#' Attach plant taxon ids to records carrying a flower name
#'
#' @param df Records with a flower-name column.
#' @param lookup_path The plant taxonomy lookup; defaults to the `PATHS` entry.
#' @return `df` with `flower_taxon_id`, `flower_in_park`, `plant_genus` and
#'   `plant_species` attached.
attach_flower_ids <- function(df, lookup_path = NULL) {
  if (is.null(lookup_path))
    lookup_path <- if (exists("PATHS") && !is.null(PATHS$plant_taxonomy_lookup)) PATHS$plant_taxonomy_lookup
                   else PATHS$plant_taxonomy_lookup
  if (!"flower_taxon_id" %in% names(df)) df$flower_taxon_id <- NA_character_
  if (!"flower_in_park"  %in% names(df)) df$flower_in_park  <- NA
  # plant_genus + full-binomial plant_species from the flower name (independent of the lookup)
  if ("flower_visited" %in% names(df)) {
    pp <- plant_name_parts(df$flower_visited); df$plant_genus <- pp$plant_genus; df$plant_species <- pp$plant_species
  } else {
    if (!"plant_genus"   %in% names(df)) df$plant_genus   <- NA_character_
    if (!"plant_species" %in% names(df)) df$plant_species <- NA_character_
  }
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
  # Re-split now that each flower has an id. The first call above ran before
  # flower_taxon_id existed, so the rank gate had nothing to join on.
  .pp <- plant_name_parts(df$flower_visited, df$flower_taxon_id, lookup_path)
  df$plant_genus <- .pp$plant_genus; df$plant_species <- .pp$plant_species
  df
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
