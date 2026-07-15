# =============================================================
# clean/verify.R
# beescabr pipeline -- "new-to-Holway" verification flagging (pure helpers)
# Created: 2026-07-13
#
# Verification here means: an observation whose genus / subgenus / complex /
# species / subspecies is NOT in the Holway reference is something we haven't
# seen before and needs a human to check the iNaturalist photo/ID. Holway is
# the trusted baseline; anything beyond it is flagged until you verify it.
#
# Holway carries no complex or subspecies concept, so any complex/subspecies
# value counts as "new" (needs a look). A taxon_id you've recorded in
# verified_taxa.csv is treated as already-checked and is not re-flagged.
#
# Pure + unit-tested in test-verify.R. Depends on: dplyr, stringr.
# =============================================================

library(dplyr)
library(stringr)

`%||%` <- function(a, b) if (is.null(a)) b else a

.clean_epithet <- function(x) {
  x |>
    str_remove("^CF\\s+") |> str_remove("^MSN\\s+") |> str_remove("^(aff\\.|AFF)\\s+") |>
    str_remove("(^|\\s+)sp\\.\\s*nov\\.$") |> str_trim()
}

# Sets of lowercased names Holway knows, by rank. species keys are "genus
# species"; subspecies keys "genus species subspecies". Holway packs the
# subspecies into species_raw ("cactorum basalis"), so we split on the space:
# first token = species epithet, remainder = subspecies epithet.
holway_name_sets <- function(holway_df) {
  g  <- tolower(trimws(holway_df$genus))
  sg <- tolower(str_remove_all(holway_df$subgenus %||% "", "[()]"))
  cleaned <- .clean_epithet(holway_df$species_raw %||% "")
  sp_ep <- tolower(str_trim(word(cleaned, 1)))
  ss_ep <- tolower(str_trim(word(cleaned, 2, -1)))
  has_sp <- !is.na(sp_ep) & sp_ep != ""
  has_ss <- !is.na(ss_ep) & ss_ep != ""
  list(
    genus      = unique(g[!is.na(g) & g != ""]),
    subgenus   = unique(sg[!is.na(sg) & sg != ""]),
    species    = unique(paste(g, sp_ep)[has_sp]),
    subspecies = unique(paste(g, sp_ep, ss_ep)[has_ss])
  )
}

# Add new_at_rank (comma-joined ranks new to Holway) and needs_verification
# (any new rank AND taxon_id not in verified_ids) to a data frame that has
# genus/subgenus/complex/species/subspecies (+ taxon_id).
flag_new_taxa <- function(df, sets, verified_ids = integer(0)) {
  col <- function(n) if (n %in% names(df)) df[[n]] else rep(NA_character_, nrow(df))
  g  <- col("genus"); sg <- col("subgenus"); cx <- col("complex")
  sp <- col("species"); ss <- col("subspecies")
  present <- function(x) !is.na(x) & x != ""

  ss_set <- sets$subspecies %||% character(0)
  new_genus   <- present(g)  & !(tolower(g)  %in% sets$genus)
  new_subg    <- present(sg) & !(tolower(str_remove_all(sg, "[()]")) %in% sets$subgenus)
  new_complex <- present(cx)                                   # Holway has no complexes
  new_species <- present(sp) & !(paste(tolower(g), tolower(.clean_epithet(sp))) %in% sets$species)
  # Holway DOES carry subspecies (packed into species_raw), so a subspecies is
  # only "new" if the genus+species+subspecies key isn't in the Holway set.
  new_subsp   <- present(ss) &
    !(paste(tolower(g), tolower(.clean_epithet(sp)), tolower(.clean_epithet(ss))) %in% ss_set)

  M <- cbind(genus = new_genus, subgenus = new_subg, complex = new_complex,
             species = new_species, subspecies = new_subsp)
  labels <- colnames(M)
  new_at_rank <- apply(M, 1, function(r) paste(labels[which(r)], collapse = ","))

  tid <- if ("taxon_id" %in% names(df)) suppressWarnings(as.integer(df$taxon_id)) else rep(NA_integer_, nrow(df))
  is_new <- rowSums(M, na.rm = TRUE) > 0
  already_verified <- !is.na(tid) & tid %in% verified_ids

  df$new_at_rank <- ifelse(new_at_rank == "", NA_character_, new_at_rank)
  df$needs_verification <- is_new & !already_verified
  df
}

# Read the user-maintained verified list; return integer taxon_ids (empty if
# the file doesn't exist yet). Any truthy `verified` value counts.
load_verified_taxa <- function(path) {
  if (!file.exists(path)) return(integer(0))
  v <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!"taxon_id" %in% names(v) || nrow(v) == 0) return(integer(0))
  keep <- if ("verified" %in% names(v)) {
    tolower(trimws(as.character(v$verified))) %in% c("y", "yes", "true", "1")
  } else rep(TRUE, nrow(v))
  unique(suppressWarnings(as.integer(v$taxon_id[keep])))
}
