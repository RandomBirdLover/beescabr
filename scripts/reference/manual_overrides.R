# =============================================================
# reference/manual_overrides.R
# beescabr -- user-curated taxon-id overrides for NAME CHANGES / synonyms the automated
# iNat name-search can't bridge (e.g. Holway 'Holcopasites minima' == iNat 'minimus').
#
# THE LOOP (per Brandi):
#   1. Each run, whatever the resolvers still can't find is written to a REVIEW WORKLIST
#      (cabr_taxon_ids_needs_review.csv) with an iNaturalist search link -- the pipeline
#      asking you to look those few up by hand. (One list, at the lookup stage, which by
#      then already contains the Holway taxa too.)
#   2. You find the taxon on iNat and record rank + name + the correct taxon_id (+ its
#      current iNat name) in manual_taxon_overrides.csv.
#   3. Every future run READS that file and applies your answer at BOTH the Holway reference
#      build AND the taxonomy lookup build -- so the id (and corrected name) land in both
#      tables, and survive the from-scratch rebuild that would wipe a hand-edit.
#
# The Holway reference table and the taxonomy lookup share the same rank/genus/species/...
# shape, so ONE apply function serves both. Pure + unit-tested in test-manual-overrides.R.
# Depends on: dplyr, readr, stringr (+ stringi if present, for diacritics).
# =============================================================
suppressWarnings(suppressMessages({ library(dplyr); library(readr); library(stringr) }))

`%||%` <- if (exists("%||%")) `%||%` else function(a, b) if (is.null(a) || length(a) == 0) b else a

MANUAL_OVERRIDES_PATH <- "data/reference/manual_taxon_overrides.csv"   # user-curated answers
TAXON_REVIEW_PATH     <- "data/reference/cabr_taxon_ids_needs_review.csv"  # the prompt / worklist
RMI_CACHE_PATH        <- "data/reference/resolved_missing_ids.csv"     # the resolver's verdict cache
MO_REVIEW_RANKS       <- c("genus", "subgenus", "complex", "species", "subspecies")

.mo_titlecase1 <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | x == "", x, paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))))
}

# .mo_norm(): diacritic-insensitive, lowercase, letters+spaces only ("Schönnherria" -> "schonnherria").
.mo_norm <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- if (requireNamespace("stringi", quietly = TRUE))
    stringi::stri_trans_general(x, "Latin-ASCII") else x
  x[is.na(x)] <- ""
  x <- tolower(x); x <- gsub("[^a-z ]", " ", x); trimws(gsub("\\s+", " ", x))
}

# .mo_name_at_rank(): each row's OWN name at its rank -- genus for genus, "genus species" for a
# species, "genus species subspecies" for a subspecies, the subgenus/complex value otherwise.
# This is the key the override + worklist match on (same string the user sees in the worklist).
.mo_name_at_rank <- function(df) {
  col <- function(n) if (n %in% names(df)) as.character(df[[n]]) else rep(NA_character_, nrow(df))
  g <- col("genus"); sp <- col("species"); ss <- col("subspecies")
  sg <- str_remove_all(col("subgenus"), "[()]"); cx <- col("complex")
  rk <- tolower(trimws(col("rank")))
  nz <- function(x) !is.na(x) & trimws(x) != ""   # incomplete names -> NA (never match junk rows)
  dplyr::case_when(
    rk == "genus"      & nz(g)                   ~ trimws(g),
    rk == "subgenus"   & nz(sg)                  ~ trimws(sg),
    rk == "complex"    & nz(cx)                  ~ trimws(cx),
    rk == "species"    & nz(g) & nz(sp)          ~ trimws(paste(g, sp)),
    rk == "subspecies" & nz(g) & nz(sp) & nz(ss) ~ trimws(paste(g, sp, ss)),
    TRUE ~ NA_character_)
}

# load_manual_overrides(): read the curated CSV (rank,name,taxon_id,correct_name,note). Missing file
# -> empty. Rows with a blank taxon_id are dropped (no answer yet -> nothing to apply).
load_manual_overrides <- function(path = MANUAL_OVERRIDES_PATH) {
  empty <- tibble(rank = character(), name = character(), taxon_id = integer(),
                  correct_name = character(), note = character())
  if (!file.exists(path)) return(empty)
  ov <- tryCatch(suppressWarnings(read_csv(path, show_col_types = FALSE)), error = function(e) NULL)
  if (is.null(ov) || !nrow(ov)) return(empty)
  for (c in c("rank", "name", "taxon_id", "correct_name", "note"))
    if (!c %in% names(ov)) ov[[c]] <- if (c == "taxon_id") NA_integer_ else NA_character_
  ov$taxon_id <- suppressWarnings(as.integer(ov$taxon_id))
  ov |> filter(!is.na(taxon_id), !is.na(name), trimws(name) != "")
}

# apply_manual_overrides(): PURE. For each df row whose (rank, name-at-rank) matches an override,
# set taxon_id and -- when correct_name is given -- set scientific_name to it and correct the rank's
# epithet columns to match (Brandi: correct to iNat's current name). A resolved override also marks
# the row resolved / verified when those columns exist. Works on the Holway reference table AND the
# taxonomy lookup. Returns df with an attribute "n_applied".
apply_manual_overrides <- function(df, overrides = NULL) {
  if (is.null(overrides)) overrides <- load_manual_overrides()
  if (is.null(overrides) || !nrow(overrides) || !"rank" %in% names(df)) {
    attr(df, "n_applied") <- 0L; return(df)
  }
  if (!"taxon_id" %in% names(df))        df$taxon_id <- NA_integer_
  if (!"scientific_name" %in% names(df)) df$scientific_name <- NA_character_
  df$taxon_id <- suppressWarnings(as.integer(df$taxon_id))
  key  <- paste(tolower(trimws(as.character(df$rank))),        .mo_norm(.mo_name_at_rank(df)))
  okey <- paste(tolower(trimws(as.character(overrides$rank))), .mo_norm(overrides$name))
  n_applied <- 0L
  for (j in seq_len(nrow(overrides))) {
    for (i in which(key == okey[j])) {
      df$taxon_id[i] <- overrides$taxon_id[j]
      cn <- overrides$correct_name[j]
      if (!is.na(cn) && trimws(cn) != "") {
        df$scientific_name[i] <- trimws(cn)
        w <- strsplit(trimws(cn), "\\s+")[[1]]
        r <- tolower(trimws(as.character(df$rank[i])))
        if (r == "genus"      && length(w) >= 1 && "genus"   %in% names(df)) df$genus[i] <- w[1]
        if (r == "subgenus"   && length(w) >= 1 && "subgenus"%in% names(df)) df$subgenus[i] <- w[length(w)]
        if (r == "complex"    && "complex" %in% names(df))                   df$complex[i] <- trimws(cn)
        if (r == "species"    && length(w) >= 2) {
          if ("genus"   %in% names(df)) df$genus[i]   <- w[1]
          if ("species" %in% names(df)) df$species[i] <- w[2]
        }
        if (r == "subspecies" && length(w) >= 3) {
          if ("genus"      %in% names(df)) df$genus[i]      <- w[1]
          if ("species"    %in% names(df)) df$species[i]    <- w[2]
          if ("subspecies" %in% names(df)) df$subspecies[i] <- w[3]
        }
      }
      if ("resolved" %in% names(df)) df$resolved[i] <- TRUE
      if ("verified" %in% names(df)) df$verified[i] <- TRUE
      n_applied <- n_applied + 1L
    }
  }
  attr(df, "n_applied") <- n_applied
  df
}

# write_review_worklist(): the "please look these up" prompt. Reads the RESOLVER'S verdict cache
# (resolved_missing_ids.csv) and lists exactly the taxa it marked not_found_or_ambiguous -- the
# ones the automated iNat search tried and couldn't resolve (e.g. Holcopasites minima). Each row
# carries the rank, name, an iNaturalist SEARCH URL, and blank taxon_id / correct_name columns to
# fill and paste into manual_taxon_overrides.csv. Taxa you've ALREADY answered (present in the
# overrides) are dropped, so the list only ever shows what's genuinely still open.
write_review_worklist <- function(cache_path = RMI_CACHE_PATH, overrides = NULL, path = TAXON_REVIEW_PATH) {
  empty <- tibble(rank = character(), name = character(), taxon_id = integer(),
                  correct_name = character(), note = character(), inat_search_url = character())
  finish <- function(wl) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    suppressWarnings(readr::write_csv(wl, path, na = ""))
    message(sprintf("  taxon review worklist: %d taxa the auto-search couldn't resolve -> %s",
                    nrow(wl), basename(path)))
    invisible(wl)
  }
  cache <- if (file.exists(cache_path))
    tryCatch(suppressWarnings(read_csv(cache_path, show_col_types = FALSE)), error = function(e) NULL) else NULL
  if (is.null(cache) || !nrow(cache) || !all(c("key", "status") %in% names(cache))) return(finish(empty))
  nf <- cache[grepl("not_found|ambiguous", cache$status, ignore.case = TRUE), , drop = FALSE]
  if (!nrow(nf)) return(finish(empty))

  # key format from resolve_missing_ids.R: "rank|name|parent". Recover rank + name.
  parts <- strsplit(as.character(nf$key), "|", fixed = TRUE)
  rank  <- vapply(parts, function(p) if (length(p) >= 1) p[1] else NA_character_, character(1))
  name  <- vapply(parts, function(p) if (length(p) >= 2) p[2] else NA_character_, character(1))

  if (is.null(overrides)) overrides <- load_manual_overrides()
  answered <- if (nrow(overrides)) paste(tolower(trimws(overrides$rank)), .mo_norm(overrides$name)) else character(0)
  key  <- paste(tolower(trimws(rank)), .mo_norm(name))
  keep <- !is.na(name) & trimws(name) != "" & !(key %in% answered)   # drop already-answered taxa
  if (!any(keep)) return(finish(empty))

  wl <- tibble(
    rank         = tolower(trimws(rank[keep])),
    name         = .mo_titlecase1(trimws(name[keep])),   # "holcopasites minima" -> "Holcopasites minima"
    taxon_id     = NA_integer_,
    correct_name = NA_character_,
    note         = NA_character_,
    inat_search_url = paste0("https://www.inaturalist.org/taxa/search?q=",
                             vapply(trimws(name[keep]), function(s) utils::URLencode(s, reserved = TRUE),
                                    character(1), USE.NAMES = FALSE))
  ) |> distinct(rank, name, .keep_all = TRUE) |> arrange(rank, name)
  finish(wl)
}
