# =============================================================
# analysis/shared/not_on_holway.R
# SHARED: CABR bees absent from Holway's SD-county checklist (ANY rank) + the
# "N new bees not in Holway's Checklist" prompt.
#
# Factored out so BOTH callers compute the same set and print the same grouped
# list, with different framing:
#   * pipeline stage 11 (mode = "review")  -> "double-check these in review"
#   * coverage_cabr_vs_holway.R (mode = "report") -> park-facing "our new finds"
#
# A taxon is "not on Holway" when the CABR official checklist's holway flag is FALSE
# *and* Holway's reference table lists neither its taxon_id nor its binomial at any rank.
# The flag alone is rank-sensitive, so iNat 'complex' nodes of a name/taxon Holway carries
# (e.g. the blank-named Bombus fervidus / Colletes americanus complexes) would otherwise show
# as false new records; the taxon_id + binomial recheck drops those. Only iNat-evidenced taxa are
# listed in the prompt -- those are the ones
# a reviewer can open on iNaturalist to confirm a trusted scientist made the ID.
# Two groups, each numbered:
#   * seen only on iNat (never collected) -- photo ID only, verify first
#   * seen on iNat AND collected          -- a specimen voucher backs it too
# Unlike the misID QC (species-rank only), this surfaces complex/genus-rank
# finds too, so "actually a new species" candidates aren't missed.
#
# Depends on: dplyr, stringr.
# =============================================================
suppressPackageStartupMessages({library(dplyr); library(stringr)})

.noh_norm    <- function(x) stringr::str_squish(as.character(x))
.noh_is_true <- function(x) toupper(stringr::str_squish(as.character(x))) == "TRUE"

# lower-case binomial (first two words) of a name: "Diadasia australis complex" -> "diadasia australis",
# "Bombus" -> "bombus", "" -> "". Rank-blind, so iNat's 'complex' node and Holway's 'species' of the
# same name collapse to one key.
.noh_binom <- function(x) {
  x <- tolower(.noh_norm(x))
  vapply(strsplit(x, " "), function(p) { p <- p[nzchar(p)]; paste(utils::head(p, 2), collapse = " ") }, character(1))
}

# holway_name_set(): lower-case binomials Holway lists, from BOTH its scientific_name and its complex
# column -- a complex counts as covered when Holway carries its member species (Holway files
# Nomada formula/suavis/texana under complex "Nomada vegana"). character(0) if the path is missing.
holway_name_set <- function(holway_path) {
  if (is.null(holway_path) || !nzchar(holway_path) || !file.exists(holway_path)) return(character(0))
  h <- utils::read.csv(holway_path, stringsAsFactors = FALSE, check.names = FALSE)
  cols <- intersect(c("scientific_name", "complex"), names(h))
  if (!length(cols)) return(character(0))
  vals <- unlist(lapply(cols, function(cc) h[[cc]]), use.names = FALSE)
  vals <- sub("^\\s*\\([^)]*\\)\\s*", "", vals)   # strip a leading "(Complex) " tag on complex-column values
  setdiff(unique(.noh_binom(vals)), "")
}

# holway_id_set(): taxon_ids present in Holway's reference (as character). A CABR checklist taxon whose
# id is here IS on Holway (the reference carries listed taxa + their ancestry), regardless of a stale
# checklist flag or a blank name. id match is unambiguous -- no name-collision risk. character(0) if missing.
holway_id_set <- function(holway_path) {
  if (is.null(holway_path) || !nzchar(holway_path) || !file.exists(holway_path)) return(character(0))
  h <- utils::read.csv(holway_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"taxon_id" %in% names(h)) return(character(0))
  setdiff(unique(as.character(h$taxon_id)), c("", NA, "NA"))
}

# .noh_row_binom(): best lower-case binomial for a checklist/reference ROW -- prefer scientific_name,
# else the `complex` column (minus its "(Complex) " tag -- where a blank-named complex keeps its name),
# else genus+species, else genus. Mirrors holway_name_set() reading BOTH columns, so an iNat 'complex'
# node whose scientific_name is blank still matches the name Holway lists.
.noh_row_binom <- function(df) {
  colc <- function(n) .noh_norm(if (n %in% names(df)) df[[n]] else rep("", nrow(df)))
  # strip a leading "(Complex) " tag from BOTH name columns: complex rows now carry it on
  # scientific_name too, and the binomial extraction below needs the bare "Genus species".
  sn <- sub("^\\s*\\([^)]*\\)\\s*", "", colc("scientific_name"))
  cx <- sub("^\\s*\\([^)]*\\)\\s*", "", colc("complex"))
  g  <- colc("genus"); sp <- colc("species")
  raw <- ifelse(nzchar(sn), sn, ifelse(nzchar(cx), cx, ifelse(nzchar(g) & nzchar(sp), paste(g, sp), g)))
  .noh_binom(raw)
}

.noh_empty <- function() data.frame(
  scientific_name = character(), taxon_rank = character(), genus = character(),
  taxon_id = character(), n_specimen_records = integer(), n_inat_records = integer(),
  n_inat_research_grade = integer(), group = character(), stringsAsFactors = FALSE)

# not_on_holway_bees(): one row per CABR-checklist taxon with holway == FALSE, annotated with
# CURRENT specimen/iNat record counts (recomputed from the cleaned tables, not the checklist's own
# stale flags) and a `group` label. Returns a 0-row frame if the checklist is missing its holway
# flag or nothing is off-Holway.
#   checklist_path : the CABR official checklist csv (has the boolean `holway` column)
#   spec, inat     : the cleaned specimen + iNat bee tables (data.frames)
#   holway_path    : (optional) Holway reference csv -- when given, applies the name-based correction
#                    so rank/complex mismatches (a name Holway actually lists) are dropped
not_on_holway_bees <- function(checklist_path, spec, inat, holway_path = NULL) {
  chk <- utils::read.csv(checklist_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"holway" %in% names(chk) || !nrow(chk)) return(.noh_empty())
  noth <- chk[!.noh_is_true(chk$holway), , drop = FALSE]
  if (!nrow(noth)) return(.noh_empty())

  ## NOTE -- the Holway reference is CORRECT as-is; do NOT rebuild it to "fix" this, and do NOT
  ## fall back to name-only matching. It ALREADY carries the complex taxon_ids (1266534 Bombus
  ## fervidus, 1438678 Colletes americanus). The false "new bee" hits were never a missing-reference
  ## problem -- they came from the rank-sensitive `holway` flag plus a BLANK scientific_name on those
  ## iNat complex nodes, which a name-only check can't catch. The taxon_id (+ resolved-binomial) match
  ## below IS the fix; keep it. (An earlier "the reference is missing that row" reading turned out to
  ## be a stale local copy of the reference, not a real bug -- so don't go remaking the reference.)
  # CORRECTION (up front): the holway flag is rank-sensitive, so an iNat 'complex' node of a name/taxon
  # Holway carries lands here as a FALSE positive. Drop anything Holway actually has -- by taxon_id
  # (unambiguous, no name-collision risk) OR by binomial resolved from scientific_name/complex/genus.
  # Blank-named complexes (Bombus fervidus, Colletes americanus) are caught here, where the old
  # scientific_name-only recheck missed them.
  .hset <- holway_name_set(holway_path); .hids <- holway_id_set(holway_path)
  if (nrow(noth) && (length(.hset) || length(.hids))) {
    noth <- noth[!((.noh_row_binom(noth) %in% .hset) | (as.character(noth$taxon_id) %in% .hids)), , drop = FALSE]
    if (!nrow(noth)) return(.noh_empty())
  }

  match_n <- function(df, tid, sci) {                     # rows of df matching this taxon
    sci <- .noh_norm(sci)
    hit <- (!is.na(df$taxon_id) & as.character(df$taxon_id) == as.character(tid))
    if (nzchar(sci)) hit <- hit | (.noh_norm(df$scientific_name) == sci)
    df[hit, , drop = FALSE]
  }

  res <- do.call(rbind, lapply(seq_len(nrow(noth)), function(i) {
    tid <- noth$taxon_id[i]; sci <- noth$scientific_name[i]
    s <- match_n(spec, tid, sci); n <- match_n(inat, tid, sci)
    n_res <- sum(tolower(.noh_norm(n$quality_grade)) == "research")
    grp <- if (nrow(n) > 0 && nrow(s) > 0) "inat_and_collected"
           else if (nrow(n) > 0)           "inat_only"
           else if (nrow(s) > 0)           "specimen_only"
           else                            "no_record"
    data.frame(
      scientific_name = .noh_norm(sci), taxon_rank = .noh_norm(noth$taxon_rank[i]),
      genus = .noh_norm(noth$genus[i]), taxon_id = as.character(tid),
      n_specimen_records = nrow(s), n_inat_records = nrow(n),
      n_inat_research_grade = n_res, group = grp, stringsAsFactors = FALSE)
  }))
  res <- if (is.null(res)) .noh_empty() else res
  res   # correction already applied up front (by taxon_id + resolved binomial), before record matching
}

# format_new_bees(): the grouped, numbered "N new bees not in Holway's Checklist" text block.
# Only iNat-evidenced taxa (inat_only + inat_and_collected) are listed. If `noh` lacks a `group`
# column (e.g. the coverage summary_tbl), it's derived from the record counts. Returns a character
# vector of lines -- character(0) when there are none, so callers can writeLines() or skip.
#   mode = "review" -> pipeline QC framing (go verify these)
#   mode = "report" -> analysis framing (our park/county additions)
format_new_bees <- function(noh, mode = c("review", "report")) {
  mode <- match.arg(mode)
  if (is.null(noh) || !nrow(noh)) return(character(0))
  if (!"group" %in% names(noh)) {
    ni <- if ("n_inat_records"     %in% names(noh)) noh$n_inat_records     else rep(0, nrow(noh))
    ns <- if ("n_specimen_records" %in% names(noh)) noh$n_specimen_records else rep(0, nrow(noh))
    noh$group <- ifelse(ni > 0 & ns > 0, "inat_and_collected",
                 ifelse(ni > 0, "inat_only", ifelse(ns > 0, "specimen_only", "no_record")))
  }
  d <- noh[noh$group %in% c("inat_only", "inat_and_collected"), , drop = FALSE]
  if (!nrow(d)) return(character(0))
  only <- d[d$group == "inat_only", , drop = FALSE]
  both <- d[d$group == "inat_and_collected", , drop = FALSE]
  only <- only[order(-only$n_inat_records), , drop = FALSE]     # most-observed first
  both <- both[order(-both$n_inat_records), , drop = FALSE]

  item <- function(r, off) if (!nrow(r)) character(0) else
    sprintf("  %d. (%s) %s", off + seq_len(nrow(r)),
            stringr::str_to_title(.noh_norm(r$taxon_rank)),
            ifelse(nzchar(.noh_norm(r$scientific_name)),
                   .noh_norm(r$scientific_name), paste0("unnamed ", r$taxon_id)))

  sub <- if (mode == "report")
    "New bees recorded at CABR that are NOT on Holway's SD-county checklist -- our park/county additions:"
  else
    c("Double-check these in review on iNaturalist -- confirm a trusted scientist ID'd them",
      "(research-grade / community-vetted), so we know each is real and not a misID.")

  out <- c(sprintf("%d new bees that are not in Holway's Checklist", nrow(d)), "", sub, "")
  if (nrow(only)) out <- c(out, "Seen only on iNat (never collected):", item(only, 0), "")
  if (nrow(both)) out <- c(out, "Seen on iNat and collected:", item(both, nrow(only)))
  out
}
