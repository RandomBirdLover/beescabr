# =============================================================
# observations/qc/inat_misid_qc.R
# beescabr -- misID-candidate QC (Q9's per-observation version)
# Created 2026-07-22
#
# Flags individual iNaturalist bee observations whose species-level ID is likely
# WRONG -- or a genuine NEW record -- so a human can verify them on iNaturalist.
# Advisory only: it writes a review QUEUE and changes nothing else.
#
# Runs AFTER the checklist / Holway reference exist -- NOT inside inat_bee_clean.R
# (that stage runs before the checklist and can't judge "corroborated"). Match is
# done DIRECTLY against the specimen table + the Holway SD reference (not the
# checklist's specimen/inat flags, which are stale until the flag rebuild lands).
#
# FLAG (per obs) when ALL hold:
#   1. taxon_rank in species / subspecies   -- a species-level claim is what can be "wrong"
#   2. quality_grade != "research"          -- not community-vetted
#   3. no specimen voucher                  -- name / taxon_id absent from the specimen table
#   4. not on Holway                        -- name / taxon_id absent from the Holway SD reference
# Match: normalized scientific_name, its rolled binomial (so a subspecies matches its
# vouchered species), OR taxon_id.
#
# OUTPUT  data/observations/review/cabr_inat_misid_review.csv
#   obs_id, observed_on, observer, scientific_name, taxon_rank, quality_grade, reason, url
#
# Run: source("scripts/observations/qc/inat_misid_qc.R"); inat_misid_qc()
# =============================================================
suppressWarnings(suppressMessages({ library(dplyr); library(readr) }))

local({
  sdir <- "scripts"
  for (cand in c("scripts", "../scripts", "../../scripts", "../../../scripts", "../../../../scripts"))
    if (dir.exists(cand)) { sdir <- cand; break }
  need <- function(sym, file) if (!exists(sym)) source(file.path(sdir, file))
  need("PATHS",           "config.R")
  need("write_fresh",     "utils/utils.R")
  need("require_columns", "utils/utils.R")
})
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.imq_path <- function(key, default) if (!is.null(PATHS[[key]])) PATHS[[key]] else default
IMQ_INAT     <- .imq_path("inat_clean",       "data/observations/inat_clean/cabr_inat_bee_clean.csv")
IMQ_SPECIMEN <- .imq_path("specimen_clean",   "data/specimens/specimens_clean/cabr_specimen_bee_clean.csv")
IMQ_HOLWAY   <- .imq_path("holway_reference", "data/reference/holway_sd_bee_reference_table_v3.csv")
IMQ_OUT      <- "data/observations/review/cabr_inat_misid_review.csv"
IMQ_OUT_COLS <- c("obs_id", "observed_on", "observer", "scientific_name",
                  "taxon_rank", "quality_grade", "reason", "url")
IMQ_REASON   <- "species-level iNat ID, not research-grade, no specimen voucher + not on Holway -- verify (misID or new record?)"

# ---- pure helpers ----------------------------------------------------------
imq_norm  <- function(x) tolower(trimws(gsub("\\s+", " ", as.character(x))))
# first two words (a subspecies "Genus sp ssp" -> "genus sp"); a 1-word name stays.
imq_binom <- function(x) vapply(strsplit(imq_norm(x), " ", fixed = TRUE),
  function(w) if (length(w) >= 2) paste(w[1], w[2]) else if (length(w)) w[1] else NA_character_, character(1))

# a reference's KNOWN set: normalized full names + rolled binomials, plus taxon_ids.
imq_ref_set <- function(df, name_col = "scientific_name", id_col = "taxon_id") {
  if (is.null(df) || !nrow(df)) return(list(names = character(0), ids = character(0)))
  nm    <- if (name_col %in% names(df)) df[[name_col]] else character(0)
  names <- unique(c(imq_norm(nm), imq_binom(nm))); names <- names[!is.na(names) & names != ""]
  ids   <- if (id_col %in% names(df)) unique(as.character(df[[id_col]])) else character(0)
  ids   <- ids[!is.na(ids) & ids != ""]
  list(names = names, ids = ids)
}

# imq_flag(): PURE. Given the iNat table + specimen ref-set + Holway ref-set, return
# the flagged rows (misID candidates) with a `reason` column.
imq_flag <- function(inat, spec_ref, hol_ref) {
  rank  <- tolower(as.character(inat$taxon_rank))
  qual  <- tolower(trimws(as.character(inat$quality_grade)))
  sci_n <- imq_norm(inat$scientific_name); sci_b <- imq_binom(inat$scientific_name)
  tid   <- as.character(inat$taxon_id)
  is_sp        <- rank %in% c("species", "subspecies")
  not_research <- is.na(qual) | qual != "research"
  in_ref <- function(ref) (sci_n %in% ref$names) | (sci_b %in% ref$names) | (tid %in% ref$ids)
  flag   <- is_sp & not_research & !in_ref(spec_ref) & !in_ref(hol_ref)
  out    <- inat[which(flag), , drop = FALSE]
  if (nrow(out)) out$reason <- IMQ_REASON
  out
}

# ---- main ------------------------------------------------------------------
inat_misid_qc <- function(inat_path = IMQ_INAT, specimen_path = IMQ_SPECIMEN,
                          holway_path = IMQ_HOLWAY, out_path = IMQ_OUT,
                          write = TRUE, verbose = TRUE) {
  stopifnot(file.exists(inat_path))
  rc <- function(p) suppressWarnings(suppressMessages(read_csv(p, show_col_types = FALSE, col_types = cols(.default = "c"))))
  inat <- rc(inat_path)
  require_columns(inat, c("obs_id", "scientific_name", "taxon_id", "taxon_rank", "quality_grade",
                          "observer", "observed_on", "url"), "cabr_inat_bee_clean.csv")

  spec_ref <- if (file.exists(specimen_path)) imq_ref_set(rc(specimen_path)) else list(names = character(0), ids = character(0))
  hol_ref  <- if (file.exists(holway_path))   imq_ref_set(rc(holway_path))   else list(names = character(0), ids = character(0))
  if (verbose && !file.exists(specimen_path)) message("  (misID QC: no specimen table at ", specimen_path, " -- voucher check skipped)")
  if (verbose && !file.exists(holway_path))   message("  (misID QC: no Holway reference at ", holway_path, " -- Holway check skipped)")

  out <- imq_flag(inat, spec_ref, hol_ref) |>
    select(any_of(IMQ_OUT_COLS)) |>
    arrange(scientific_name, observed_on)

  if (write) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    write_fresh(out, out_path, na = "")
  }
  if (verbose)
    message(sprintf("misID QC: %d likely-misID bee obs -> %s\n  (what this is: species-level iNaturalist IDs with no specimen to vouch them AND not on Holway's checklist -- flagged for a human to double-check on iNaturalist; nothing is changed)",
                    nrow(out), basename(out_path)))
  invisible(out)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) inat_misid_qc()
