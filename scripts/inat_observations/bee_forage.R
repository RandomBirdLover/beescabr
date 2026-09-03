# =============================================================
# observations/bee_forage.R
# beescabr -- BEE-OBS FORAGE PLANTS extractor
# Created 2026-07-21
#
# The iNat bee observations carry a flower_visited observation field: the PLANT a
# bee was recorded on, INSIDE the park. That's direct in-park evidence for the
# plant (a bee was physically on it here) -- and it names plants the standalone
# plant pull misses, including threatened taxa iNat obscures (Euphorbia misera,
# Leptosyne maritima, Ferocactus viridescens all show up as bee forage).
#
# This module pulls the DISTINCT forage plant names (+ obs counts) straight from
# the flattened bee export, using the SAME crosswalk flower-field coalesce the bee
# cleaner uses (inat_bee_clean.R::ibc_annotations -- kept in sync). The plant
# taxonomy lookup ingests these as a second in-park truth source.
#
# Dependency-light on purpose (dplyr/readr only, no sf) so the lookup builder can
# source it cheaply.
#
# Run: source("scripts/inat_observations/bee_forage.R"); write_bee_forage()
# =============================================================
suppressWarnings(suppressMessages({ library(dplyr); library(readr) }))
if (!exists("bx_kv") && file.exists("scripts/utils/console.R")) source("scripts/utils/console.R")

local({
  sdir <- "scripts"
  for (cand in c("scripts", "../scripts", "../../scripts", "../../../scripts"))
    if (dir.exists(cand)) { sdir <- cand; break }
  need <- function(sym, file) if (!exists(sym)) source(file.path(sdir, file))
  need("PATHS",       "config.R")
  need("write_fresh", "utils/utils.R")
})

.bf_path <- function(key, default) if (!is.null(PATHS[[key]])) PATHS[[key]] else default
BF_EXPORT     <- "data/inat_observations/cache/export_flat.rds"
BF_CROSSWALK  <- "data/project_info/crosswalk/master_crosswalk_manual.csv"
# the brain's per-obs CABR membership (obs_id + status + kind). REQUIRED: forage
# must be scoped to bees INSIDE the park, not the whole county export -- an
# unscoped pull would import plants bees visit anywhere in San Diego.
BF_MEMBERSHIP <- "data/inat_observations/cabr_inat_raw.csv"
BF_OUT        <- .bf_path("inat_bee_forage", PATHS$inat_bee_forage)

# bee_forage_names(): distinct flower_visited PLANT names + obs counts from the
# flattened bee export. Mirrors the flower coalesce in ibc_annotations (most-
# populated visited-plant field first). PURE-ish (reads the two files). Returns
# tibble(scientific_name, n_obs); empty tibble if either input is missing.
bee_forage_names <- function(export_path = BF_EXPORT, crosswalk_path = BF_CROSSWALK,
                             membership_path = BF_MEMBERSHIP) {
  empty <- tibble(scientific_name = character(), n_obs = integer())
  if (!file.exists(export_path) || !file.exists(crosswalk_path)) return(empty)
  # REQUIRED scope: only bees recorded INSIDE the CABR box (the brain's keep/flag
  # rows). Without it we'd harvest forage from bees across the whole county export,
  # which is not in-park evidence. No membership file -> no forage (fail safe).
  if (!file.exists(membership_path)) {
    bx_note("bee forage: no CABR membership file -- skipped, would otherwise pull county-wide")
    return(empty)
  }
  ex_full <- readRDS(export_path)
  mem <- suppressWarnings(suppressMessages(read_csv(membership_path, show_col_types = FALSE)))
  if ("kind"   %in% names(mem)) mem <- mem[!is.na(mem$kind)   & mem$kind == "bee", , drop = FALSE]
  if ("status" %in% names(mem)) mem <- mem[!is.na(mem$status) & mem$status %in% c("keep", "flag"), , drop = FALSE]
  ex_full <- ex_full[as.character(ex_full$id) %in% as.character(mem$obs_id), , drop = FALSE]
  if (!nrow(ex_full)) return(empty)
  cw <- suppressWarnings(suppressMessages(read_csv(crosswalk_path, show_col_types = FALSE, col_types = cols(.default = "c"))))
  if (!all(c("name", "inat_field_variants") %in% names(cw))) return(empty)

  splitv <- function(s) { s <- s[!is.na(s)]; if (!length(s)) return(character(0))
                          tolower(trimws(unlist(strsplit(s, "[;,]")))) }
  nonempty <- function(x) !is.na(x) & trimws(as.character(x)) != ""
  fcols <- grep("^field:", names(ex_full), value = TRUE)
  names(fcols) <- tolower(sub("^field:", "", fcols))
  fvf <- splitv(cw$inat_field_variants[cw$name == "flower_visited"])
  fvc <- fcols[intersect(fvf, names(fcols))]
  if (!length(fvc)) return(empty)
  fvc <- fvc[order(-vapply(fvc, function(c) sum(nonempty(ex_full[[c]])), integer(1)))]
  val <- rep(NA_character_, nrow(ex_full))
  for (c in fvc) { v <- trimws(as.character(ex_full[[c]])); take <- is.na(val) & nonempty(v); val[take] <- v[take] }
  tibble(scientific_name = val) %>%
    filter(!is.na(scientific_name), scientific_name != "") %>%
    count(scientific_name, name = "n_obs", sort = TRUE)
}

# write the forage list to cabr_inat_bee_forage.csv (the lookup's in-park source).
write_bee_forage <- function(export_path = BF_EXPORT, crosswalk_path = BF_CROSSWALK,
                             membership_path = BF_MEMBERSHIP, out_path = BF_OUT, verbose = TRUE) {
  fg <- bee_forage_names(export_path, crosswalk_path, membership_path)
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  write_fresh(fg, out_path, na = "")
  if (verbose) { bx_kv("Forage", format(nrow(fg), big.mark = ","), " flower species bees visited"); bx_out(basename(out_path)) }
  invisible(fg)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) write_bee_forage()
