# =============================================================
# analysis/coverage_cabr_vs_holway.R
# beescabr pipeline -- CABR bees NOT on the Holway SD-county checklist (Q9)
# Created: 2026-07-21
#
# Q9: "Which taxa on the CABR official checklist are absent from Holway's San
# Diego County checklist?" -- i.e. bees recorded at Cabrillo that Holway's
# county reference does not list. These are the interesting ones: genuine
# county/park additions, OR iNaturalist misidentifications that slipped onto the
# checklist. This run is built to help tell those apart.
#
# WHICH taxa: the official checklist's `holway` flag == FALSE.
# HOW MUCH TO TRUST each: computed from the CURRENT cleaned tables, NOT the
# checklist's own `specimen`/`inat` flags (those are stale -- the specimen-flag
# rebuild is still pending, so e.g. Lasioglossum gemmatum reads specimen=FALSE
# on the checklist yet has 19 specimens in the cleaned data). We recompute
# evidence from records and show the checklist flags alongside for comparison.
#
# THE MISID CHECK:
#   * specimen-backed  -> a physical voucher exists -> solid, low misID risk.
#   * iNat-only        -> photo ID only -> verify. iNat `quality_grade` splits
#                         this: `research` = 2+ community IDs agree (stronger);
#                         `needs_id` / `casual` = one person's call (weaker).
#   * no direct record -> flagged via complex/ancestry mapping but no current
#                         record matches by name or taxon_id -> review the mapping.
# The iNat observation URLs for every photo-evidenced taxon are written out so a
# reviewer can open each record and confirm the ID.
#
# Run from the repo root:  Rscript scripts/analysis/coverage_cabr_vs_holway.R
# Depends on: dplyr, stringr (+ config.R). No modeling -- checklist arithmetic.
# =============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
CHECKLIST_CABR <- "data/checklists/cabr/cabr_official_native_bee_checklist.csv"
OUT_DIR        <- "data/analysis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

norm <- function(x) str_squish(as.character(x))            # trim/collapse whitespace
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"
`%||%` <- function(a, b) if (is.null(a)) b else a  # null-coalescing (base R lacks it)

# ---- 1. the CABR taxa Holway doesn't list -----------------------------------
chk  <- read.csv(CHECKLIST_CABR, stringsAsFactors = FALSE, check.names = FALSE)
noth <- chk %>% filter(!is_true(holway))                   # holway flag == FALSE
message(sprintf("CABR checklist taxa NOT on Holway: %d", nrow(noth)))

spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

# ---- 2. match each taxon to CURRENT records (by scientific_name, taxon_id) ---
# returns the matching rows of `df` for one checklist taxon
match_records <- function(df, tid, sci) {
  sci <- norm(sci)
  hit <- (!is.na(df$taxon_id) & as.character(df$taxon_id) == as.character(tid))
  if (nzchar(sci)) hit <- hit | (norm(df$scientific_name) == sci)
  df[hit, , drop = FALSE]
}

rows <- lapply(seq_len(nrow(noth)), function(i) {
  tid <- noth$taxon_id[i]; sci <- noth$scientific_name[i]
  s <- match_records(spec, tid, sci)
  n <- match_records(inat, tid, sci)
  n_research <- sum(tolower(norm(n$quality_grade)) == "research")
  list(spec = s, inat = n, n_spec = nrow(s), n_inat = nrow(n), n_research = n_research)
})

# ---- 3. evidence class + review priority ------------------------------------
classify <- function(r) {
  if (r$n_spec > 0)                      # a physical voucher exists
    return(c(evidence = "specimen-backed",
             priority = "Solid - voucher specimen"))
  if (r$n_inat > 0 && r$n_research > 0)
    return(c(evidence = "iNat only (some research-grade)",
             priority = "Verify - photo ID (some community-vetted)"))
  if (r$n_inat > 0)
    return(c(evidence = "iNat only (none research-grade)",
             priority = "Verify FIRST - photo ID, none community-vetted"))
  c(evidence = "no direct record match",
    priority = "Review mapping - flagged via complex/ancestry, no current record")
}

summary_tbl <- do.call(rbind, lapply(seq_len(nrow(noth)), function(i) {
  r <- rows[[i]]; cl <- classify(r)
  data.frame(
    scientific_name       = noth$scientific_name[i],
    taxon_rank            = noth$taxon_rank[i],
    family                = noth$family[i],
    genus                 = noth$genus[i],
    taxon_id              = noth$taxon_id[i],
    n_specimen_records    = r$n_spec,
    n_inat_records        = r$n_inat,
    n_inat_research_grade = r$n_research,
    evidence              = cl["evidence"],
    review_priority       = cl["priority"],
    checklist_specimen_flag = toupper(norm(noth$specimen[i])),
    checklist_inat_flag     = toupper(norm(noth$inat[i])),
    row.names = NULL)
}))

# sort: verify-first, then verify, then review-mapping, then solid
prio_rank <- c(
  "Verify FIRST - photo ID, none community-vetted" = 1,
  "Verify - photo ID (some community-vetted)"      = 2,
  "Review mapping - flagged via complex/ancestry, no current record" = 3,
  "Solid - voucher specimen"                       = 4)
summary_tbl <- summary_tbl[order(prio_rank[summary_tbl$review_priority],
                                 -summary_tbl$n_inat_records), ]
write.csv(summary_tbl, file.path(OUT_DIR, "coverage_cabr_not_on_holway.csv"), row.names = FALSE)

# ---- 4. iNat observation URLs to double-check (photo-evidenced taxa) ---------
url_rows <- do.call(rbind, lapply(seq_len(nrow(noth)), function(i) {
  n <- rows[[i]]$inat
  if (nrow(n) == 0) return(NULL)
  data.frame(
    scientific_name = noth$scientific_name[i],
    taxon_rank      = noth$taxon_rank[i],
    obs_id          = n$obs_id,
    observed_on     = n$observed_on,
    observer        = n$observer,
    quality_grade   = n$quality_grade,
    flower_visited  = n$flower_visited,
    url             = n$url,
    row.names = NULL)
}))
if (!is.null(url_rows))
  url_rows <- url_rows[order(url_rows$scientific_name,
                             url_rows$quality_grade != "research"), ]
write.csv(url_rows %||% data.frame(),
          file.path(OUT_DIR, "coverage_cabr_not_on_holway_inat_records.csv"),
          row.names = FALSE)

# ---- 5. figure: the taxa, record counts, coloured by evidence ---------------
pal <- c("specimen"      = "#1b7837",   # green  = voucher-backed (solid)
         "inat_research" = "#2166ac",   # blue   = iNat, community-vetted
         "inat_needsid"  = "#d95f02")   # orange = iNat, unvetted (check first)

pdat <- summary_tbl[nrow(summary_tbl):1, ]   # so highest priority sits at top
spec_n     <- pdat$n_specimen_records
res_n      <- pdat$n_inat_research_grade
needs_n    <- pmax(pdat$n_inat_records - pdat$n_inat_research_grade, 0)
M <- rbind(specimen = spec_n, inat_research = res_n, inat_needsid = needs_n)
colnames(M) <- ifelse(nzchar(norm(pdat$scientific_name)),
                      pdat$scientific_name, paste0("(unnamed ", pdat$taxon_id, ")"))

png(file.path(OUT_DIR, "coverage_cabr_not_on_holway.png"),
    width = 1900, height = 1150, res = 200)
op <- par(mar = c(4.5, 12, 3.5, 1))
bp <- barplot(M, horiz = TRUE, las = 1, col = pal, border = NA,
              xlab = "Number of CABR records",
              main = "CABR bees not on Holway's SD-county checklist\n(bar = records; colour = evidence type)",
              cex.names = 0.8)
legend("bottomright", bty = "n",
       fill = pal,
       legend = c("specimen (voucher - solid)",
                  "iNat research-grade (community-vetted)",
                  "iNat needs-ID (verify first)"))
par(op); dev.off()

# ---- 6. console summary -----------------------------------------------------
message("\nCABR-not-on-Holway, by review priority:")
print(summary_tbl[, c("scientific_name", "taxon_rank", "n_specimen_records",
                      "n_inat_records", "n_inat_research_grade", "review_priority")],
      row.names = FALSE)
message("\nWrote: coverage_cabr_not_on_holway.csv (summary), ",
        "coverage_cabr_not_on_holway_inat_records.csv (URLs to verify), ",
        "coverage_cabr_not_on_holway.png")
message("Done. Outputs in: ", normalizePath(OUT_DIR))

