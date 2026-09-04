# =============================================================
# analysis/method_comparison/coverage_method_venn.R
# beescabr pipeline -- lethal vs non-lethal taxa overlap (Q1)
# Created: 2026-07-21
#
# Q1: "What bees exist in lethal (net/specimen) vs non-lethal (photo/iNat), and
# where do they NOT overlap?" A 2-set Venn per rank, plus the actual taxon lists
# for each region -- so the gaps can be acted on.
#
# WHY THIS MATTERS (detection bias): taxa found ONLY by nets are usually small /
# cryptic bees that photos can't key -- the Lasioglossum (Dialictus) case the
# stakeholders flagged. Taxa found ONLY by photos are either genuine sampling
# gaps (netting missed them) or misIDs to double-check (see Q9). The overlap is
# the confidently-known core.
#
# TWO RANKS (like the accumulation + network runs, since not all bees are ID'd
# to species):
#   * SPECIES Venn -- species-level IDs only (subspecies rolled up). The sharp
#     view; this is where the Dialictus detection gap shows.
#   * GENUS Venn   -- any record that pins a genus. The robust/complete view.
#
# SETS ARE COMPUTED FROM THE CURRENT CLEANED TABLES, not the checklist's
# specimen/inat flags (those are stale -- see coverage_cabr_vs_holway.R).
#
# SPLIT FIGURE -- runs at TWO scopes, one per paper:
#   * _journal -> FAIR WINDOW (survey-only, Mar-Oct 2021-2023, attributed) so the
#                 overlap compares the two methods on equal footing -- the same scope
#                 as every other lethal-vs-non-lethal figure. Method-comparison paper.
#   * _report  -> ALL RECORDS (every specimen vs every iNaturalist photo, no window,
#                 casual public included) -- the park's full picture. NPS report.
#
# Run from the repo root:  Rscript scripts/analysis/method_comparison/coverage_method_venn.R
# Depends on: dplyr, stringr (+ config.R). Base-R Venn -- no extra packages.
# =============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/shared/theme_beescabr.R")   # shared house style
YIELD_SUB     <- "method_comparison/yield"   # concept path under each paper root (journal & report)
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
# METHOD is the whole subject here, so it owns color: its own palette (purple net / vermillion photo),
# kept off the transect hues -- the same two method colors used wherever method is shown by fill.
COL_LETHAL    <- unname(BEE_METHOD_COL["lethal"])     # purple     = lethal / specimen (net)
COL_NONLETHAL <- unname(BEE_METHOD_COL["nonlethal"])  # vermillion = non-lethal / iNat (photo)
# each scope writes into its own paper folder (created inside run_scope)

# ---- 1. taxa sets per method ------------------------------------------------
species_set <- function(df) {
  df %>%
    filter(taxon_rank %in% SPECIES_RANKS,
           !is.na(genus), genus != "", !is.na(species), species != "") %>%
    transmute(taxon = paste(genus, word(species, -1))) %>%
    distinct() %>% pull(taxon)
}
genus_set <- function(df) {
  df %>%
    filter(taxon_rank %in% GENUS_RANKS, !is.na(genus), genus != "") %>%
    distinct(genus) %>% pull(genus)
}

spec_all <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat_all <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

# ---- FAIR WINDOW helper -----------------------------------------------------
# Same scope as coverage_yield_by_method.R / efficiency, so every method comparison
# shares one definition of lethal vs non-lethal: survey records only, Mar-Oct,
# 2021-2023, attributed (unattributed/casual dropped; interns' 2024 photos excluded).
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"
fair_window <- function(df) {
  st <- str_squish(tolower(as.character(df$surveyor_type)))
  st[is.na(st) | st == ""] <- "unattributed"
  mo <- suppressWarnings(as.integer(substr(df$observed_on, 6, 7)))
  yr <- suppressWarnings(as.integer(substr(df$observed_on, 1, 4)))
  df[is_true(df$is_survey) & mo %in% FAIR_MONTHS & yr %in% FAIR_YEARS & st != "unattributed", , drop = FALSE]
}

# ---- TWO SCOPES (this figure is a SPLIT) ------------------------------------
#   * journal -> FAIR WINDOW: methods compared on equal footing (the method-
#                comparison paper). Drops casual public + interns' 2024 photos.
#   * report  -> ALL RECORDS: every specimen vs every iNaturalist photo, no window,
#                casual public included -- the park's full "what nets vs photos found".
SCOPES <- list(
  journal = list(
    spec = fair_window(spec_all), inat = fair_window(inat_all),
    subfn = function(nl, nn) sprintf(
      "Fair window (survey-only, Mar-Oct 2021-2023):  lethal %s records   |   non-lethal %s records",
      format(nl, big.mark = ",", trim = TRUE), format(nn, big.mark = ",", trim = TRUE))),
  report = list(
    spec = spec_all, inat = inat_all,
    subfn = function(nl, nn) sprintf(
      "All records (every specimen vs every iNaturalist photo, no window):  lethal %s records   |   non-lethal %s records",
      format(nl, big.mark = ",", trim = TRUE), format(nn, big.mark = ",", trim = TRUE)))
)

# ---- region membership + counts (per rank) ----------------------------------
region_table <- function(rank_name, s) {
  L <- s$lethal; N <- s$nonlethal
  all_taxa <- sort(union(L, N))
  region <- ifelse(all_taxa %in% L & all_taxa %in% N, "both",
            ifelse(all_taxa %in% L, "lethal_only", "nonlethal_only"))
  data.frame(rank = rank_name, taxon = all_taxa, region = region, row.names = NULL)
}
counts <- function(s) c(lethal_only    = length(setdiff(s$lethal, s$nonlethal)),
                        both           = length(intersect(s$lethal, s$nonlethal)),
                        nonlethal_only = length(setdiff(s$nonlethal, s$lethal)))

# ---- Venn drawing (donut-chart style: saturated regions cut apart by white gaps,
# counts big + bold + white inside each region, colored method headers above) ----
.arc <- function(x, y, r, a0, a1, n = 240) {
  a <- seq(a0, a1, length.out = n); cbind(x + r * cos(a), y + r * sin(a))
}
venn2 <- function(cc, title) {
  plot.new(); plot.window(xlim = c(0, 10), ylim = c(0, 8), asp = 1)
  x1 <- 4.1; x2 <- 5.9; yy <- 3.6; r <- 2.6
  th  <- acos((x2 - x1) / (2 * r))         # half-angle at the circle intersection points
  mix <- grDevices::rgb(t(round((grDevices::col2rgb(COL_LETHAL) +
                                 grDevices::col2rgb(COL_NONLETHAL)) / 2)), maxColorValue = 255)
  # the three regions as their OWN polygons: left crescent, lens, right crescent
  left  <- rbind(.arc(x1, yy, r, th, 2 * pi - th),        .arc(x2, yy, r, pi + th, pi - th))
  lens  <- rbind(.arc(x1, yy, r, -th, th),                .arc(x2, yy, r, pi - th, pi + th))
  right <- rbind(.arc(x2, yy, r, pi - th, -(pi - th)),    .arc(x1, yy, r, -th, th))
  polygon(left,  col = COL_LETHAL,    border = "white", lwd = 7)
  polygon(right, col = COL_NONLETHAL, border = "white", lwd = 7)
  polygon(lens,  col = mix,           border = "white", lwd = 7)
  text(2.48, yy + 0.14, cc["lethal_only"],    cex = 2.2, font = 2, col = "white")
  text(7.52, yy + 0.14, cc["nonlethal_only"], cex = 2.2, font = 2, col = "white")
  text(5.0, yy + 0.14, cc["both"],           cex = 2.2, font = 2, col = "white")
  text(2.48, yy - 0.62, "nets\nonly",   cex = 0.6, col = "white")
  text(7.52, yy - 0.62, "photos\nonly", cex = 0.6, col = "white")
  text(5.0, yy - 0.66, "both",        cex = 0.62, col = "white")
  text(2.9, 7.35, "NETS",        col = COL_LETHAL,    font = 2, cex = 1.15)
  text(2.9, 6.85, sprintf("specimens · %d", cc["lethal_only"] + cc["both"]),
       col = COL_LETHAL, cex = 0.78)
  text(7.1, 7.35, "INATURALIST", col = COL_NONLETHAL, font = 2, cex = 1.15)
  text(7.1, 6.85, sprintf("photos · %d", cc["nonlethal_only"] + cc["both"]),
       col = COL_NONLETHAL, cex = 0.78)
  title(main = title, line = 0.2)
}

# ---- chi-square: does taxonomic resolution depend on method? ----------------
# 2x2 (method x resolution); resolution = "species-level" (species/subspecies) vs
# "coarser". Tests the Q2 hypothesis that nets reach species more often than photos.
res_cat <- function(df, method) {
  df <- df[!is.na(df$genus) & df$genus != "", ]
  data.frame(method = method,
             resolution = ifelse(df$taxon_rank %in% SPECIES_RANKS, "species-level", "coarser"))
}

# ---- run BOTH scopes --------------------------------------------------------
run_scope <- function(scope, spec, inat, subfn) {
  sfx <- paste0("_", scope)
  od  <- file.path(if (scope == "journal") DIR_JOURNAL else DIR_REPORT,
                   if (scope == "journal") file.path(YIELD_SUB, "fair_method_2021_2023") else YIELD_SUB)   # straight into the paper folder
  dir.create(od, recursive = TRUE, showWarnings = FALSE)
  sets <- list(
    species = list(lethal = species_set(spec), nonlethal = species_set(inat)),
    genus   = list(lethal = genus_set(spec),   nonlethal = genus_set(inat)))
  n_leth <- nrow(spec); n_nonleth <- nrow(inat)

  taxa_tbl <- rbind(region_table("species", sets$species),
                    region_table("genus",   sets$genus))
  write.csv(taxa_tbl, file.path(od, paste0("yield_by_method_taxa", sfx, ".csv")), row.names = FALSE)
  cn <- list(species = counts(sets$species), genus = counts(sets$genus))

  bee_png(file.path(od, paste0("yield_by_method", sfx, ".png")),
      width = 2000, height = 1120, res = 200)
  bee_base_par()                                  # house-style fonts + muted axis/title colors
  op <- par(mfrow = c(1, 2), mar = c(1, 1, 3.5, 1), oma = c(3.4, 1.6, 3.8, 1.6))  # left/right + bottom padding so the caption isn't flush/clipped at the edges
  venn2(cn$species, sprintf("Species (%d total)",
        cn$species["lethal_only"] + cn$species["both"] + cn$species["nonlethal_only"]))
  venn2(cn$genus, sprintf("Genera (%d total)",
        cn$genus["lethal_only"] + cn$genus["both"] + cn$genus["nonlethal_only"]))
  mtext("Did the different sampling methods find the same bees?", outer = TRUE, line = 1.7,
        cex = 1.2, font = 2, col = BEE_INK$primary)
  mtext("Each method turns up taxa the other misses -- the shared core plus each method's own wedge.",
        outer = TRUE, line = 0.5, cex = 0.82, col = BEE_INK$secondary)   # takeaway
  bee_caption_base(scope = if (scope == "journal") "fair window: survey-only, Mar-Oct 2021-2023, attributed" else "all records",
                   method = "lethal vs non-lethal", rank = "species + genus panels")
  par(op); dev.off()

  rc   <- rbind(res_cat(spec, "lethal"), res_cat(inat, "nonlethal"))
  ctab <- table(method = rc$method, resolution = rc$resolution)
  chi  <- suppressWarnings(chisq.test(ctab))
  pct_species <- prop.table(ctab, 1)[, "species-level"] * 100
  write.csv(data.frame(method = rownames(ctab), n_records = as.integer(rowSums(ctab)),
                       n_species_level = as.integer(ctab[, "species-level"]),
                       pct_species_level = round(pct_species, 1), row.names = NULL),
            file.path(od, paste0("coverage_method_resolution", sfx, ".csv")), row.names = FALSE)
  writeLines(c(
    sprintf("TEST (%s scope): taxonomic resolution (species-level vs coarser) x survey method", scope),
    "Pearson chi-square test of independence on the 2x2 contingency table.",
    "",
    sprintf("  X-squared = %.1f, df = %d, p = %s",
            chi$statistic, chi$parameter, format.pval(chi$p.value, digits = 3, eps = 1e-16)),
    sprintf("  lethal (net) reaches species: %.1f%%   non-lethal (photo): %.1f%%",
            pct_species["lethal"], pct_species["nonlethal"]),
    "",
    "Interpretation: p < 0.05 -> ID resolution is not independent of method."),
    file.path(od, paste0("coverage_method_resolution_chisq", sfx, ".txt")))

  message(sprintf("\n[%s] Resolution x method chi-square: X2=%.1f, df=%d, p=%.2e  (lethal %.0f%% vs non-lethal %.0f%% species-level)",
                  scope, chi$statistic, chi$parameter, chi$p.value, pct_species["lethal"], pct_species["nonlethal"]))
  for (rk in c("species", "genus")) {
    cc <- cn[[rk]]
    message(sprintf("[%s] %s: %d lethal-only, %d shared, %d non-lethal-only",
                    scope, toupper(rk), cc["lethal_only"], cc["both"], cc["nonlethal_only"]))
  }
  lo <- taxa_tbl$taxon[taxa_tbl$rank == "species" & taxa_tbl$region == "lethal_only"]
  message(sprintf("[%s] Species found ONLY by nets (photo-detection gaps), n=%d:", scope, length(lo)))
  message("  ", paste(lo, collapse = ", "))
}

for (scope in names(SCOPES)) {
  s <- SCOPES[[scope]]
  run_scope(scope, s$spec, s$inat, s$subfn)
}
message("\nWrote {journal,report} Venns: yield_by_method_{journal,report}.png (+ taxa/resolution CSVs)")
message("Done. Venn split written to journal_paper_2026/ + nps_report_2026/ under ", YIELD_SUB)
