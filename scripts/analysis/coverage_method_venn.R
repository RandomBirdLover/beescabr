# =============================================================
# analysis/coverage_method_venn.R
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
# Run from the repo root:  Rscript scripts/analysis/coverage_method_venn.R
# Depends on: dplyr, stringr (+ config.R). Base-R Venn -- no extra packages.
# =============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_DIR       <- "data/analysis/method_comparison/yield"
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
# METHOD is the whole subject here, so it owns colour: its own palette (purple net / vermillion photo),
# kept off the transect hues -- the same two method colours used wherever method is shown by fill.
COL_LETHAL    <- unname(BEE_METHOD_COL["lethal"])     # purple     = lethal / specimen (net)
COL_NONLETHAL <- unname(BEE_METHOD_COL["nonlethal"])  # vermillion = non-lethal / iNat (photo)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

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
      format(nl, big.mark = ","), format(nn, big.mark = ","))),
  report = list(
    spec = spec_all, inat = inat_all,
    subfn = function(nl, nn) sprintf(
      "All records (every specimen vs every iNaturalist photo, no window):  lethal %s records   |   non-lethal %s records",
      format(nl, big.mark = ","), format(nn, big.mark = ",")))
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

# ---- Venn drawing -----------------------------------------------------------
draw_circle <- function(x, y, r, col) {
  a <- seq(0, 2 * pi, length.out = 200)
  polygon(x + r * cos(a), y + r * sin(a), col = col, border = "grey45", lwd = 1.5)
}
venn2 <- function(cc, title) {
  plot.new(); plot.window(xlim = c(0, 10), ylim = c(0, 8), asp = 1)
  draw_circle(4.1, 3.6, 2.6, adjustcolor(COL_LETHAL,    0.35))
  draw_circle(5.9, 3.6, 2.6, adjustcolor(COL_NONLETHAL, 0.35))
  # axis-less figure: ink the counts explicitly (bee_base_par's grey fg is tuned for axis plots)
  text(2.7, 3.6, cc["lethal_only"],    cex = 1.7, font = 2, col = BEE_INK$primary)
  text(7.3, 3.6, cc["nonlethal_only"], cex = 1.7, font = 2, col = BEE_INK$primary)
  text(5.0, 3.6, cc["both"],           cex = 1.7, font = 2, col = BEE_INK$secondary)
  text(3.0, 7.2, sprintf("Lethal - %d", cc["lethal_only"] + cc["both"]),
       col = COL_LETHAL, font = 2, cex = 0.95)
  text(7.0, 7.2, sprintf("Non-lethal - %d", cc["nonlethal_only"] + cc["both"]),
       col = COL_NONLETHAL, font = 2, cex = 0.95)
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
  sets <- list(
    species = list(lethal = species_set(spec), nonlethal = species_set(inat)),
    genus   = list(lethal = genus_set(spec),   nonlethal = genus_set(inat)))
  n_leth <- nrow(spec); n_nonleth <- nrow(inat)

  taxa_tbl <- rbind(region_table("species", sets$species),
                    region_table("genus",   sets$genus))
  write.csv(taxa_tbl, file.path(OUT_DIR, paste0("yield_by_method_taxa", sfx, ".csv")), row.names = FALSE)
  cn <- list(species = counts(sets$species), genus = counts(sets$genus))

  png(file.path(OUT_DIR, paste0("yield_by_method", sfx, ".png")),
      width = 2000, height = 1050, res = 200)
  bee_base_par()                                  # house-style fonts + muted axis/title colours
  op <- par(mfrow = c(1, 2), mar = c(1, 1, 3.5, 1), oma = c(2, 0, 3.6, 0))
  venn2(cn$species, sprintf("Species (%d total)",
        cn$species["lethal_only"] + cn$species["both"] + cn$species["nonlethal_only"]))
  venn2(cn$genus, sprintf("Genera (%d total)",
        cn$genus["lethal_only"] + cn$genus["both"] + cn$genus["nonlethal_only"]))
  mtext("Comparing Native Bees Sampling Methods", outer = TRUE, line = 1.4,
        cex = 1.2, font = 2, col = BEE_INK$primary)
  mtext(subfn(n_leth, n_nonleth), outer = TRUE, line = 0.2, cex = 0.85, col = BEE_INK$secondary)
  par(op); dev.off()

  rc   <- rbind(res_cat(spec, "lethal"), res_cat(inat, "nonlethal"))
  ctab <- table(method = rc$method, resolution = rc$resolution)
  chi  <- suppressWarnings(chisq.test(ctab))
  pct_species <- prop.table(ctab, 1)[, "species-level"] * 100
  write.csv(data.frame(method = rownames(ctab), n_records = as.integer(rowSums(ctab)),
                       n_species_level = as.integer(ctab[, "species-level"]),
                       pct_species_level = round(pct_species, 1), row.names = NULL),
            file.path(OUT_DIR, paste0("coverage_method_resolution", sfx, ".csv")), row.names = FALSE)
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
    file.path(OUT_DIR, paste0("coverage_method_resolution_chisq", sfx, ".txt")))

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
message("Done. Outputs in: ", normalizePath(OUT_DIR))
