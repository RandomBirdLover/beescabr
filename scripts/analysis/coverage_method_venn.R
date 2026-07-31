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
# specimen/inat flags (those are stale -- see coverage_cabr_vs_holway.R). Survey
# filter is intentionally OFF: "what exists in each method" wants every record
# (the stakeholders noted survey isn't needed for this overlap).
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
OUT_DIR       <- "data/analysis/coverage"
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
# METHOD is the whole subject here, so it owns colour: its own palette (red net / blue photo),
# kept off the transect hues -- the same two method colours used wherever method is shown by fill.
COL_LETHAL    <- unname(BEE_METHOD_COL["lethal"])     # red  = lethal / specimen (net)
COL_NONLETHAL <- unname(BEE_METHOD_COL["nonlethal"])  # blue = non-lethal / iNat (photo)
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

spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

sets <- list(
  species = list(lethal = species_set(spec), nonlethal = species_set(inat)),
  genus   = list(lethal = genus_set(spec),   nonlethal = genus_set(inat))
)

# ---- 2. region membership table (per rank) ----------------------------------
region_table <- function(rank_name, s) {
  L <- s$lethal; N <- s$nonlethal
  all_taxa <- sort(union(L, N))
  region <- ifelse(all_taxa %in% L & all_taxa %in% N, "both",
            ifelse(all_taxa %in% L, "lethal_only", "nonlethal_only"))
  data.frame(rank = rank_name, taxon = all_taxa, region = region, row.names = NULL)
}
taxa_tbl <- rbind(region_table("species", sets$species),
                  region_table("genus",   sets$genus))
write.csv(taxa_tbl, file.path(OUT_DIR, "coverage_method_venn_taxa.csv"), row.names = FALSE)

counts <- function(s) c(lethal_only    = length(setdiff(s$lethal, s$nonlethal)),
                        both           = length(intersect(s$lethal, s$nonlethal)),
                        nonlethal_only = length(setdiff(s$nonlethal, s$lethal)))
cn <- list(species = counts(sets$species), genus = counts(sets$genus))

# ---- 3. Venn figure (2 panels: species | genus) -----------------------------
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
  text(3.0, 7.2, sprintf("Lethal\n(net) - %d", cc["lethal_only"] + cc["both"]),
       col = COL_LETHAL, font = 2, cex = 0.95)
  text(7.0, 7.2, sprintf("Non-lethal\n(photo) - %d", cc["nonlethal_only"] + cc["both"]),
       col = COL_NONLETHAL, font = 2, cex = 0.95)
  title(main = title, line = 0.2)
}

png(file.path(OUT_DIR, "coverage_method_venn.png"),
    width = 2000, height = 1050, res = 200)
bee_base_par()                                    # house-style fonts + muted axis/title colours
op <- par(mfrow = c(1, 2), mar = c(1, 1, 3.5, 1), oma = c(2, 0, 2, 0))
venn2(cn$species, sprintf("Species (%d total)",
      cn$species["lethal_only"] + cn$species["both"] + cn$species["nonlethal_only"]))
venn2(cn$genus, sprintf("Genera (%d total)",
      cn$genus["lethal_only"] + cn$genus["both"] + cn$genus["nonlethal_only"]))
mtext("Native Bees Sampling Method: Lethal vs Non-Lethal Overlap", outer = TRUE,
      cex = 1.2, font = 2, col = BEE_INK$primary)
par(op); dev.off()

# ---- 3b. STATISTICAL TEST: does taxonomic resolution depend on method? -------
# Chi-square on a 2x2 contingency (method x resolution), where resolution =
# "species-level" (species/subspecies) vs "coarser". Tests the Q2 hypothesis that
# nets reach species more often than photos. Uses every bee record with a genus.
res_cat <- function(df, method) {
  df <- df[!is.na(df$genus) & df$genus != "", ]
  data.frame(method = method,
             resolution = ifelse(df$taxon_rank %in% SPECIES_RANKS, "species-level", "coarser"))
}
rc   <- rbind(res_cat(spec, "lethal"), res_cat(inat, "nonlethal"))
ctab <- table(method = rc$method, resolution = rc$resolution)
chi  <- suppressWarnings(chisq.test(ctab))
pct_species <- prop.table(ctab, 1)[, "species-level"] * 100
write.csv(data.frame(method = rownames(ctab), n_records = as.integer(rowSums(ctab)),
                     n_species_level = as.integer(ctab[, "species-level"]),
                     pct_species_level = round(pct_species, 1), row.names = NULL),
          file.path(OUT_DIR, "coverage_method_resolution.csv"), row.names = FALSE)
writeLines(c(
  "TEST: taxonomic resolution (species-level vs coarser) x survey method",
  "Pearson chi-square test of independence on the 2x2 contingency table.",
  "",
  sprintf("  X-squared = %.1f, df = %d, p = %s",
          chi$statistic, chi$parameter, format.pval(chi$p.value, digits = 3, eps = 1e-16)),
  sprintf("  lethal (net) reaches species: %.1f%%   non-lethal (photo): %.1f%%",
          pct_species["lethal"], pct_species["nonlethal"]),
  "",
  "Interpretation: p < 0.05 -> ID resolution is not independent of method."),
  file.path(OUT_DIR, "coverage_method_resolution_chisq.txt"))
message(sprintf("\nResolution x method chi-square: X2=%.1f, df=%d, p=%.2e  (lethal %.0f%% vs non-lethal %.0f%% species-level)",
                chi$statistic, chi$parameter, chi$p.value, pct_species["lethal"], pct_species["nonlethal"]))

# ---- 4. console summary -----------------------------------------------------
for (rk in c("species", "genus")) {
  cc <- cn[[rk]]
  message(sprintf("\n%s: %d lethal-only, %d shared, %d non-lethal-only",
                  toupper(rk), cc["lethal_only"], cc["both"], cc["nonlethal_only"]))
}
lo <- taxa_tbl$taxon[taxa_tbl$rank == "species" & taxa_tbl$region == "lethal_only"]
message("\nSpecies found ONLY by nets (photo-detection gaps), n=", length(lo), ":")
message("  ", paste(lo, collapse = ", "))
message("\nWrote: coverage_method_venn.png, coverage_method_venn_taxa.csv")
message("Done. Outputs in: ", normalizePath(OUT_DIR))
