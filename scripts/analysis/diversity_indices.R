# =============================================================
# analysis/diversity_indices.R
# beescabr pipeline -- bee community diversity (Q8)
# Created: 2026-07-22
#
# Q8: "How diverse is the bee community -- by transect, by year, at genus vs
# species rank -- and do the assemblages differ?" Alpha-diversity indices,
# rank-abundance structure, and an NMDS + PERMANOVA composition test, following
# the vegan methods in Stevens' Primer of Ecology (ch. 14).
#
# SCOPE (read this before reading any figure):
#   * DIVERSITY INDICES + NMDS/PERMANOVA use SURVEY RECORDS ONLY (is_survey ==
#     TRUE), because abundance-/composition-based measures only compare fairly
#     across standardized effort. Both methods (lethal specimens + non-lethal
#     survey iNat) are POOLED into one community per group -- that is the bee
#     community found on that transect/year.
#   * The RANK-ABUNDANCE figure additionally SPLITS by method (lethal vs
#     non-lethal, survey-only) because the method contrast is the whole point there.
#   * Every figure carries a caption stating its exact scope + method + rank.
#   * (A whole-park richness MAP that uses ALL records is a separate script.)
#
# Note: TP's larger survey count is legitimate effort, not oversampling. The
# transect is split into two halves (TP1 and TP2) and only one half is walked per
# trip, logged as a single "TP" survey-event -- so each TP record is one
# independent survey walk, the same unit of effort as BST/UPMON/OT; TP's larger
# total just reflects being surveyed on more days. Year comparisons are restricted
# to the intern survey window (Mar-Sep) so seasonal coverage is comparable.
#
# Run from the repo root:  Rscript scripts/analysis/diversity_indices.R
# Depends on: dplyr, stringr, vegan, ggplot2 (+ config.R).
# =============================================================

for (pkg in c("vegan", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(vegan); library(ggplot2) })

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_TRANSECT")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_DIR       <- "data/analysis/diversity"
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
TRANSECTS     <- c("BST", "UPMON", "TP", "OT")
WINDOW_MONTHS <- 3:9                 # intern survey window (Mar-Sep) for year comparisons
MIN_SITE_REC  <- 15                  # a site needs this many records to enter NMDS/PERMANOVA
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"

# consistent scope caption stamped on every figure
scope_cap <- function(scope, method, rank) sprintf("Scope: %s  |  Method: %s  |  Rank: %s",
                                                   scope, method, rank)

# ---- 1. SURVEY-ONLY bee records (both methods), with keys + grouping ----------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
prep <- function(df, method) {
  df %>%
    filter(is_true(is_survey)) %>%
    transmute(
      method   = method,
      transect = toupper(str_squish(transect)),
      year     = suppressWarnings(as.integer(ifelse(!is.na(survey_year) & survey_year != "",
                                                    survey_year, substr(observed_on, 1, 4)))),
      month    = suppressWarnings(as.integer(substr(observed_on, 6, 7))),
      taxon_rank, genus, species) %>%
    mutate(
      species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                             !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
      genus_key   = ifelse(taxon_rank %in% GENUS_RANKS & !is.na(genus) & genus != "", genus, NA))
}
rec <- bind_rows(prep(spec, "lethal"), prep(inat, "nonlethal")) %>%
  filter(transect %in% TRANSECTS)
message(sprintf("Survey-only bee records: %d (lethal %d, non-lethal %d)",
                nrow(rec), sum(rec$method == "lethal"), sum(rec$method == "nonlethal")))

# ---- helpers: community matrix + alpha indices -------------------------------
comm_matrix <- function(df, group_col, key_col) {
  d <- df[!is.na(df[[key_col]]) & !is.na(df[[group_col]]), ]
  t <- table(d[[group_col]], d[[key_col]])
  matrix(as.integer(t), nrow = nrow(t), dimnames = dimnames(t))
}
# NOTE: fully-qualify vegan functions -- igraph also exports diversity(), and if
# the network script loaded igraph earlier in the session it would mask vegan's.
alpha_indices <- function(M) {
  S <- vegan::specnumber(M); H <- vegan::diversity(M, "shannon")
  data.frame(group = rownames(M), n_records = as.integer(rowSums(M)),
             richness = as.integer(S),
             hill_q1_expShannon = round(exp(H), 1),      # effective # of common species
             invSimpson_q2      = round(vegan::diversity(M, "invsimpson"), 1),
             pielou_evenness    = ifelse(S > 1, round(H / log(S), 3), NA_real_), row.names = NULL)
}

# ---- 2. ALPHA DIVERSITY BY TRANSECT (survey-only, both methods, species+genus) --
Mt_sp <- comm_matrix(rec, "transect", "species_key")[TRANSECTS, , drop = FALSE]
Mt_gn <- comm_matrix(rec, "transect", "genus_key")[TRANSECTS, , drop = FALSE]
div_tr <- rbind(cbind(rank = "species", alpha_indices(Mt_sp)),
                cbind(rank = "genus",   alpha_indices(Mt_gn)))
write.csv(div_tr, file.path(OUT_DIR, "diversity_by_transect.csv"), row.names = FALSE)

plot_indices <- function(dfin, file, title, cap, group_lab) {
  long <- dfin %>%
    tidyr_pivot(c("richness", "hill_q1_expShannon", "invSimpson_q2", "pielou_evenness"))
  lvls <- c(richness = "Richness (S)", hill_q1_expShannon = "Effective #species (exp H, q1)",
            invSimpson_q2 = "Inverse Simpson (q2)", pielou_evenness = "Pielou evenness (J)")
  long$metric <- factor(lvls[long$metric], levels = lvls)
  g <- ggplot(long, aes(x = group, y = value, fill = rank)) +
    geom_col(position = position_dodge(0.8), width = 0.7) +
    facet_wrap(~ metric, scales = "free_y") +
    # rank = focal species (ink) vs coarser genus (stone background)
    scale_fill_manual(values = c(species = "#3C3B36", genus = "#C0BBB0"), name = "rank") +
    labs(title = title, subtitle = cap, x = group_lab, y = NULL) +
    theme_beescabr(11) +
    theme(panel.grid.major.x = element_blank())
  ggsave(file, g, width = 9, height = 6.2, dpi = 200, bg = "white")
}
# tiny long-pivot helper (avoid tidyr dep)
tidyr_pivot <- function(df, cols) {
  do.call(rbind, lapply(cols, function(c)
    data.frame(rank = df$rank, group = df$group, metric = c, value = df[[c]])))
}
plot_indices(div_tr, file.path(OUT_DIR, "diversity_by_transect.png"),
             "Bee diversity by transect (CABR)",
             scope_cap("survey records only", "lethal + non-lethal pooled", "species vs genus"),
             "transect")

# ---- 3. ALPHA DIVERSITY BY YEAR (survey-only, both methods, Mar-Sep, species) --
rec_win <- rec %>% filter(month %in% WINDOW_MONTHS, !is.na(year))
My_sp <- comm_matrix(rec_win, "year", "species_key")
div_yr <- cbind(rank = "species", alpha_indices(My_sp))
write.csv(div_yr, file.path(OUT_DIR, "diversity_by_year.csv"), row.names = FALSE)
{
  long <- tidyr_pivot(div_yr, c("richness", "hill_q1_expShannon", "invSimpson_q2", "pielou_evenness"))
  lvls <- c(richness = "Richness (S)", hill_q1_expShannon = "Effective #species (exp H, q1)",
            invSimpson_q2 = "Inverse Simpson (q2)", pielou_evenness = "Pielou evenness (J)")
  long$metric <- factor(lvls[long$metric], levels = lvls)
  g <- ggplot(long, aes(x = factor(group), y = value, group = 1)) +
    geom_line(color = "#3C3B36") + geom_point(color = "#3C3B36", size = 2) +
    facet_wrap(~ metric, scales = "free_y") +
    labs(title = "Bee diversity by year (CABR)",
         subtitle = scope_cap("survey records only, Mar-Sep window", "lethal + non-lethal pooled", "species-level"),
         x = "year", y = NULL) +
    theme_beescabr(11)
  ggsave(file.path(OUT_DIR, "diversity_by_year.png"), g, width = 9, height = 6, dpi = 200, bg = "white")
}

# ---- 4. RANK-ABUNDANCE (Whittaker) -- SPLIT BY METHOD (survey-only, species) ---
rad_df <- function(counts, lab) {
  counts <- sort(counts[counts > 0], decreasing = TRUE)
  data.frame(method = lab, rank = seq_along(counts), rel_abund = counts / sum(counts))
}
sp_leth <- table(rec$species_key[rec$method == "lethal"])
sp_nonl <- table(rec$species_key[rec$method == "nonlethal"])
sp_pool <- table(rec$species_key)
rad <- rbind(rad_df(as.integer(sp_pool), "both pooled"),
             rad_df(as.integer(sp_leth), "lethal (net)"),
             rad_df(as.integer(sp_nonl), "non-lethal (photo)"))
g <- ggplot(rad, aes(rank, rel_abund, color = method)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1) +
  scale_y_log10() +
  # method owns colour: pooled = dark ink, lethal = purple (net), non-lethal = vermillion (photo)
  scale_color_manual(values = c("both pooled" = BEE_INK$primary,
                                "lethal (net)" = unname(BEE_METHOD_COL["lethal"]),
                                "non-lethal (photo)" = unname(BEE_METHOD_COL["nonlethal"])), name = "method") +
  labs(title = "Rank-abundance (dominance) of CABR bee species",
       subtitle = scope_cap("survey records only", "compared: lethal vs non-lethal vs pooled", "species-level"),
       x = "species rank (most -> least common)", y = "relative abundance (log scale)") +
  theme_beescabr(11)
ggsave(file.path(OUT_DIR, "diversity_rank_abundance.png"), g, width = 9, height = 6, dpi = 200, bg = "white")

# ---- 5. NMDS + PERMANOVA: does composition differ by transect/year? ----------
# sites = transect x year (survey-only, both methods pooled), species community.
site <- rec %>% filter(!is.na(species_key), !is.na(year)) %>%
  mutate(site = paste(transect, year, sep = "_"))
Msite <- comm_matrix(site, "site", "species_key")
Msite <- Msite[rowSums(Msite) >= MIN_SITE_REC, , drop = FALSE]
Msite <- Msite[, colSums(Msite) > 0, drop = FALSE]
meta <- data.frame(site = rownames(Msite),
                   transect = sub("_.*", "", rownames(Msite)),
                   year     = sub(".*_", "", rownames(Msite)))
set.seed(1)
perm <- vegan::adonis2(Msite ~ transect + year, data = meta, method = "bray", permutations = 999)
capture.output(print(perm), file = file.path(OUT_DIR, "diversity_permanova.txt"))
message("\nPERMANOVA (Bray-Curtis, transect + year):")
print(perm)

mds <- tryCatch(vegan::metaMDS(Msite, distance = "bray", autotransform = FALSE, trace = 0),
                error = function(e) NULL)
if (!is.null(mds)) {
  sc <- as.data.frame(vegan::scores(mds, display = "sites")); sc$transect <- meta$transect
  ptr <- with(perm, sprintf("PERMANOVA transect: R2=%.2f, p=%.3f", R2[1], `Pr(>F)`[1]))
  g <- ggplot(sc, aes(NMDS1, NMDS2, color = transect, label = meta$site)) +
    geom_point(size = 3) + geom_text(vjust = -0.8, size = 2.6, show.legend = FALSE) +
    scale_color_manual(values = BEE_TRANSECT, name = "transect") +   # transect owns colour (house palette)
    labs(title = "Bee community composition by transect (NMDS, Bray-Curtis)",
         subtitle = paste0(scope_cap("survey records only", "lethal + non-lethal pooled", "species-level"),
                           sprintf("\nsites = transect x year, >=%d records each; ", MIN_SITE_REC), ptr,
                           sprintf("  (stress %.2f)", mds$stress)),
         x = "NMDS1", y = "NMDS2") +
    theme_beescabr(11)
  ggsave(file.path(OUT_DIR, "diversity_nmds_composition.png"), g, width = 8.5, height = 6.5, dpi = 200, bg = "white")
}

message("\nDone. Diversity outputs in: ", normalizePath(OUT_DIR))
