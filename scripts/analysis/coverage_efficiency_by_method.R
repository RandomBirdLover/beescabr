# =============================================================
# analysis/coverage_efficiency_by_method.R
# beescabr -- EFFICIENCY by method: richness at EQUAL sampling effort (rarefaction).
#
# The third tier of the method-comparison story (effort -> yield -> efficiency).
# Raw yield is unfair to compare because the two methods sampled very differently
# (see effort_by_method: 45 vs 451 trips). Rarefaction fixes this: sub-sample the
# bigger method down to the smaller one's record total and compare richness there,
# so leftover differences are real, not effort artifacts. Shown at BOTH species and
# genus rank. (The full multi-way rarefaction lives in richness/rarefaction/.)
#
# Survey records only (matches the rarefaction analysis scope).
# Run from the repo root:  Rscript scripts/analysis/coverage_efficiency_by_method.R
# Depends on: dplyr, stringr, vegan, ggplot2 (+ config.R).
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })
has_vegan <- requireNamespace("vegan", quietly = TRUE)

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_DIR       <- "data/analysis/method_comparison/efficiency"
SPECIES_RANKS <- c("species", "subspecies")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"

# Hurlbert rarefaction (expected species at subsample n) + observed richness.
# Uses vegan when present (matches richness/rarefaction/); falls back to base R so
# the figure still builds on machines without vegan installed.
rarefy_rows <- function(M, n) {
  if (has_vegan) return(as.numeric(vegan::rarefy(M, n)))
  apply(M, 1, function(x) {
    N <- sum(x)
    if (N == 0) return(0)
    sum(1 - exp(lchoose(N - x, n) - lchoose(N, n)))   # E(S_n) = sum_i [1 - C(N-Ni,n)/C(N,n)]
  })
}
specnum_rows <- function(M) if (has_vegan) as.numeric(vegan::specnumber(M)) else rowSums(M > 0)

# ---- 1. survey-only bee records, keyed at species + genus -------------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
prep <- function(df, method) df %>% filter(is_true(is_survey)) %>%
  transmute(method = method, taxon_rank, genus, species,
    species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                           !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
    genus_key   = ifelse(!is.na(genus) & genus != "", genus, NA))
rec <- bind_rows(prep(spec, "lethal"), prep(inat, "non-lethal"))

# ---- 2. one efficiency figure per rank: as-recorded vs at-equal-effort --------
# Two panels, SAME contrast in each ("as recorded" vs "at equal effort"):
#   * "{Rank} recorded"        -- richness as a COUNT (raw vs rarefied).
#   * "{Rank} / 100 records"   -- richness as a RATE  (raw vs rarefied).
# The rate is what Taro reads intuitively, but the raw rate favours the smaller-
# record method (dilution). Rarefying fixes BOTH: rarefy richness to the smaller
# method's record total, then (for the rate) divide by that common total. So the
# "at equal effort" bars are a fair comparison whether you read counts or rates.
eff_fig <- function(key_col, rank_lab, file) {
  rank_pl <- if (rank_lab == "Genus") "Genera" else "Species"   # plural for labels
  d <- rec[!is.na(rec[[key_col]]), ]
  t <- table(d$method, d[[key_col]])
  M <- matrix(as.integer(t), nrow = nrow(t), dimnames = dimnames(t))
  M <- M[rowSums(M) > 0, , drop = FALSE]
  recs  <- rowSums(M)
  minN  <- min(recs)
  obs   <- specnum_rows(M)
  rare  <- as.numeric(rarefy_rows(M, minN))
  naive_rate <- 100 * obs  / recs      # per-100 as recorded (diluted by sampling depth)
  fair_rate  <- 100 * rare / minN      # per-100 at EQUAL effort (rarefied richness / common N)
  P    <- c(count = sprintf("%s recorded", rank_pl), rate = sprintf("%s / 100 records", rank_pl))
  eqlab <- "at equal effort"; aslab <- "as recorded"
  mk <- function(pnl, kind, val, lab) data.frame(method = rownames(M), panel = pnl, kind = kind,
                                                 value = val, lab = lab, stringsAsFactors = FALSE)
  df <- rbind(mk(P[["count"]], aslab, as.integer(obs),        as.character(as.integer(obs))),
              mk(P[["count"]], eqlab, round(rare),            as.character(round(rare))),
              mk(P[["rate"]],  aslab, round(naive_rate, 1),   sprintf("%.1f", naive_rate)),
              mk(P[["rate"]],  eqlab, round(fair_rate, 1),    sprintf("%.1f", fair_rate)))
  df$method <- factor(df$method, levels = c("lethal", "non-lethal"))
  df$panel  <- factor(df$panel,  levels = unname(P))
  df$kind   <- factor(df$kind,   levels = c(aslab, eqlab))
  message(sprintf("  %s: recorded %s | per-100 raw %s -> fair %s | rarefied(%d) %s",
                  rank_lab, paste(sprintf("%s %d", rownames(M), obs), collapse = "/"),
                  paste(sprintf("%s %.1f", rownames(M), naive_rate), collapse = "/"),
                  paste(sprintf("%s %.1f", rownames(M), fair_rate), collapse = "/"),
                  minN, paste(sprintf("%s %.0f", rownames(M), rare), collapse = "/")))
  g <- ggplot(df, aes(x = method, y = value, fill = kind)) +
    geom_col(position = position_dodge(0.7), width = 0.62) +
    geom_text(aes(label = lab), position = position_dodge(0.7), vjust = -0.35, size = 3.2, colour = BEE_INK$secondary) +
    facet_wrap(~ panel, scales = "free_y", nrow = 1) +
    scale_fill_manual(values = setNames(c("#C0BBB0", BEE_INK$primary), c(aslab, eqlab)), name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
    labs(title = sprintf("Efficiency by Method at %s Level", rank_lab),
         subtitle = "As recorded vs at equal sampling effort (rarefaction) -- as a count and as a rate",
         caption = str_wrap(sprintf("The per-100-records rate favours the smaller-record method (a sampling-depth artifact): 'as recorded' non-lethal looks low only because its many records dilute the rate. 'At equal effort' rarefies both methods to the smaller total (%s records) -- the fair comparison, in either counts or rates. Survey records only.", format(minN, big.mark = ",")), 104),
         x = NULL, y = NULL) +
    theme_beescabr(12) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
          legend.position = "top", panel.grid.major.x = element_blank())
  ggsave(file, g, width = 8.4, height = 5.4, dpi = 200, bg = "white")
}
message("Efficiency by method (recorded / per-100 / rarefied):")
eff_fig("species_key", "Species", file.path(OUT_DIR, "efficiency_by_method_species.png"))
eff_fig("genus_key",   "Genus",   file.path(OUT_DIR, "efficiency_by_method_genus.png"))
message("Wrote efficiency_by_method_{species,genus}.png to ", OUT_DIR)
