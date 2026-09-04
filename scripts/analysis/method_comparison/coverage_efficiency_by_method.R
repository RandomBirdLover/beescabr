# =============================================================
# analysis/method_comparison/coverage_efficiency_by_method.R
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
# Run from the repo root:  source("scripts/analysis/method_comparison/coverage_efficiency_by_method.R")
# Depends on: dplyr, stringr, vegan, ggplot2 (+ config.R).
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })
has_vegan <- requireNamespace("vegan", quietly = TRUE)
# Dependencies are CHECKED here, not installed: see beescabr_require() in config.R.
if (!exists("beescabr_require")) source("scripts/config.R")
beescabr_require()

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/shared/theme_beescabr.R")   # shared house style
OUT_DIR       <- file.path(DIR_JOURNAL, "method_comparison/efficiency/fair_method_2021_2023")
SPECIES_RANKS <- c("species", "subspecies")
WINDOW_MONTHS <- 3:10          # Mar-Oct: the lethal-netting season
WINDOW_YEARS  <- 2021:2023     # the only years lethal netting ran (fair vs non-lethal)
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

# ---- 1. FAIR-WINDOW bee records, keyed at species + genus -------------------
# Same fair window as coverage_yield_by_method.R so every method-comparison figure shares
# one definition of "non-lethal": survey records only, Mar-Oct, 2021-2023, attributed
# (unattributed/casual dropped). The 2021-2023 clip means interns' iNaturalist photos --
# which only start in 2024 -- are NOT counted as non-lethal; non-lethal = beeple survey photos.
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
prep <- function(df, method) df %>%
  mutate(st = str_squish(tolower(as.character(surveyor_type))),
         st = ifelse(is.na(st) | st == "", "unattributed", st),
         .mo = suppressWarnings(as.integer(substr(observed_on, 6, 7))),
         .yr = suppressWarnings(as.integer(substr(observed_on, 1, 4)))) %>%
  filter(is_true(is_survey), .mo %in% WINDOW_MONTHS, .yr %in% WINDOW_YEARS, st != "unattributed") %>%
  transmute(method = method, taxon_rank, genus, species,
    species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                           !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
    genus_key   = ifelse(!is.na(genus) & genus != "", genus, NA))
rec <- bind_rows(prep(spec, "lethal"), prep(inat, "non-lethal"))

# ---- 2. one efficiency figure per rank: as-recorded vs at-equal-effort --------
# Two panels, SAME contrast in each ("as recorded" vs "at equal effort"):
#   * "{Rank} recorded"        -- richness as a COUNT (raw vs rarefied).
#   * "{Rank} / 100 records"   -- richness as a RATE  (raw vs rarefied).
# The rate is what Taro reads intuitively, but the raw rate favors the smaller-
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
  # color by METHOD (lethal = rose-red, non-lethal = periwinkle); the within-method split is a SHADE:
  # light tint = as recorded, full color = at equal effort. Method is named on the x-axis, so the legend
  # carries only the shade meaning (neutral light/dark swatches). Genus figure is additionally hatched.
  df$fillkey <- paste(as.character(df$method), as.character(df$kind))
  fillvec <- c(setNames(unname(BEE_METHOD_COL_LT[["lethal"]]),    paste("lethal", aslab)),
               setNames(unname(BEE_METHOD_COL[["lethal"]]),       paste("lethal", eqlab)),
               setNames(unname(BEE_METHOD_COL_LT[["nonlethal"]]), paste("non-lethal", aslab)),
               setNames(unname(BEE_METHOD_COL[["nonlethal"]]),    paste("non-lethal", eqlab)))
  .rl <- round(rare[match("lethal", rownames(M))]); .rn <- round(rare[match("non-lethal", rownames(M))])   # rarefied richness per method
  take <- if (.rl > .rn) sprintf("At equal sampling effort, lethal netting finds more %s than non-lethal photos (%d vs %d).", tolower(rank_pl), .rl, .rn)
          else if (.rl < .rn) sprintf("At equal sampling effort, non-lethal photos find more %s than lethal netting (%d vs %d).", tolower(rank_pl), .rn, .rl)
          else sprintf("At equal sampling effort, both methods find the same number of %s (%d each).", tolower(rank_pl), .rl)
  g <- ggplot(df, aes(x = method, y = value, fill = fillkey, alpha = kind)) +
    ggpattern::geom_col_pattern(position = position_dodge(0.7), width = 0.62,   # house rule: genus figure hatched, species solid
      pattern = if (rank_lab == "Genus") "stripe" else "none", pattern_fill = "white", pattern_colour = NA,
      pattern_angle = 45, pattern_density = 0.09, pattern_spacing = 0.03, pattern_key_scale_factor = 0.4) +
    scale_fill_manual(values = fillvec, guide = "none") +
    scale_alpha_manual(values = c(1, 1), breaks = c(aslab, eqlab), name = NULL,   # phantom aes -> builds the shade legend
      guide = guide_legend(override.aes = list(fill = c(BEE_INK$axis, BEE_INK$secondary), alpha = 1, pattern = "none"))) +
    geom_text(aes(label = lab), position = position_dodge(0.7), vjust = -0.35, size = 3.2, colour = BEE_INK$secondary, show.legend = FALSE) +
    facet_wrap(~ panel, scales = "free_y", nrow = 1) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
    labs(title = sprintf("At equal effort, which method finds more %s?", tolower(rank_pl)),
         subtitle = take,
         caption = paste0(
           scope_cap(scope  = "fair window: survey records only, Mar-Oct 2021-2023, attributed (excludes casual/off-date records and interns' 2024 photos)",
                     method = "lethal vs non-lethal",
                     rank   = rank_lab,
                     sig    = bee_test("individual-based rarefaction to equal effort")),
           "\n",
           str_wrap(sprintf("The per-100-records rate favors the smaller-record method (dilution); 'at equal effort' rarefies both to the smaller total (%s records) -- the fair comparison.", format(minN, big.mark = ",", trim = TRUE)), 108)),
         x = "method", y = NULL) +
    theme_beescabr(12) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
          legend.position = "top", panel.grid.major.x = element_blank())
  bee_ggsave(file, g, width = 8.4, height = 5.4, bg = "white")
}
message("Efficiency by method (recorded / per-100 / rarefied):")
# The per-rank figures are RETIRED: efficiency_by_method_both_ranks.png below shows
# both ranks at once, and shows only the equal-effort comparison. The "as recorded"
# bars they carried invited the diluted per-100 rate to be quoted as if it were fair.
# eff_fig("species_key", "Species", file.path(OUT_DIR, "efficiency_by_method_species.png"))
# eff_fig("genus_key",   "Genus",   file.path(OUT_DIR, "efficiency_by_method_genus.png"))

# ---- 5. BOTH RANKS ON ONE FIGURE --------------------------------------------
# The two figures above answer the same question at two ranks, and reading them
# means flipping between files. Here they sit together: species SOLID, genus
# HATCHED (the house rule for rank), method by colour, effort by shade.
eff_rows <- function(key_col, rank_lab) {
  d <- rec[!is.na(rec[[key_col]]), ]
  t <- table(d$method, d[[key_col]])
  M <- matrix(as.integer(t), nrow = nrow(t), dimnames = dimnames(t))
  M <- M[rowSums(M) > 0, , drop = FALSE]
  recs <- rowSums(M); minN <- min(recs)
  obs  <- specnum_rows(M); rare <- as.numeric(rarefy_rows(M, minN))
  rbind(
    data.frame(method = rownames(M), rank = rank_lab, panel = "count", kind = "as recorded",    value = as.numeric(obs)),
    data.frame(method = rownames(M), rank = rank_lab, panel = "count", kind = "at equal effort", value = round(rare)),
    data.frame(method = rownames(M), rank = rank_lab, panel = "rate",  kind = "as recorded",    value = round(100 * obs  / recs, 1)),
    data.frame(method = rownames(M), rank = rank_lab, panel = "rate",  kind = "at equal effort", value = round(100 * rare / minN, 1)))
}
both <- rbind(eff_rows("species_key", "Species"), eff_rows("genus_key", "Genus"))
both$method <- factor(both$method, levels = c("lethal", "non-lethal"))
both$rank   <- factor(both$rank,   levels = c("Species", "Genus"))
both$kind   <- factor(both$kind,   levels = c("as recorded", "at equal effort"))
both$panel  <- factor(ifelse(both$panel == "count", "Taxa recorded", "Taxa / 100 records"),
                      levels = c("Taxa recorded", "Taxa / 100 records"))
both$lab    <- ifelse(both$panel == "Taxa recorded", sprintf("%.0f", both$value), sprintf("%.1f", both$value))
fillmap <- c("lethal.as recorded" = BEE_METHOD_COL_LT[["lethal"]],
             "lethal.at equal effort" = BEE_METHOD_COL[["lethal"]],
             "non-lethal.as recorded" = BEE_METHOD_COL_LT[["nonlethal"]],
             "non-lethal.at equal effort" = BEE_METHOD_COL[["nonlethal"]])
both$fillkey <- paste(both$method, both$kind, sep = ".")

# the finding in a sentence, per rank, from the rarefied (equal-effort) numbers --
# the same takeaway the single-rank figures carry, said once for both.
.eff_takeaway <- function(d) {
  say <- function(rk) {
    v <- d[d$rank == rk & d$panel == "Taxa recorded" & d$kind == "at equal effort", ]
    l <- v$value[v$method == "lethal"]; n <- v$value[v$method == "non-lethal"]
    if (!length(l) || !length(n)) return(NULL)
    w <- if (l > n) "lethal netting finds more" else if (n > l) "non-lethal photos find more" else "the two are tied on"
    sprintf("%s %s (%.0f vs %.0f)", w, if (rk == "Genus") "genera" else tolower(rk), l, n)
  }
  parts <- Filter(Negate(is.null), list(say("Species"), say("Genus")))
  paste0("At equal sampling effort, ", paste(parts, collapse = ";\nbut "),
         ".\nBoth methods rarefied to the smaller record total, so the comparison is fair.")
}
# "as recorded" is the unfair number -- it just reflects who has more records. The
# figure shows only the rarefied comparison; the raw counts stay in the CSV.
both_fair <- both[both$kind == "at equal effort", , drop = FALSE]
gg <- ggplot(both_fair, aes(x = method, y = value, fill = fillkey)) +
  ggpattern::geom_col_pattern(aes(pattern = rank), position = position_dodge(0.8), width = 0.72,
      pattern_fill = "white", pattern_colour = NA, pattern_density = 0.30,
      pattern_spacing = 0.021, pattern_angle = 45, colour = NA) +
  ggpattern::scale_pattern_manual(values = c(Species = "none", Genus = "stripe"), name = NULL) +
  scale_fill_manual(values = fillmap, guide = "none") +
  geom_text(aes(label = lab, group = rank), position = position_dodge(0.8), vjust = -0.35, size = 3.4) +
  facet_wrap(~ panel, nrow = 1, scales = "free_y") +
  # headroom so the value label above the tallest bar is not clipped
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(title = "At equal effort, which method finds more taxa?",
       subtitle = .eff_takeaway(both), x = NULL, y = NULL,
       caption = scope_cap(scope  = "fair window: survey records only, Mar-Oct 2021-2023, attributed (excludes casual/off-date records and interns' 2024 photos)",
                           method = "lethal vs non-lethal", rank = "Species + Genus",
                           sig    = bee_test("individual-based rarefaction to equal effort"))) +
  theme_beescabr(12) +                       # theme_minimal + the house text conventions
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top", panel.grid.major.x = element_blank())
bee_ggsave(file.path(OUT_DIR, "efficiency_by_method_both_ranks.png"), gg,
           width = 8.5, height = 5.2, bg = "white")
message("  both ranks on one figure: efficiency_by_method_both_ranks.png")
message("Wrote efficiency_by_method_{species,genus}.png to ", OUT_DIR)
