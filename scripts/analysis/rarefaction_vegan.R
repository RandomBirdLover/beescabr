# =============================================================
# Rarefaction (vegan) -- fair diversity by standardizing sampling effort
# beescabr / Cabrillo National Monument (CABR) native bees
#
# WHY: raw species counts are unfair to compare because more sampling almost
# always finds more species. Rarefaction asks "if every group had been sampled
# the SAME amount, how many species would each have?" -- it sub-samples the better-
# sampled groups down to the smallest group's size and reports expected richness
# there, so leftover differences are real, not effort artifacts.
#
# FOUR COMPARISONS, each run at BOTH genus and species rank (survey records only,
# both methods pooled unless the comparison IS the method split):
#   1. per TRANSECT  -- rarefy all transects to the smallest transect's total
#                       ("is OT really poorer, or just under-sampled?")
#   2. per YEAR      -- Mar-Sep window; rarefy all years to the lowest-count year
#   3. beeple vs intern -- rarefy the two observer groups to a common effort
#   4. observations vs specimens -- non-lethal iNaturalist vs lethal specimens
# Genus rank = robust (every ID'd record counts); species rank = finer but sparser.
#
# METHOD = classic individual-based rarefaction to the LOWEST sample size
# (vegan::rarefy, interpolation only). See the folder README for how this compares
# to iNEXT's size-based and coverage-based standardization, and which to trust.
#
# Run from the repo root:  Rscript scripts/analysis/rarefaction_vegan.R
# Depends on: dplyr, stringr, vegan, ggplot2 (+ config.R).
# =============================================================

for (pkg in c("vegan", "ggplot2", "ggpattern")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(vegan); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_TRANSECT")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_JOURNAL   <- file.path(DIR_JOURNAL, "richness/accumulation")  # consolidated with accumulation (richness-by-effort family);
OUT_REPORT    <- file.path(DIR_REPORT,  "richness/accumulation")  # the dimension is baked into each filename, no by_<dim>/ subfolders
SPECIES_RANKS <- c("species", "subspecies")
TRANSECTS     <- c("BST", "UPMON", "TP", "OT")
WINDOW_MONTHS <- 3:9
# which paper each rarefaction dimension belongs to
JOURNAL_DIMS  <- c("by_method", "by_observer")
# No "rarefaction" folder -- each output sits with the FINDING it supports:
#   by_transect -> richness/accumulation (per-transect richness, next to the accumulation curves + Chao2)
#   by_year     -> richness/diversity    (next to the other by-year community analyses)
#   by_method / by_observer (journal)    -> richness/accumulation (journal side)
rare_base <- function(dimdir) {
  if (dimdir %in% JOURNAL_DIMS)      OUT_JOURNAL
  else if (dimdir == "by_year")      file.path(DIR_REPORT, "richness/diversity")
  else                               OUT_REPORT   # by_transect
}
dir.create(OUT_JOURNAL, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_REPORT,  recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"
# scope_cap(): use the SHARED helper from theme_beescabr.R -- adds Source + data-as-of, one canonical order (no local override).

# ---- 1. survey-only bee records with keys + grouping vars --------------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
prep <- function(df, method) {
  df %>% filter(is_true(is_survey)) %>%
    transmute(method = method,
      surveyor = ifelse(is.na(surveyor_type) | str_squish(surveyor_type) == "",
                        "unattributed", str_squish(tolower(surveyor_type))),
      transect = toupper(str_squish(transect)),
      year  = suppressWarnings(as.integer(ifelse(!is.na(survey_year) & survey_year != "",
                                                 survey_year, substr(observed_on, 1, 4)))),
      month = suppressWarnings(as.integer(substr(observed_on, 6, 7))),
      taxon_rank, genus, species) %>%
    mutate(obs_type = ifelse(method == "lethal", "specimen", "observation"),
           species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                                  !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
           genus_key   = ifelse(!is.na(genus) & genus != "", genus, NA))
}
rec <- bind_rows(prep(spec, "lethal"), prep(inat, "nonlethal"))

# community matrix: rows = groups, cols = taxa at the chosen rank (key_col)
comm <- function(df, group_col, key_col) {
  d <- df[!is.na(df[[group_col]]) & !is.na(df[[key_col]]), ]
  t <- table(d[[group_col]], d[[key_col]])
  matrix(as.integer(t), nrow = nrow(t), dimnames = dimnames(t))
}

# ---- 2. helpers: rarefy-to-lowest table + curves -----------------------------
rarefy_table <- function(M) {
  M <- M[rowSums(M) > 0, , drop = FALSE]
  minN <- min(rowSums(M))
  rr <- vegan::rarefy(M, sample = minN, se = TRUE)      # rows: S (expected) and se
  data.frame(group = rownames(M),
             n_records = as.integer(rowSums(M)),
             observed_richness = as.integer(vegan::specnumber(M)),
             rarefied_to = minN,
             rarefied_richness = round(as.numeric(rr["S", ]), 1),
             rarefied_se = round(as.numeric(rr["se", ]), 2),
             row.names = NULL) %>%
    arrange(desc(rarefied_richness))
}
UNIT <- function(rank) if (rank == "genus") "genera" else "species"
curve_df <- function(M) {
  M <- M[rowSums(M) > 0, , drop = FALSE]
  do.call(rbind, lapply(rownames(M), function(g) {
    v <- M[g, ]; N <- sum(v); ns <- unique(pmax(1, round(seq(1, N, length.out = 40))))
    data.frame(group = g, n = ns, S = sapply(ns, function(n) as.numeric(vegan::rarefy(v, n))))
  }))
}
draw <- function(M, key, title, rank, cols = NULL, group_fill = FALSE) {
  M <- M[rowSums(M) > 0, , drop = FALSE]
  if (nrow(M) < 2) { message("  ", key, ": <2 groups with data, skipped"); return(invisible()) }
  unit <- UNIT(rank)
  # HOUSE RULE: genus-rank figures are hatched, species-rank solid (so a genus chart reads distinct from
  # its species twin). col_geom() draws bars either way -- hatched stripes for genus, plain for species.
  col_geom <- function(...) ggpattern::geom_col_pattern(...,
    pattern = if (rank == "genus") "stripe" else "none",
    pattern_fill = "white", pattern_colour = NA, pattern_angle = 45,
    pattern_density = 0.10, pattern_spacing = 0.028, pattern_key_scale_factor = 0.5)
  # write straight into the accumulation folder; the dimension (+ method label, journal only)
  # is baked into the filename, e.g. rarefaction_by_transect_species_curves.png. The report has
  # only vegan so it drops the "_vegan" tag; the journal keeps it (it also carries iNEXT files).
  dimdir <- sub(paste0("_", rank, "$"), "", key)   # "by_transect_species" -> "by_transect"
  outdir <- rare_base(dimdir); dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  vlab   <- if (dimdir %in% JOURNAL_DIMS) "_vegan" else ""
  pre    <- paste0("rarefaction_", dimdir)   # rank appended LAST, e.g. rarefaction_by_transect_bars_species
  tab <- rarefy_table(M); write.csv(tab, file.path(outdir, paste0(pre, vlab, "_", rank, ".csv")), row.names = FALSE)
  minN <- min(rowSums(M)); cdf <- curve_df(M)
  cap  <- scope_cap("survey records only", "lethal + non-lethal pooled", rank)
  cols <- if (is.null(cols)) setNames(grDevices::colorRampPalette(BEE_SEQ)(nrow(M)), rownames(M)) else cols  # ordinal groups (years) -> blue sequential
  g1 <- ggplot(cdf, aes(n, S, color = group)) +
    geom_vline(xintercept = minN, linetype = "dashed", color = "grey50") +
    annotate("text", x = minN, y = 0, label = sprintf(" rarefy to %d", minN),
             hjust = 0, vjust = 0, size = 3, color = "grey40") +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = cols, name = NULL) +
    labs(title = title, subtitle = "Richness rarefied to a common sampling effort -- differences between groups are real, not just who-was-sampled-more.",
         caption = cap,
         x = "records sampled", y = paste0(unit, " (adjusted for effort)")) +
    theme_beescabr(11) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  # curves: JOURNAL only. The REPORT dropped its rarefaction curves -- the bars (observed vs
  # rarefied) are the report's rarefaction figure (Taro's call). Journal keeps both for review.
  if (dimdir %in% JOURNAL_DIMS) {
    bee_ggsave(file.path(outdir, paste0(pre, vlab, "_curves_", rank, ".png")), g1, width = 8, height = 5.4, bg = "white")
  }
  # RAREFIED-ONLY bars (raw observed counts dropped for clarity, per project decision): the raw
  # vs rarefied pairing cluttered the figure, and the rarefied bar is the one that carries the
  # claim ("genuinely richer, not just more-sampled"). Applies to every dimension (year/transect/
  # method/observer). Flip back to the grouped raw+rarefied version via git history if ever needed.
  bdr <- data.frame(group = tab$group, S = tab$rarefied_richness)
  # transect bars sort ALPHABETICALLY (consistent BST/OT/TP/UPMON across every transect figure); YEAR bars
  # sort CHRONOLOGICALLY; other dimensions keep the rarefy_table order (descending rarefied richness).
  lev <- if (group_fill)               sort(unique(as.character(bdr$group)))
         else if (dimdir == "by_year") as.character(sort(as.numeric(unique(as.character(bdr$group)))))
         else                          tab$group
  bdr$group <- factor(bdr$group, levels = lev)
  # transect version colours each bar by its transect hue; every other dimension uses one neutral ink.
  g2 <- if (group_fill)
    ggplot(bdr, aes(group, S, fill = group)) + col_geom(width = 0.62) +
      scale_fill_manual(values = cols, guide = "none")
  else
    ggplot(bdr, aes(group, S)) + col_geom(width = 0.62, fill = BEE_NEUTRAL[["dark"]])
  g2 <- g2 +
    geom_text(aes(label = round(S)), vjust = -0.35, size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(title = title, subtitle = sprintf("Rarefied to %d records -- taller bar = genuinely richer, not just more-sampled.", minN),
         x = NULL, y = unit, caption = cap) +
    theme_beescabr(11) +
    theme(panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  bee_ggsave(file.path(outdir, paste0(pre, vlab, "_bars_", rank, ".png")), g2, width = 8, height = 5, bg = "white")
  message(sprintf("  %-22s: rarefied to %d records; %s",
                  key, minN, paste(sprintf("%s=%.0f", tab$group, tab$rarefied_richness), collapse = "  ")))
}

# ---- 3. run every comparison at BOTH ranks (genus + species) -----------------
# Every comparison is run twice -- once at genus rank (robust: every ID'd record
# counts) and once at species rank (finer, but only species-resolved records).
RANKS <- c(species = "species_key", genus = "genus_key")
TCOLS <- BEE_TRANSECT   # house transect palette
rec_win  <- rec %>% filter(month %in% WINDOW_MONTHS, !is.na(year))
rec_fair <- rec %>% filter(month %in% 3:10, year %in% 2021:2023)   # FAIR WINDOW for the lethal-vs-non-lethal comparisons (matches yield_by_method/efficiency/Venn; drops 2024 intern photos)

for (rk in names(RANKS)) {
  kc <- RANKS[[rk]]
  message(sprintf("Vegan rarefaction, %s rank:", rk))
  # 1. transect
  Mt <- comm(filter(rec, transect %in% TRANSECTS), "transect", kc)
  Mt <- Mt[intersect(TRANSECTS, rownames(Mt)), , drop = FALSE]
  draw(Mt, paste0("by_transect_", rk), "Bees by Transect", rk, TCOLS, group_fill = TRUE)
  # 2. year (Mar-Sep)
  draw(comm(rec_win, "year", kc), paste0("by_year_", rk), "Bees by Year", rk)
  # 3. observer: beeple vs intern (fair window -- lethal vs non-lethal comparison)
  draw(comm(filter(rec_fair, surveyor %in% c("beeple", "intern")), "surveyor", kc),
       paste0("by_observer_", rk), "Bees by Observer: Beeple vs Intern", rk,
       c(intern = BEE_NEUTRAL[["dark"]], beeple = BEE_NEUTRAL[["light"]]))   # intern = house ink (focus), beeple = stone (background)
  # 4. method: observations (iNaturalist) vs specimens (fair window)
  draw(comm(rec_fair, "obs_type", kc), paste0("by_method_", rk),
       "Bees: Observations vs Specimens", rk,
       c(observation = unname(BEE_METHOD_COL["nonlethal"]), specimen = unname(BEE_METHOD_COL["lethal"])))  # photo vermillion / net purple
}

message("Wrote rarefaction_by_{transect,year}_*_{species,genus} (report) + rarefaction_by_{method,observer}_vegan_*_{species,genus} (journal)\n",
        "  into richness/accumulation/ of each paper.")
