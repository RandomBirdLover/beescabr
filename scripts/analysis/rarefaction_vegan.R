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
#   2. per YEAR      -- Mar-Oct window; rarefy all years to the lowest-count year
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

for (pkg in c("vegan", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(vegan); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("scope_cap")) source("scripts/analysis/theme_beescabr.R")  # canonical caption helper (single source)
OUT_DIR       <- "data/analysis/rarefaction"
SPECIES_RANKS <- c("species", "subspecies")
TRANSECTS     <- c("BST", "UPMON", "TP", "OT")
WINDOW_MONTHS <- 3:10
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"

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
draw <- function(M, key, title, rank, cols = NULL) {
  M <- M[rowSums(M) > 0, , drop = FALSE]
  if (nrow(M) < 2) { message("  ", key, ": <2 groups with data, skipped"); return(invisible()) }
  unit <- UNIT(rank)
  tab <- rarefy_table(M); write.csv(tab, file.path(OUT_DIR, paste0(key, "_vegan.csv")), row.names = FALSE)
  minN <- min(rowSums(M)); cdf <- curve_df(M)
  cap  <- scope_cap("survey records only", "lethal + non-lethal pooled", rank)
  cols <- if (is.null(cols)) setNames(scales::hue_pal()(nrow(M)), rownames(M)) else cols
  g1 <- ggplot(cdf, aes(n, S, color = group)) +
    geom_vline(xintercept = minN, linetype = "dashed", color = "grey50") +
    annotate("text", x = minN, y = 0, label = sprintf(" rarefy to %d", minN),
             hjust = 0, vjust = 0, size = 3, color = "grey40") +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = cols, name = NULL) +
    labs(title = sprintf("%s (%s) - rarefaction curve", title, rank), subtitle = cap,
         x = "records sampled", y = paste0("expected ", unit)) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(color = "#b2182b"))
  ggsave(file.path(OUT_DIR, paste0(key, "_vegan_curves.png")), g1, width = 8, height = 5.4, dpi = 200, bg = "white")
  bd <- rbind(data.frame(group = tab$group, kind = "observed (raw)", S = tab$observed_richness),
              data.frame(group = tab$group, kind = sprintf("rarefied to %d", minN), S = tab$rarefied_richness))
  bd$group <- factor(bd$group, levels = tab$group)
  g2 <- ggplot(bd, aes(group, S, fill = kind)) +
    geom_col(position = position_dodge(0.8), width = 0.7) +
    geom_text(aes(label = round(S)), position = position_dodge(0.8), vjust = -0.3, size = 3) +
    scale_fill_manual(values = setNames(c("#b8b8b8", "#2166ac"),
                                        c("observed (raw)", sprintf("rarefied to %d", minN))), name = NULL) +
    labs(title = sprintf("%s (%s) - rarefied richness", title, rank), subtitle = cap,
         x = NULL, y = unit) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(color = "#b2182b"),
          panel.grid.major.x = element_blank())
  ggsave(file.path(OUT_DIR, paste0(key, "_vegan_bars.png")), g2, width = 8, height = 5, dpi = 200, bg = "white")
  message(sprintf("  %-22s: rarefied to %d records; %s",
                  key, minN, paste(sprintf("%s=%.0f", tab$group, tab$rarefied_richness), collapse = "  ")))
}

# ---- 3. run every comparison at BOTH ranks (genus + species) -----------------
# Every comparison is run twice -- once at genus rank (robust: every ID'd record
# counts) and once at species rank (finer, but only species-resolved records).
RANKS <- c(species = "species_key", genus = "genus_key")
TCOLS <- c(BST = "#1b7837", UPMON = "#762a83", TP = "#2166ac", OT = "#d95f02")
rec_win <- rec %>% filter(month %in% WINDOW_MONTHS, !is.na(year))

for (rk in names(RANKS)) {
  kc <- RANKS[[rk]]
  message(sprintf("Vegan rarefaction, %s rank:", rk))
  # 1. transect
  Mt <- comm(filter(rec, transect %in% TRANSECTS), "transect", kc)
  Mt <- Mt[intersect(TRANSECTS, rownames(Mt)), , drop = FALSE]
  draw(Mt, paste0("by_transect_", rk), "Bees by transect", rk, TCOLS)
  # 2. year (Mar-Oct)
  draw(comm(rec_win, "year", kc), paste0("by_year_", rk), "Bees by year (Mar-Oct)", rk)
  # 3. observer: beeple vs intern
  draw(comm(filter(rec, surveyor %in% c("beeple", "intern")), "surveyor", kc),
       paste0("by_observer_", rk), "Bees by observer (beeple vs intern)", rk,
       c(beeple = "#2166ac", intern = "#1a9850"))
  # 4. method: observations (iNaturalist) vs specimens
  draw(comm(rec, "obs_type", kc), paste0("by_method_", rk),
       "Bees: observations vs specimens", rk,
       c(observation = "#4575b4", specimen = "#d73027"))
}

message("Wrote by_{transect,year,observer,method}_{species,genus}_vegan.{csv,curves.png,bars.png} to ", OUT_DIR)
