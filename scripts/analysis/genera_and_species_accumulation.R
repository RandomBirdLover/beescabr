# =============================================================
# analysis/species_accumulation.R
# beescabr pipeline -- native-bee species accumulation by SURVEY EFFORT
# Created: 2026-07-21
#
# Adapts Owen Jones' "Species Accumulation Curves" tutorial
#   (https://rpubs.com/jonesor/SACs)  -- vegan::specaccum -- to the CABR
# native-bee surveys.
#
# THE QUESTION: "as we run more surveys on a transect, how fast do we keep
# turning up new bee TAXA, and are we levelling off?" -- asked at two ranks:
#   * species  (Figure 1)
#   * genus    (Figure 2)
#
# THE UNIT (x-axis): one SURVEY. The authoritative survey list is
# master_per_survey_info.csv (494 surveys). A survey that walked several
# transects in a day counts as one survey FOR EACH transect it covered, so
# the list is expanded to one survey-event per (day x transect). Surveys that
# recorded no identifiable bee still count as effort (an empty row that flattens
# the curve) -- that is the honest picture of survey return.
#
# THE LAYOUT: a 2x2 small-multiple -- one PANEL per transect (BST, UPMON, TP, OT),
# its title tinted in the transect colour. Inside each panel the two methods are drawn
# as curves, COLOUR = method (poppy = lethal net, teal = non-lethal photo) with solid/dashed
# as a secondary cue -- so lethal/non-lethal read the SAME poppy/teal here as in every other
# figure. A `lethal` survey's taxa come from the specimen table (net), a `non-lethal` survey's
# from iNaturalist (photo). The per-transect lethal/non-lethal split is also in the summary
# table. Just TWO figures: species (Fig 1) and genera (Fig 2).
#
# RANK RULES: for the SPECIES figure only species-level IDs count (subspecies
# rolled up). For the GENUS figure any ID that pins a genus counts
# (species / subspecies / subgenus / complex / genus) -- so the genus curve
# uses more records, as it should.
#
# Run from the repo root:
#   Rscript scripts/analysis/genera_and_species_accumulation.R
# Figures + summary table are written to data/analysis/.
# Depends on: vegan, dplyr, stringr (+ config.R for the cleaned-table paths).
# =============================================================

suppressPackageStartupMessages({
  library(vegan)
  library(dplyr)
  library(stringr)
})

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")   # PATHS$specimen_clean, PATHS$inat_clean

# master_per_survey_info.csv is not in config's PATHS list; name it here.
PER_SURVEY_INFO <- "data/project_info/master_per_survey_info.csv"

OUT_DIR         <- "data/analysis/accumulation"
TRANSECTS       <- c("BST", "UPMON", "TP", "OT")          # the lines, in legend order
SPECIES_RANKS   <- c("species", "subspecies")             # ranks that resolve to a species
GENUS_RANKS     <- c("species", "subspecies", "subgenus", # ranks that pin a genus
                     "complex", "genus")
PERMUTATIONS    <- 200                                    # specaccum random permutations
set.seed(1)                                               # reproducible permutation curves

# house style: colours + line-style come from the shared module (theme_beescabr.R)
if (!exists("BEE_TRANSECT")) source("scripts/analysis/theme_beescabr.R")
COLS         <- BEE_TRANSECT      # transect colour -> now TINTS each panel's title
METHOD_COL   <- BEE_METHOD_COL    # curve colour = method: poppy = net (lethal), teal = photo (non-lethal)
LTY          <- BEE_METHOD_LTY    # secondary cue: solid = lethal, dashed = non-lethal
METHOD_LABEL <- BEE_METHOD_LABEL

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 1. bee records, tagged with method + join keys -------------------------
is_true <- function(x) toupper(trimws(as.character(x))) == "TRUE"

spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

# lethal survey  -> specimen (net);  non-lethal survey -> iNat survey rows (photo)
recs <- bind_rows(
  spec %>% transmute(date = trimws(observed_on), transect = toupper(trimws(transect)),
                     method = "lethal", taxon_rank, genus, species),
  inat %>% filter(is_true(is_survey)) %>%
           transmute(date = trimws(observed_on), transect = toupper(trimws(transect)),
                     method = "nonlethal", taxon_rank, genus, species)
) %>%
  mutate(
    species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                           !is.na(species) & species != "",
                         paste(genus, word(species, -1)), NA_character_),
    genus_key   = ifelse(taxon_rank %in% GENUS_RANKS & !is.na(genus) & genus != "",
                         genus, NA_character_),
    k = paste(date, transect, method)
  )
recs_by_key <- split(recs, recs$k)   # fast per-survey lookup

# ---- 2. authoritative survey list, expanded to one row per (day x transect) --
surv <- read.csv(PER_SURVEY_INFO, stringsAsFactors = FALSE, check.names = FALSE)
surv$method <- ifelse(trimws(surv$method) == "lethal", "lethal", "nonlethal")
surv$date   <- trimws(surv$date)

tl       <- strsplit(surv$transects, "[,;]")               # split multi-transect days
expanded <- surv[rep(seq_len(nrow(surv)), lengths(tl)), c("date", "method"), drop = FALSE]
expanded$transect <- toupper(trimws(unlist(tl)))
expanded <- expanded[expanded$transect %in% TRANSECTS, ]
expanded$k <- paste(expanded$date, expanded$transect, expanded$method)

# ---- 3. survey x taxa presence matrix for one group of surveys --------------
# key_col = "species_key" or "genus_key". Rows are surveys (empty rows kept as
# effort); columns are the taxa ever seen in that group.
survey_taxa_matrix <- function(survey_rows, key_col) {
  taxa <- sort(unique(na.omit(
    unlist(lapply(recs_by_key[survey_rows$k], function(df) df[[key_col]])))))
  M <- matrix(0L, nrow = nrow(survey_rows), ncol = length(taxa),
              dimnames = list(NULL, taxa))
  if (length(taxa) == 0L) return(M)
  for (i in seq_len(nrow(survey_rows))) {
    df <- recs_by_key[[survey_rows$k[i]]]
    if (is.null(df)) next
    kk <- unique(na.omit(df[[key_col]]))
    if (length(kk)) M[i, kk] <- 1L
  }
  M
}

# ---- 4. accumulation curves: one PANEL per transect, both methods inside --------
# accumulate(): one specaccum per group; NULL if the group never recorded a taxon
# at this rank (nothing to accumulate).
accumulate <- function(survey_rows, key_col) {
  M <- survey_taxa_matrix(survey_rows, key_col)
  if (ncol(M) == 0L || nrow(M) == 0L) return(NULL)
  specaccum(M, method = "random", permutations = PERMUTATIONS)
}

# Two figures only (species, genera). Each is a 2x2 small-multiple: one panel per transect
# (title tinted its transect colour), and inside each panel the two methods as mean curves --
# COLOUR = method (poppy = lethal net, teal = non-lethal photo), solid/dashed kept as a secondary
# cue. Panels share x/y limits so heights compare directly. CI bands are omitted on purpose
# (overlaps unreadable); the Chao2 +/- SE uncertainty lives in the summary table (section 5).
plot_accumulation <- function(key_col, rank_label, file) {
  sacs <- list()
  for (tr in TRANSECTS) for (m in c("lethal", "nonlethal")) {
    rows <- expanded[expanded$transect == tr & expanded$method == m, ]
    if (!nrow(rows)) next
    s <- accumulate(rows, key_col); if (is.null(s)) next
    sacs[[paste(tr, m)]] <- s
  }
  if (!length(sacs)) { message("No ", rank_label, " to plot."); return(invisible()) }
  xmax <- max(vapply(sacs, function(s) max(s$sites),    numeric(1)))
  ymax <- max(vapply(sacs, function(s) max(s$richness), numeric(1)))

  png(file, width = 1700, height = 1250, res = 200); on.exit(dev.off())
  bee_base_par()                     # house-style fonts + muted axis/label colours
  op <- par(mfrow = c(2, 2), mar = c(4, 4.2, 2.8, 1), oma = c(3.4, 0, 3, 0))
  for (tr in TRANSECTS) {
    plot(NA, xlim = c(0, xmax), ylim = c(0, ymax),
         xlab = "Number of surveys", ylab = paste("Number of", rank_label))
    title(main = tr, col.main = COLS[tr], font.main = 2)      # transect = the panel, tinted its colour
    for (m in c("nonlethal", "lethal")) {                     # lethal drawn last -> on top
      s <- sacs[[paste(tr, m)]]; if (is.null(s)) next
      lines(s$sites, s$richness, col = METHOD_COL[m], lwd = 2.6, lty = LTY[m])
    }
  }
  mtext(sprintf("Native Bee %s Accumulation by Survey Effort",
                paste0(toupper(substring(rank_label, 1, 1)), substring(rank_label, 2))),
        outer = TRUE, cex = 1.15, font = 2, col = BEE_INK$primary, line = 0.4)
  par(op)
  # one shared method legend across the bottom (transect identity is already the panel titles)
  par(fig = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), new = TRUE)
  plot.new()
  legend("bottom", horiz = TRUE, bty = "n", cex = 0.95, seg.len = 2.8, inset = c(0, 0.01),
         legend = c(METHOD_LABEL["lethal"], METHOD_LABEL["nonlethal"]),
         col = c(METHOD_COL["lethal"], METHOD_COL["nonlethal"]),
         lty = c(LTY["lethal"], LTY["nonlethal"]), lwd = 2.6, text.col = BEE_INK$secondary)
}
plot_accumulation("species_key", "species", file.path(OUT_DIR, "accumulation_species.png"))
plot_accumulation("genus_key",   "genera",  file.path(OUT_DIR, "accumulation_genera.png"))

# ---- 5. summary table + incidence-based richness estimates -------------------
# richness_est(): observed richness plus the Chao2 incidence estimate of TRUE
# richness for a survey x taxa matrix, via vegan::specpool(). Chao2 (incidence,
# survey-based) is the estimator that matches these curves -- it asks "given how
# many taxa were seen in exactly one vs two surveys, how many were missed?".
# completeness = observed / estimated, in percent.
richness_est <- function(M) {
  if (ncol(M) == 0L) return(c(obs = 0, chao = NA_real_, se = NA_real_, pct = NA_real_))
  sp <- specpool(M)
  c(obs = sp$Species, chao = round(sp$chao, 1), se = round(sp$chao.se, 1),
    pct = round(100 * sp$Species / sp$chao, 1))
}

summary_tbl <- do.call(rbind, lapply(TRANSECTS, function(tr) {
  rows <- expanded[expanded$transect == tr, ]
  rs <- richness_est(survey_taxa_matrix(rows, "species_key"))
  rg <- richness_est(survey_taxa_matrix(rows, "genus_key"))
  data.frame(
    transect             = tr,
    n_surveys            = nrow(rows),
    surveys_lethal       = sum(rows$method == "lethal"),
    surveys_nonlethal    = sum(rows$method == "nonlethal"),
    species_observed     = rs["obs"],
    species_chao2        = rs["chao"],
    species_chao2_se     = rs["se"],
    species_pct_complete = rs["pct"],
    genera_observed      = rg["obs"],
    genera_chao2         = rg["chao"],
    genera_chao2_se      = rg["se"],
    genera_pct_complete  = rg["pct"],
    row.names = NULL)
}))
write.csv(summary_tbl, file.path(OUT_DIR, "transect_accumulation_summary.csv"), row.names = FALSE)

message("Per-transect survey effort + observed / Chao2-estimated richness:")
print(summary_tbl, row.names = FALSE)
message("Chao2 estimates TRUE richness; the +/- SE (standard error) is how uncertain that\n",
        "estimate is -- true richness is ~95% likely within +/-2 SE. A large SE (e.g. OT)\n",
        "means the estimate is rough ('sample more here'), not a precise target.")
message("\nDone. Figures + table in: ", normalizePath(OUT_DIR))
