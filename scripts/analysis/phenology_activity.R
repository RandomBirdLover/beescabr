# =============================================================
# analysis/phenology_activity.R
# beescabr pipeline -- seasonal activity phenology (Q12 + bee extension)
# Created: 2026-07-22
#
# "When in the year is each taxon active?" -- descriptive phenology straight from
# the records' observation date. NO climate modelling (phenor) and no nesting-
# curve fitting (the CRAN `phenology` package) -- those answer a different
# question and need inputs we don't have. Here we bin observations by day-of-year.
#
# RIDGELINE plots (one smooth activity curve per taxon, stacked and indexed by
# peak day, filled on a spring->fall gradient), three views:
#   * FLOWERING-PLANT phenology -- per plant GENUS, restricted to FLOWERING records
#     (survey plant records; by protocol a plant is only logged when in flower, so
#     this is bloom timing, not year-round presence). See section 1.
#   * BEE phenology    -- per bee GENUS and per bee SPECIES (both methods pooled),
#     the same two-rank split as the accumulation + network runs.
#
# READ IT: each ridge is a kernel density of that taxon's records across the year.
# Taxa are ordered by peak day (earliest at top -> latest at bottom); fill colour
# also tracks the peak (green = spring, peach = autumn). A month-share CSV is
# written alongside each figure.
#
# CAVEAT: interns survey ~Mar-Sep and beeple (non-lethal) run year-round, so the
# winter tails partly reflect WHO was sampling, not just biology.
#
# Run from the repo root:  Rscript scripts/analysis/phenology_activity.R
# Depends on: dplyr, stringr, ggplot2, ggridges (+ config.R).
# =============================================================

for (pkg in c("ggplot2", "ggridges")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(ggplot2); library(ggridges)
})

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_INK")) source("scripts/analysis/theme_beescabr.R")   # shared house style (ink tokens + >=10 rule)
if (!exists("plant_label")) source("scripts/analysis/plant_names.R")  # shared plant common-name labels
OUT_DIR       <- file.path(DIR_REPORT, "phenology")
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
MIN_RECORDS   <- 25   # REPORT floor for phenology: a taxon needs >=25 records to draw a seasonal curve
BW_DAYS       <- 15       # kernel bandwidth (days) -- smooths sparse taxa
# figure-specific SEASONAL ramp (spring green -> fall peach): a continuous by-season gradient,
# reinforced by peak-day ordering. Never a categorical transect, so it stays local, not a house concept.
SPRING_FALL   <- c("#1a9850", "#66bd63", "#d9ef8b", "#fee08b", "#fdae61", "#f46d43")
MONTH_STARTS  <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- ridgeline phenology: one density curve per taxon, peak-ordered ----------
phenology_ridge <- function(df, file, label, min_records = MIN_RECORDS, scope = NULL, title = NULL) {
  df <- df[!is.na(df$taxon) & df$taxon != "" & !is.na(df$doy), ]
  keep <- names(which(table(df$taxon) >= min_records))
  df <- df[df$taxon %in% keep, ]
  if (nrow(df) == 0) { message("No taxa >= ", min_records, " records for ", label); return(invisible()) }

  # peak day-of-year (mode of each taxon's density) -> ordering + fill
  peak <- tapply(df$doy, df$taxon, function(v) {
    if (length(unique(v)) < 2) return(stats::median(v))
    d <- stats::density(v, bw = BW_DAYS); d$x[which.max(d$y)]
  })
  peak <- peak[is.finite(peak)]
  ord  <- names(sort(peak))
  df   <- df[df$taxon %in% ord, ]
  df$taxon    <- factor(df$taxon, levels = rev(ord))       # earliest peak at top
  df$peak_doy <- as.numeric(peak[as.character(df$taxon)])

  g <- ggplot(df, aes(x = doy, y = taxon, fill = peak_doy)) +
    ggridges::geom_density_ridges(bandwidth = BW_DAYS, scale = 2.4, rel_min_height = 0.01,
                                  color = "grey35", linewidth = 0.2, alpha = 0.95) +
    scale_fill_gradientn(colors = SPRING_FALL, guide = "none") +
    scale_x_continuous(breaks = MONTH_STARTS, labels = month.abb,
                       limits = c(1, 366), expand = c(0.01, 0)) +
    labs(title = if (!is.null(title)) title else sprintf("%s phenology - seasonal activity (ridgeline)", label),
         subtitle = sprintf("%d taxa with >= %d records; each curve = record density over the year, ordered by peak day",
                            length(ord), min_records),
         caption = scope, x = NULL, y = NULL) +
    ggridges::theme_ridges(font_size = 8, grid = TRUE) +
    theme(axis.text.y = element_text(size = 6),
          plot.title  = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(color = BEE_INK$note, hjust = 0, size = 8, face = "bold"))
  ggsave(file, g, dpi = 200, limitsize = FALSE, bg = "white",
         width = 8.5, height = max(5, 0.20 * length(ord) + 2))

  # month-share CSV alongside (each taxon's % of records per month)
  df$month <- as.integer(cut(df$doy, breaks = c(MONTH_STARTS, 367), labels = 1:12, right = FALSE))
  cs <- df %>% count(taxon, month, name = "n") %>%
    group_by(taxon) %>% mutate(share = round(n / sum(n), 3)) %>% ungroup()
  write.csv(cs[order(cs$taxon, cs$month), ], sub("\\.png$", ".csv", file), row.names = FALSE)

  # ---- STATISTICAL TEST: Rayleigh test of circular uniformity per taxon -------
  # Treats day-of-year as an angle. H0 = records spread uniformly round the year;
  # a small p means activity is significantly CONCENTRATED (a real season).
  # R = mean resultant length (0 = uniform, 1 = one date); Z = n*R^2; p = exp(-Z).
  ray <- do.call(rbind, lapply(split(df$doy, as.character(df$taxon)), function(v) {
    a <- v / 365 * 2 * pi
    C <- mean(cos(a)); S <- mean(sin(a)); R <- sqrt(C^2 + S^2)
    mu_day <- (atan2(S, C) %% (2 * pi)) / (2 * pi) * 365
    Z <- length(a) * R^2
    data.frame(n = length(a), mean_day = round(mu_day), peak_month = month.abb[pmin(12, pmax(1, ceiling(mu_day / 30.4)))],
               resultant_R = round(R, 3), rayleigh_Z = round(Z, 2),
               rayleigh_p = signif(exp(-Z), 3), seasonal = exp(-Z) < 0.05)
  }))
  ray <- data.frame(taxon = rownames(ray), ray, row.names = NULL)
  write.csv(ray[order(ray$mean_day), ], sub("\\.png$", "_rayleigh.csv", file), row.names = FALSE)
  message(sprintf("  %-14s: %d taxa (>= %d records); %d significantly seasonal (Rayleigh p<0.05)",
                  label, length(ord), min_records, sum(ray$seasonal)))
}

doy_of <- function(x) suppressWarnings(as.integer(format(as.Date(x), "%j")))

## NOTE (#12 -- non-lethal-only is intentional; do NOT add lethal/specimen data here):
## The plant phenology below is built from iNat SURVEY records only (is_survey == TRUE, non-lethal) --
## question #12 asks for "plant phenology based on the survey data (nonlethal only)." Excluding
## specimen/lethal data is deliberate, not a bug. (The BEE phenology in section 2 pools both methods --
## that is a separate extension, not the #12 deliverable.) The scope differs from #7 (both methods)
## because each matches its own question. Keep it survey / non-lethal only.
# ---- 1. FLOWERING-PLANT phenology (per plant genus; survey records = flowering) -
# A plant is around all year, so "recorded" != "flowering". By survey protocol a
# plant is only photographed/logged WHEN IT IS FLOWERING, so survey records are the
# flowering signal. The flower_flowering y/n column is almost never filled (only ~68
# of 9,243 rows), so it can't be the filter on its own -- we use it only to DROP the
# handful explicitly marked "no". Flowering set = is_survey TRUE and flower_flowering
# is not "no". This makes the curves seasonal BLOOM timing, not mere presence.
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"
plants_all <- read.csv(PATHS$inat_plant_clean, stringsAsFactors = FALSE, check.names = FALSE)
plants <- plants_all %>%
  filter(is_true(is_survey), tolower(str_squish(flower_flowering)) != "no")
message(sprintf("Building phenology ridgelines:\n  Flowering plants: %d of %d records (survey-only; dropped %d non-survey, %d flower_flowering='no')",
                nrow(plants), nrow(plants_all),
                sum(!is_true(plants_all$is_survey)),
                sum(is_true(plants_all$is_survey) & tolower(str_squish(plants_all$flower_flowering)) == "no")))
# (The survey-plant-only ridgeline was retired -- the pooled BLOOM-EVIDENCE figure below
#  supersedes it. `plants` is still built above because it is one of the three pooled sources.)

spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

# ---- 1b. COMBINED plant BLOOM-EVIDENCE phenology (folds in bee-associated flowers) ----
# The #12 figure above uses only STANDALONE survey plant observations, so a plant that bees
# clearly use but that nobody logged as its own plant record is under-represented (or absent).
# This COMPANION figure ADDS bloom evidence from the flowers bees were recorded ON -- a bee on
# plant X on date D means X was in bloom on D. Three pooled sources, each a (plant_genus, date)
# bloom point:
#   * survey flowering plant observations (the #12 set)
#   * iNaturalist bee records carrying a plant_genus (the flower_visited obs-field)
#   * specimen records carrying a plant_genus (collection tags)
# Kept SEPARATE from the #12 figure (which stays survey-plant-only by protocol). Broader coverage,
# but it blends sources and their differing effort -- read it as "when is each plant in bloom,
# across all evidence," a companion to (not a replacement for) the protocol-clean plant curve.
pg <- function(x) str_squish(as.character(x))
bloom <- bind_rows(
  data.frame(plant_genus = pg(plants$plant_genus), observed_on = plants$observed_on, src = "survey plant obs",         stringsAsFactors = FALSE),
  data.frame(plant_genus = pg(inat$plant_genus),   observed_on = inat$observed_on,   src = "bee-on-flower (iNat field)", stringsAsFactors = FALSE),
  data.frame(plant_genus = pg(spec$plant_genus),   observed_on = spec$observed_on,   src = "bee-on-flower (specimen tag)", stringsAsFactors = FALSE)) %>%
  filter(!is.na(plant_genus), plant_genus != "")
message(sprintf("  Bloom evidence (combined): %d points  [survey plant obs %d, iNat bee-visited %d, specimen-tagged %d]",
                nrow(bloom), sum(bloom$src == "survey plant obs"),
                sum(bloom$src == "bee-on-flower (iNat field)"), sum(bloom$src == "bee-on-flower (specimen tag)")))
phenology_ridge(data.frame(taxon = plant_label(bloom$plant_genus), doy = doy_of(bloom$observed_on)),
                file.path(OUT_DIR, "phenology_plant_genus_bloom_evidence.png"), "Plant bloom (all evidence)",
                scope = paste0("All bloom evidence: survey in-flower plant records + every plant a bee was recorded on\n",
                               "(iNat flower_visited obs-field + specimen tags). A bee on a plant = it was in bloom then.\n",
                               "Broader coverage than survey plant records alone, but blends sources & their differing effort."),
                title = "Seasonal Plant Bloom by Genus")

# ---- 2. BEE phenology (per genus + per species; both methods) ----------------
bees <- bind_rows(spec[c("observed_on", "taxon_rank", "genus", "species")],
                  inat[c("observed_on", "taxon_rank", "genus", "species")])
bees$doy <- doy_of(bees$observed_on)

phenology_ridge(
  data.frame(taxon = ifelse(bees$taxon_rank %in% GENUS_RANKS & !is.na(bees$genus),
                            str_squish(bees$genus), NA), doy = bees$doy),
  file.path(OUT_DIR, "phenology_bee_genus.png"), "Bee genus", title = "Seasonal Bee Activity by Genus")

phenology_ridge(
  data.frame(taxon = ifelse(bees$taxon_rank %in% SPECIES_RANKS & !is.na(bees$genus) & bees$species != "",
                            paste(str_squish(bees$genus), word(bees$species, -1)), NA), doy = bees$doy),
  file.path(OUT_DIR, "phenology_bee_species.png"), "Bee species", title = "Seasonal Bee Activity by Species")

message("\nDone. Phenology ridgelines + tables in: ", normalizePath(OUT_DIR))
