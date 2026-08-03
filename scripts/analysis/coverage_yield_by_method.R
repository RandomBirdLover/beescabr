# =============================================================
# Q11 -- Yield by METHOD: which method finds the most, on a FAIR footing?
# beescabr / Cabrillo National Monument (CABR) native bees
#
# Restricted to a fair comparison window so we measure the METHOD, not when/how it ran:
#   SCOPE  = survey records only (is_survey = TRUE)
#   SEASON = March-October (the months lethal netting ran)
#   YEARS  = 2021-2023 (the only years lethal netting ran; non-lethal photography continued
#            2024-2026 with no lethal counterpart, so those years are clipped out).
# Untagged on-transect iNat records (no surveyor) are dropped, so non-lethal = attributed
# survey photos. NOTE: in this fair window every intern record is a specimen (lethal) and
# every beeple record is a photo (non-lethal), so "yield by method" and "yield by surveyor
# group" would be the SAME two bars -- the group figure was dropped as redundant.
#
# TWO figures (species + genus rank), 4 panels each: n_records (effort), taxa recorded,
#   taxa_per_100_records (efficiency), method-exclusive taxa (found ONLY by that method).
#   * SPECIES-level  = detection + ID resolution (a record must be keyed to species)
#   * GENUS-level    = detection alone (photos not penalised for stalling at genus)
# Descriptive counts -- no hypothesis test (the coverage-standardized richness comparison
# lives in rarefaction_inext.R / rarefaction_vegan.R; method x ID resolution in Q2).
#
# Run from the repo root:  Rscript scripts/analysis/coverage_yield_by_method.R
# Depends on: dplyr, stringr, ggplot2 (+ config.R, theme_beescabr.R).
# =============================================================

for (pkg in c("ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_DIR       <- "data/analysis/coverage"
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
WINDOW_MONTHS <- 3:10                                   # Mar-Oct: the lethal-netting season
WINDOW_YEARS  <- 2021:2023                              # the only years lethal netting ran (fair vs non-lethal)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"

# ---- 1. pool records with method + taxonomy keys + month/year ---------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

prep <- function(df, method) {
  st <- str_squish(tolower(as.character(df$surveyor_type)))
  st[is.na(st) | st == ""] <- "unattributed"
  data.frame(
    method     = method,
    surveyor   = st,
    is_survey  = is_true(df$is_survey),
    month      = suppressWarnings(as.integer(substr(df$observed_on, 6, 7))),
    year       = suppressWarnings(as.integer(substr(df$observed_on, 1, 4))),
    taxon_rank = df$taxon_rank, genus = df$genus, species = df$species,
    stringsAsFactors = FALSE) %>%
    mutate(
      species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                             !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
      genus_key   = ifelse(taxon_rank %in% GENUS_RANKS & !is.na(genus) & genus != "", genus, NA))
}
# FAIR window: survey records only, Mar-Oct, 2021-2023, untagged non-lethal dropped.
rec <- bind_rows(prep(spec, "lethal"), prep(inat, "nonlethal")) %>%
  filter(is_survey, !is.na(month), month %in% WINDOW_MONTHS,
         !is.na(year), year %in% WINDOW_YEARS, surveyor != "unattributed")
message(sprintf("Survey records in the fair window (Mar-Oct, 2021-2023): %d (lethal %d, non-lethal %d)",
                nrow(rec), sum(rec$method == "lethal"), sum(rec$method == "nonlethal")))

# ---- 2. yield metrics (both species- and genus-level) -----------------------
# species_key needs a species/subspecies ID; genus_key counts anything that pins a genus
# (species/subgenus/complex/genus), so the genus metrics DON'T penalise photos ID'd only to
# genus -- a cleaner read on DETECTION, separate from ID resolution.
yield_tbl <- function(d) {
  excl_sp <- d %>% filter(!is.na(species_key)) %>% distinct(method, species_key) %>%
    count(species_key) %>% filter(n == 1) %>% pull(species_key)
  excl_gn <- d %>% filter(!is.na(genus_key)) %>% distinct(method, genus_key) %>%
    count(genus_key) %>% filter(n == 1) %>% pull(genus_key)
  d %>% group_by(method) %>%
    summarise(
      n_records         = n(),
      species           = n_distinct(species_key[!is.na(species_key)]),
      genera            = n_distinct(genus_key[!is.na(genus_key)]),
      exclusive_species = n_distinct(species_key[!is.na(species_key) & species_key %in% excl_sp]),
      exclusive_genera  = n_distinct(genus_key[!is.na(genus_key) & genus_key %in% excl_gn]),
      .groups = "drop") %>%
    mutate(species_per_100_records = round(100 * species / n_records, 1),
           genera_per_100_records  = round(100 * genera  / n_records, 1))
}
tbl <- yield_tbl(rec)
tbl <- tbl[match(c("lethal", "nonlethal"), tbl$method), ]; tbl <- tbl[!is.na(tbl$method), ]
write.csv(tbl, file.path(OUT_DIR, "coverage_yield_by_method.csv"), row.names = FALSE)
message("\nYield by METHOD (survey-only, Mar-Oct 2021-2023):"); print(as.data.frame(tbl), row.names = FALSE)

# ---- 3. one 4-panel yield figure (rank = "species" or "genus") --------------
plot_yield <- function(tbl, title, excl_label, rank = c("species", "genus"), file, w = 7.5) {
  rank <- match.arg(rank)
  tbl$method <- factor(tbl$method, levels = c("lethal", "nonlethal"))   # lethal LEFT, non-lethal RIGHT
  metrics <- if (rank == "species")
    c(n_records = "Records", species = "Species recorded",
      species_per_100_records = "Species / 100 records", exclusive_species = excl_label)
  else
    c(n_records = "Records", genera = "Genera recorded",
      genera_per_100_records = "Genera / 100 records", exclusive_genera = excl_label)
  long <- do.call(rbind, lapply(names(metrics), function(m)
    data.frame(method = tbl$method, metric = metrics[[m]], value = tbl[[m]], stringsAsFactors = FALSE)))
  long$metric <- factor(long$metric, levels = unname(metrics))
  g <- ggplot(long, aes(x = method, y = value, fill = method)) +
    geom_col(width = 0.62) +
    geom_text(aes(label = value), vjust = -0.35, size = 2.7, colour = BEE_INK$secondary) +
    facet_wrap(~ metric, scales = "free_y") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    scale_fill_manual(values = BEE_METHOD_COL, labels = BEE_METHOD_LABEL, name = "method") +
    labs(title = title,
         caption = scope_cap("survey records only, Mar-Oct + 2021-2023 (years both methods ran)",
                             "lethal (specimen net) vs non-lethal (iNat photo)", rank),
         x = NULL, y = NULL) +
    theme_beescabr(11) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),   # methods named in the legend
          panel.grid.major.x = element_blank())
  ggsave(file, g, width = w, height = 6, dpi = 200, bg = "white")
}

# species-level (detection + ID resolution) ...
plot_yield(tbl, "Q11 - Yield by method: SPECIES-level (CABR bees)",
           "Method-exclusive species", rank = "species",
           file = file.path(OUT_DIR, "coverage_yield_by_method.png"))
# ... and genus-level (detection alone -- photos not penalised for stalling at genus)
plot_yield(tbl, "Q11 - Yield by method: GENUS-level (CABR bees)",
           "Method-exclusive genera", rank = "genus",
           file = file.path(OUT_DIR, "coverage_yield_by_method_genus.png"))

message("\nWrote coverage_yield_by_method.csv + .png + _genus.png (species + genus levels) to ", OUT_DIR)
