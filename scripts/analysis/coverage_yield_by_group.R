# =============================================================
# Q11 -- Yield by SURVEYOR GROUP (who logged the park's bees, 2021-2023?)
# beescabr / Cabrillo National Monument (CABR) native bees
#
# WINDOW: March-October 2021-2023 (the years/season lethal netting ran).
# This figure is a CONTRIBUTION view -- it includes CASUAL public records, so it is NOT
# limited to structured surveys. Three contributor groups:
#   * interns (lethal)      -- net specimens (a structured survey; interns only collect)
#   * beeple (non-lethal)   -- iNaturalist photos by the beeple community-science group (survey)
#   * strangers (non-lethal)-- casual iNaturalist photos by the public, NOT on the surveyor
#                              roster and NOT part of a survey (is_survey = FALSE)
# Intern iNaturalist photos are EXCLUDED (there are none in 2021-2023; interns only
# photographed from 2024 on). NOTE the scope difference from coverage_yield_by_method.R,
# which is SURVEY-ONLY (structured lethal vs non-lethal) and so excludes strangers.
#
# TWO figures (species + genus rank), 4 panels each: n_records (effort), taxa recorded,
#   taxa_per_100_records (efficiency), group-exclusive taxa (found ONLY by that group).
# Descriptive counts -- no hypothesis test.
#
# Run from the repo root:  Rscript scripts/analysis/coverage_yield_by_group.R
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
WINDOW_MONTHS <- 3:10
WINDOW_YEARS  <- 2021:2023
GRPS   <- c("interns (lethal)", "beeple (non-lethal)", "strangers (non-lethal)")   # lethal first; casual last
GRP_COL <- setNames(c(BEE_METHOD_COL[["lethal"]], BEE_METHOD_COL[["nonlethal"]], BEE_INK[["muted"]]), GRPS)
SUBTITLE <- "interns = lethal net specimens (survey) | beeple = non-lethal survey photos | strangers = casual iNaturalist photos by the public (NOT a survey)"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
norm <- function(x) str_squish(tolower(as.character(x)))

# ---- 1. pool records, tag contributor group + taxonomy keys + month/year ----
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

keys <- function(df) df %>% mutate(
  taxon_rank = tolower(taxon_rank),
  species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                         !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
  genus_key   = ifelse(taxon_rank %in% GENUS_RANKS & !is.na(genus) & genus != "", genus, NA),
  month = suppressWarnings(as.integer(substr(observed_on, 6, 7))),
  year  = suppressWarnings(as.integer(substr(observed_on, 1, 4))))

sp <- keys(spec) %>% transmute(grp = "interns (lethal)", taxon_rank, species_key, genus_key, month, year)
iu <- keys(inat) %>% mutate(st = norm(surveyor_type),
        grp = case_when(st == "beeple" ~ "beeple (non-lethal)",
                        st == "intern" ~ "intern-photo-EXCLUDE",     # interns' iNat photos -- dropped
                        TRUE           ~ "strangers (non-lethal)")) %>%
      transmute(grp, taxon_rank, species_key, genus_key, month, year)

rec <- bind_rows(sp, iu) %>%
  filter(grp %in% GRPS, !is.na(month), month %in% WINDOW_MONTHS, !is.na(year), year %in% WINDOW_YEARS)
rec$grp <- factor(rec$grp, levels = GRPS)
message(sprintf("Records by group (Mar-Oct 2021-2023): %s",
                paste(sprintf("%s=%d", levels(rec$grp), tabulate(rec$grp)), collapse = ", ")))

# ---- 2. yield metrics (species- and genus-level) per group ------------------
yield_tbl <- function(d) {
  excl_sp <- d %>% filter(!is.na(species_key)) %>% distinct(grp, species_key) %>%
    count(species_key) %>% filter(n == 1) %>% pull(species_key)
  excl_gn <- d %>% filter(!is.na(genus_key)) %>% distinct(grp, genus_key) %>%
    count(genus_key) %>% filter(n == 1) %>% pull(genus_key)
  d %>% group_by(grp, .drop = FALSE) %>%
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
tbl <- tbl[match(GRPS, tbl$grp), ]
write.csv(tbl, file.path(OUT_DIR, "coverage_yield_by_group.csv"), row.names = FALSE)
message("\nYield by SURVEYOR GROUP (Mar-Oct 2021-2023):"); print(as.data.frame(tbl), row.names = FALSE)

# ---- 3. one 4-panel yield figure (rank = "species" or "genus") --------------
plot_yield <- function(tbl, title, excl_label, rank = c("species", "genus"), file, w = 8) {
  rank <- match.arg(rank)
  tbl$grp <- factor(tbl$grp, levels = GRPS)
  metrics <- if (rank == "species")
    c(n_records = "Records", species = "Species recorded",
      species_per_100_records = "Species / 100 records", exclusive_species = excl_label)
  else
    c(n_records = "Records", genera = "Genera recorded",
      genera_per_100_records = "Genera / 100 records", exclusive_genera = excl_label)
  long <- do.call(rbind, lapply(names(metrics), function(m)
    data.frame(grp = tbl$grp, metric = metrics[[m]], value = tbl[[m]], stringsAsFactors = FALSE)))
  long$metric <- factor(long$metric, levels = unname(metrics))
  g <- ggplot(long, aes(x = grp, y = value, fill = grp)) +
    geom_col(width = 0.66) +
    geom_text(aes(label = value), vjust = -0.35, size = 2.7, colour = BEE_INK$secondary) +
    facet_wrap(~ metric, scales = "free_y") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    scale_fill_manual(values = GRP_COL, name = "surveyor group") +
    labs(title = title, subtitle = str_wrap(SUBTITLE, 96),
         caption = scope_cap("ALL records incl. casual public (NOT survey-only), Mar-Oct 2021-2023",
                             "lethal specimens (interns) + non-lethal photos (beeple + public)", rank),
         x = NULL, y = NULL) +
    theme_beescabr(11) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          plot.subtitle = element_text(size = 8.5),
          panel.grid.major.x = element_blank())
  ggsave(file, g, width = w, height = 6.2, dpi = 200, bg = "white")
}

plot_yield(tbl, "Q11 - Yield by surveyor group: SPECIES-level (CABR bees)",
           "Group-exclusive species", rank = "species",
           file = file.path(OUT_DIR, "coverage_yield_by_group.png"))
plot_yield(tbl, "Q11 - Yield by surveyor group: GENUS-level (CABR bees)",
           "Group-exclusive genera", rank = "genus",
           file = file.path(OUT_DIR, "coverage_yield_by_group_genus.png"))

message("\nWrote coverage_yield_by_group.csv + .png + _genus.png (species + genus levels) to ", OUT_DIR)
