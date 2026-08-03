# =============================================================
# Q11 -- Yield: who/what finds the most, on a FAIR footing?
# beescabr / Cabrillo National Monument (CABR) native bees
#
# Two companion figures, BOTH restricted to a fair comparison window:
#   SCOPE  = survey records only (is_survey = TRUE)
#   SEASON = March-October (the months lethal netting actually ran; non-lethal
#            photography runs year-round, so we clip it to the same window).
# Without both clips the comparison would partly measure WHEN/HOW each ran, not yield.
#
#   FIGURE A -- yield by METHOD:  lethal (net) vs non-lethal (iNat photo).
#               Untagged on-transect iNat records (no surveyor) are dropped, so
#               non-lethal = attributed survey photos (beeple + interns).
#   FIGURE B -- yield by SURVEYOR GROUP:  beeple (non-lethal photos) vs
#               intern (lethal specimens only) -- each program by its primary method.
#
# METRICS per bar (4 panels): n_records (effort), species (distinct taxa),
#   species_per_100_records (efficiency), exclusive_species (found ONLY by that
#   method/group in this window -- what would be missed without it).
# Descriptive counts -- no hypothesis test (method's effect on ID resolution is
# tested in coverage_method_venn.R / Q2).
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
WINDOW_MONTHS <- 3:10                                   # Mar-Oct: the lethal-netting season
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"

# ---- 1. pool records with group + method + taxonomy keys + month ------------
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
    taxon_rank = df$taxon_rank, genus = df$genus, species = df$species,
    stringsAsFactors = FALSE) %>%
    mutate(
      species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                             !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
      genus_key   = ifelse(taxon_rank %in% GENUS_RANKS & !is.na(genus) & genus != "", genus, NA))
}
# FAIR window: survey records only, Mar-Oct
rec <- bind_rows(prep(spec, "lethal"), prep(inat, "nonlethal")) %>%
  filter(is_survey, !is.na(month), month %in% WINDOW_MONTHS)
message(sprintf("Survey records in the Mar-Oct window: %d (lethal %d, non-lethal %d)",
                nrow(rec), sum(rec$method == "lethal"), sum(rec$method == "nonlethal")))

# ---- 2. yield metrics for a grouping column (d must carry a `grp` column) ----
yield_tbl <- function(d) {
  excl_sp <- d %>% filter(!is.na(species_key)) %>% distinct(grp, species_key) %>%
    count(species_key) %>% filter(n == 1) %>% pull(species_key)
  d %>% group_by(grp) %>%
    summarise(
      n_records         = n(),
      species           = n_distinct(species_key[!is.na(species_key)]),
      genera            = n_distinct(genus_key[!is.na(genus_key)]),
      exclusive_species = n_distinct(species_key[!is.na(species_key) & species_key %in% excl_sp]),
      .groups = "drop") %>%
    mutate(species_per_100_records = round(100 * species / n_records, 1))
}

# ---- 3. one 4-panel yield figure -------------------------------------------
plot_yield <- function(tbl, title, excl_label, fill_vals, fill_labels, fill_name, file, w = 7.5) {
  metrics <- c(n_records = "Records", species = "Species recorded",
               species_per_100_records = "Species / 100 records",
               exclusive_species = excl_label)
  long <- do.call(rbind, lapply(names(metrics), function(m)
    data.frame(grp = tbl$grp, metric = metrics[[m]], value = tbl[[m]], stringsAsFactors = FALSE)))
  long$metric <- factor(long$metric, levels = unname(metrics))
  long$grp    <- factor(long$grp, levels = tbl$grp)
  g <- ggplot(long, aes(x = grp, y = value, fill = grp)) +
    geom_col(width = 0.62) +
    geom_text(aes(label = value), vjust = -0.35, size = 2.7, colour = BEE_INK$secondary) +
    facet_wrap(~ metric, scales = "free_y") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    scale_fill_manual(values = fill_vals, labels = fill_labels, name = fill_name) +
    labs(title = title,
         caption = scope_cap("survey records only, Mar-Oct window (fair comparison)",
                             "lethal (specimen net) vs non-lethal (iNat photo)", "species"),
         x = NULL, y = NULL) +
    theme_beescabr(11) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),   # groups named in the legend -- no need to repeat on x
          panel.grid.major.x = element_blank())
  ggsave(file, g, width = w, height = 6, dpi = 200, bg = "white")
}

# ---- 4a. FIGURE A -- by method (drop untagged non-lethal) -------------------
recA <- rec %>% filter(surveyor != "unattributed") %>%
  mutate(grp = factor(method, levels = c("lethal", "nonlethal")))
tblA <- yield_tbl(recA)
tblA <- tblA[match(c("lethal", "nonlethal"), tblA$grp), ]; tblA <- tblA[!is.na(tblA$grp), ]
write.csv(tblA, file.path(OUT_DIR, "coverage_yield_by_method.csv"), row.names = FALSE)
message("\nYield by METHOD (survey-only, Mar-Oct):"); print(as.data.frame(tblA), row.names = FALSE)
plot_yield(tblA, "Q11 - Yield by method (CABR bees, survey-only, Mar-Oct)",
           "Method-exclusive species",
           fill_vals = BEE_METHOD_COL, fill_labels = BEE_METHOD_LABEL, fill_name = "method",
           file = file.path(OUT_DIR, "coverage_yield_by_method.png"))

# ---- 4b. FIGURE B -- by surveyor group: beeple photos vs intern specimens ---
recB <- rec %>%
  filter((surveyor == "beeple" & method == "nonlethal") |
         (surveyor == "intern" & method == "lethal")) %>%
  mutate(grp = ifelse(method == "lethal", "intern (lethal)", "beeple (non-lethal)"))
GRP_B <- c("beeple (non-lethal)", "intern (lethal)")
recB$grp <- factor(recB$grp, levels = GRP_B)
tblB <- yield_tbl(recB)
tblB <- tblB[match(GRP_B, tblB$grp), ]; tblB <- tblB[!is.na(tblB$grp), ]
write.csv(tblB, file.path(OUT_DIR, "coverage_yield_by_group.csv"), row.names = FALSE)
message("\nYield by SURVEYOR GROUP (beeple photos vs intern specimens, Mar-Oct):")
print(as.data.frame(tblB), row.names = FALSE)
GRP_COL <- setNames(c(BEE_METHOD_COL[["nonlethal"]], BEE_METHOD_COL[["lethal"]]), GRP_B)
plot_yield(tblB, "Q11 - Yield by surveyor group (CABR bees, survey-only, Mar-Oct)",
           "Group-exclusive species",
           fill_vals = GRP_COL, fill_labels = GRP_B, fill_name = "surveyor group",
           file = file.path(OUT_DIR, "coverage_yield_by_group.png"))

message("\nWrote coverage_yield_by_method.{csv,png} + coverage_yield_by_group.{csv,png} to ", OUT_DIR)
