# =============================================================
# analysis/records_per_species_by_evidence.R
# beescabr pipeline -- CABR native bees: how many records back each SPECIES,
# split by the two survey METHODS. Species-level companion to
# records_per_genus_by_evidence.R (same look, one row per species).
#
#   * lethal      -- a physical specimen (net collection). Voucher-backed.
#   * non-lethal  -- an iNaturalist photo (research-grade AND needs-ID pooled together).
#
# Only records identified to species (species/subspecies rank) appear -- a genus-only
# record can't be placed on a species row. A species with few TOTAL records is flagged
# (*) -- thin evidence base, read its composition with care (>=10-record rule of thumb).
#
# Run from the repo root:  Rscript scripts/analysis/records_per_species_by_evidence.R
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
OUT_JOURNAL   <- file.path(DIR_JOURNAL, "coverage/records_by_evidence")
OUT_REPORT    <- file.path(DIR_REPORT,  "coverage/records_by_evidence")
SPECIES_RANKS <- c("species", "subspecies")
MIN_REPORT    <- 50    # report (all records) figure cutoff
MIN_JOURNAL   <- 25    # journal (fair window) figure cutoff -- fewer records, so a lower bar
dir.create(OUT_JOURNAL, recursive = TRUE, showWarnings = FALSE); dir.create(OUT_REPORT, recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"
norm <- function(x) str_squish(as.character(x))

# species binomial (Genus epithet) for species/subspecies-rank rows; NA otherwise
sp_key <- function(df) {
  ok <- tolower(norm(df$taxon_rank)) %in% SPECIES_RANKS & !is.na(df$genus) & norm(df$genus) != "" &
        !is.na(df$species) & norm(df$species) != ""
  ifelse(ok, paste(norm(df$genus), word(norm(df$species), -1)), NA_character_)
}

# ---- 1. pool records, tag METHOD + fair-window fields -----------------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
grab <- function(df, method) {
  st <- str_squish(tolower(as.character(df$surveyor_type))); st[is.na(st) | st == ""] <- "unattributed"
  data.frame(species = sp_key(df), method = method,
             surv = is_true(df$is_survey), st = st,
             mo = suppressWarnings(as.integer(substr(df$observed_on, 6, 7))),
             yr = suppressWarnings(as.integer(substr(df$observed_on, 1, 4))),
             stringsAsFactors = FALSE)
}
rec_all  <- bind_rows(grab(spec, "lethal"), grab(inat, "nonlethal")) %>% filter(!is.na(species))
# fair window (journal): survey-only, Mar-Oct 2021-2023, attributed
rec_fair <- rec_all %>% filter(surv, mo %in% FAIR_MONTHS, yr %in% FAIR_YEARS, st != "unattributed")

# ---- 2. figure builder: per-species method composition, trimmed by threshold ----
# SPLIT figure: report = ALL records (>= MIN_REPORT); journal = FAIR WINDOW (>= MIN_JOURNAL).
# The long tail is dropped from the FIGURE for readability -- the full list stays in the CSV.
make_fig <- function(rec, min_shown, scope_lab, out_png, out_csv) {
  wide <- rec %>% count(species, method, name = "n") %>%
    tidyr::pivot_wider(names_from = method, values_from = n, values_fill = 0)
  for (col in c("lethal", "nonlethal")) if (is.null(wide[[col]])) wide[[col]] <- 0L
  wide <- wide %>% mutate(total = lethal + nonlethal) %>% arrange(desc(total))
  write.csv(wide, out_csv, row.names = FALSE)
  wf  <- wide %>% filter(total >= min_shown)
  lab <- wf$species
  long <- bind_rows(
    data.frame(lab = lab, method = "lethal",    n = wf$lethal),
    data.frame(lab = lab, method = "nonlethal", n = wf$nonlethal))
  long$method <- factor(long$method, levels = c("lethal", "nonlethal"))
  long$lab    <- factor(long$lab, levels = rev(lab))
  g <- ggplot(long, aes(x = n, y = lab, fill = method)) +
    geom_col(width = 0.74, position = position_stack(reverse = TRUE)) +
    geom_text(data = wf, aes(x = total, y = factor(species, levels = rev(lab)), label = total),
              hjust = -0.2, size = 2.7, colour = BEE_INK$secondary, inherit.aes = FALSE) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
    scale_fill_manual(values = BEE_METHOD_COL, labels = BEE_METHOD_LABEL, name = "method") +
    labs(title = "Total Records of Bee Species",
         subtitle = sprintf("%s -- species with >= %d records (%d of %d shown; full list in the CSV)",
                            scope_lab, min_shown, nrow(wf), nrow(wide)),
         x = "Number of records", y = NULL) +
    theme_beescabr(11) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 8, face = "italic"))
  ggsave(out_png, g, width = 9.5, height = max(5, 0.26 * nrow(wf) + 1.7), dpi = 200, bg = "white", limitsize = FALSE)
  message(sprintf("  %-32s %d of %d species >= %d records", scope_lab, nrow(wf), nrow(wide), min_shown))
}
make_fig(rec_all,  MIN_REPORT,  "All records (report)",
         file.path(OUT_REPORT, "records_per_species_by_evidence_report.png"),
         file.path(OUT_REPORT, "records_per_species_by_evidence_report.csv"))
make_fig(rec_fair, MIN_JOURNAL, "Fair window: Mar-Oct 2021-2023 (journal)",
         file.path(OUT_JOURNAL, "records_per_species_by_evidence_journal.png"),
         file.path(OUT_JOURNAL, "records_per_species_by_evidence_journal.csv"))
message("\nWrote records_per_species_by_evidence_{report,journal}.{csv,png} to journal_paper_2026/ + nps_report_2026/")
