# =============================================================
# analysis/coverage/records_per_genus_by_evidence.R
# beescabr pipeline -- CABR native bees: how much evidence backs each GENUS,
# split by the two survey METHODS.
# Created: 2026-07-30
#
# THE QUESTION: for every bee genus recorded at Cabrillo, how many records do we
# hold, and which METHOD produced them?
#   * lethal      -- a physical specimen (net collection). Voucher-backed.
#   * non-lethal  -- an iNaturalist photo (research-grade AND needs-ID pooled together).
#
# This is the park-wide companion to coverage_cabr_vs_holway.R (which shows only
# the not-on-Holway taxa): here EVERY genus is shown, so a reader can see at a glance
# which genera rest on netted specimens vs which rest on photos.
#
# A genus with few TOTAL records is flagged (*) -- its evidence base is thin, so
# read its composition with care (matches the project's >=10-record rule of thumb).
#
# Run from the repo root:  source("scripts/analysis/coverage/records_per_genus_by_evidence.R")
# Depends on: dplyr, stringr, ggplot2 (+ config.R, theme_beescabr.R).
# =============================================================

# Dependencies are CHECKED here, not installed: see beescabr_require() in config.R.
if (!exists("beescabr_require")) source("scripts/config.R")
beescabr_require()
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/shared/theme_beescabr.R")   # shared house style
OUT_JOURNAL <- file.path(DIR_JOURNAL, "coverage/records_by_evidence/fair_method_2021_2023")
OUT_REPORT  <- file.path(DIR_REPORT,  "coverage/records_by_evidence")
MIN_REPORT  <- 50    # report (all records) figure cutoff
MIN_JOURNAL <- 25    # journal (fair window) figure cutoff -- fewer records, so a lower bar
dir.create(OUT_JOURNAL, recursive = TRUE, showWarnings = FALSE); dir.create(OUT_REPORT, recursive = TRUE, showWarnings = FALSE)
norm <- function(x) str_squish(as.character(x))
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"

# ---- 1. pool records with a genus, tag METHOD + fair-window fields -----------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
grab <- function(df, method) {
  st <- str_squish(tolower(as.character(df$surveyor_type))); st[is.na(st) | st == ""] <- "unattributed"
  d <- data.frame(genus = norm(df$genus), method = method,
                  surv = is_true(df$is_survey), st = st,
                  mo = suppressWarnings(as.integer(substr(df$observed_on, 6, 7))),
                  yr = suppressWarnings(as.integer(substr(df$observed_on, 1, 4))),
                  stringsAsFactors = FALSE)
  d[!is.na(d$genus) & d$genus != "", ]
}
rec_all  <- bind_rows(grab(spec, "lethal"), grab(inat, "nonlethal"))
# fair window (journal): survey-only, Mar-Oct 2021-2023, attributed
rec_fair <- rec_all %>% filter(surv, mo %in% FAIR_MONTHS, yr %in% FAIR_YEARS, st != "unattributed")

# ---- 2. figure builder: per-genus method composition, trimmed by threshold ---
# SPLIT figure: report = ALL records (>= MIN_REPORT); journal = FAIR WINDOW (>= MIN_JOURNAL).
# The long tail is dropped from the FIGURE for readability -- the full list stays in the CSV.
make_fig <- function(rec, min_shown, scope_lab, out_png, out_csv) {
  wide <- rec %>% count(genus, method, name = "n") %>%
    tidyr::pivot_wider(names_from = method, values_from = n, values_fill = 0)
  for (col in c("lethal", "nonlethal")) if (is.null(wide[[col]])) wide[[col]] <- 0L
  wide <- wide %>% mutate(total = lethal + nonlethal) %>% arrange(desc(total))
  write.csv(wide, out_csv, row.names = FALSE)
  wf  <- wide %>% filter(total >= min_shown)
  lab <- wf$genus
  long <- bind_rows(
    data.frame(genus = wf$genus, method = "lethal",    n = wf$lethal),
    data.frame(genus = wf$genus, method = "nonlethal", n = wf$nonlethal))
  long$method <- factor(long$method, levels = c("lethal", "nonlethal"))
  long$genus  <- factor(long$genus, levels = rev(lab))
  g <- ggplot(long, aes(x = n, y = genus, fill = method)) +
    # house rule: GENUS-rank figures are hatched (species figures stay solid) -- this one is per-genus
    ggpattern::geom_col_pattern(width = 0.74, position = position_stack(reverse = TRUE),
      pattern = "stripe", pattern_fill = "white", pattern_colour = NA, pattern_angle = 45,
      pattern_density = 0.08, pattern_spacing = 0.02, pattern_key_scale_factor = 0.4) +
    geom_text(data = wf, aes(x = total, y = factor(genus, levels = rev(lab)), label = total),
              hjust = -0.2, size = 2.7, colour = BEE_INK$secondary, inherit.aes = FALSE) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
    scale_fill_manual(values = BEE_METHOD_COL, labels = BEE_METHOD_LABEL, name = "method") +
    labs(title = "How much evidence backs each bee genus?",
         subtitle = "Evidence depth varies widely -- some genera are specimen-backed, others rest on iNaturalist photos alone.",
         caption = scope_cap(
           scope  = sprintf("%s; genera with >= %d records (%d of %d shown; full list in the CSV)", scope_lab, min_shown, nrow(wf), nrow(wide)),
           method = "lethal vs non-lethal", rank = "genus"),
         x = "records", y = "bee genus") +
    theme_beescabr(11) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
          axis.text.y = element_text(face = "italic"),   # bee genus names = scientific -> italic
          panel.grid.major.y = element_blank())
  bee_ggsave(out_png, g, width = 9, height = max(5, 0.34 * nrow(wf) + 1.7), bg = "white")
  message(sprintf("  %-32s %d of %d genera >= %d records", scope_lab, nrow(wf), nrow(wide), min_shown))
}
make_fig(rec_all,  MIN_REPORT,  "All records (report)",
         file.path(OUT_REPORT, "bee_genus_evidence_depth.png"),
         file.path(OUT_REPORT, "bee_genus_evidence_depth.csv"))
make_fig(rec_fair, MIN_JOURNAL, "Fair window: Mar-Oct 2021-2023 (journal)",
         file.path(OUT_JOURNAL, "records_by_evidence_journal_genus.png"),
         file.path(OUT_JOURNAL, "records_by_evidence_journal_genus.csv"))
message("\nWrote records_by_evidence_{report,journal}_genus.{csv,png} to journal_paper_2026/ + nps_report_2026/")
