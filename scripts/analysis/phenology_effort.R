# =============================================================
# Q13 -- Effort calendar: survey trips and records by month. SPLIT figure.
# beescabr / Cabrillo National Monument (CABR) native bees
#
# THE QUESTION: when across the year (and across years) does surveying happen?
# This is the effort backdrop every seasonal result sits on -- apparent "activity"
# in a month is bounded by whether anyone surveyed that month.
#
# SOURCE: the per-survey log (one row = one survey trip/day), tagged with method
# (lethal / non-lethal) and year.
#
# SPLIT -- both figures are produced for BOTH papers:
#   * JOURNAL -> the FAIR WINDOW (FAIR_MONTHS/FAIR_YEARS = Mar-Oct 2021-2023):
#       calendar shows only the window months, trips + records split by method
#       colour; the year x month grid is restricted to the same window.
#   * REPORT  -> ALL trips, ALL months, ALL years, both methods -- the full effort
#       picture, same method-colour encoding.
# "Observations" is renamed "Records" throughout. Descriptive counts -- no test.
#
# Run from the repo root:  Rscript scripts/analysis/phenology_effort.R
# Depends on: dplyr, stringr, ggplot2 (+ config.R).
# =============================================================

for (pkg in c("ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_SEQ")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_JOURNAL <- file.path(DIR_JOURNAL, "phenology")   # fair-window effort calendar + grid
OUT_REPORT  <- file.path(DIR_REPORT,  "phenology")   # all-records effort calendar + grid
MONTH_ABB <- month.abb
MCOL      <- c("lethal" = unname(BEE_METHOD_COL["lethal"]), "non-lethal" = unname(BEE_METHOD_COL["nonlethal"]))
dir.create(OUT_JOURNAL, recursive = TRUE, showWarnings = FALSE); dir.create(OUT_REPORT, recursive = TRUE, showWarnings = FALSE)
scope_cap <- function(scope, method, rank) sprintf("Scope: %s  |  Method: %s  |  Rank: %s", scope, method, rank)

# ---- 1. read the per-survey log ---------------------------------------------
psf_path <- if (!is.null(PATHS$per_survey)) PATHS$per_survey else
            "data/project_info/master_per_survey_info.csv"
p <- read.csv(psf_path, stringsAsFactors = FALSE, check.names = FALSE)
p <- p %>% mutate(
  date  = as.Date(date),
  month = as.integer(format(date, "%m")),
  year  = suppressWarnings(as.integer(year)),
  n_obs = suppressWarnings(as.numeric(n_obs)),
  role   = str_squish(tolower(role)),
  method = str_squish(tolower(method))) %>%
  filter(!is.na(month), method %in% c("lethal", "non-lethal"))
message(sprintf("Survey trips: %d across %d-%d", nrow(p), min(p$year, na.rm=TRUE), max(p$year, na.rm=TRUE)))

# ---- 2. effort calendar (trips + records by month, split by method) ----------
calendar_fig <- function(dat, months_shown, file, subtitle, scope_txt) {
  mm <- dat %>% group_by(month, method) %>%
    summarise(trips = n(), records = sum(n_obs, na.rm = TRUE), .groups = "drop")
  long <- bind_rows(
    data.frame(month = mm$month, method = mm$method, metric = "Survey trips", value = mm$trips),
    data.frame(month = mm$month, method = mm$method, metric = "Records",      value = mm$records))
  long$metric    <- factor(long$metric, levels = c("Survey trips", "Records"))
  long$method    <- factor(long$method, levels = c("lethal", "non-lethal"))
  long$month_lab <- factor(MONTH_ABB[long$month], levels = MONTH_ABB[months_shown])
  long <- long[!is.na(long$month_lab), ]
  # write the backing table
  wide <- mm; wide$month_lab <- MONTH_ABB[wide$month]
  write.csv(wide[order(wide$month, wide$method),
                 c("month", "month_lab", "method", "trips", "records")],
            sub("\\.png$", ".csv", file), row.names = FALSE)
  g <- ggplot(long, aes(x = month_lab, y = value, fill = method)) +
    geom_col(width = 0.72) +   # stacked by method
    facet_wrap(~ metric, scales = "free_y", ncol = 1) +
    scale_x_discrete(drop = FALSE) +
    scale_fill_manual(values = MCOL, name = NULL) +
    labs(title = "Survey Effort Calendar", subtitle = subtitle,
         caption = str_wrap(scope_cap(scope_txt, "lethal + non-lethal (by colour)", "n/a (effort)"), 84),
         x = NULL, y = NULL) +
    theme_beescabr(11) +
    theme(legend.position = "top", panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  ggsave(file, g, width = 8.5, height = 6.5, dpi = 200, bg = "white")
}

# ---- 3. year x month trip-count grid ----------------------------------------
grid_fig <- function(dat, months_shown, file, subtitle) {
  yrs <- sort(unique(dat$year[!is.na(dat$year)]))
  grid <- dat %>% filter(!is.na(year)) %>% count(year, month, name = "trips") %>%
    right_join(expand.grid(year = yrs, month = months_shown), by = c("year", "month")) %>%
    mutate(trips = ifelse(is.na(trips), 0, trips),
           month_lab = factor(MONTH_ABB[month], levels = MONTH_ABB[months_shown]),
           dark = trips > max(trips, na.rm = TRUE) * 0.5)
  g <- ggplot(grid, aes(x = month_lab, y = factor(year), fill = trips)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = ifelse(trips > 0, trips, ""), colour = dark), size = 3, show.legend = FALSE) +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = BEE_INK$primary), guide = "none") +
    scale_fill_gradientn(colors = BEE_SEQ, name = "trips") +
    labs(title = "Survey Trips by Year and Month", subtitle = subtitle,
         caption = scope_cap("per-survey log", "lethal + non-lethal pooled", "n/a (effort)"),
         x = NULL, y = NULL) +
    theme_beescabr(11) +
    theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  ggsave(file, g, width = 9, height = 4.6, dpi = 200, bg = "white")
}

# ---- 4. run both scopes -----------------------------------------------------
p_journal <- p %>% filter(month %in% FAIR_MONTHS, year %in% FAIR_YEARS)
all_months <- sort(unique(p$month))

calendar_fig(p_journal, FAIR_MONTHS, file.path(OUT_JOURNAL, "effort_by_month_journal.png"),
             "Fair window: Mar-Oct 2021-2023, trips + records split by method",
             "per-survey log, fair window (Mar-Oct 2021-2023)")
calendar_fig(p, all_months, file.path(OUT_REPORT, "effort_by_month_report.png"),
             sprintf("All trips %d-%d, all months, split by method", min(p$year, na.rm=TRUE), max(p$year, na.rm=TRUE)),
             sprintf("per-survey log, all trips %d-%d", min(p$year, na.rm=TRUE), max(p$year, na.rm=TRUE)))

grid_fig(p_journal, FAIR_MONTHS, file.path(OUT_JOURNAL, "effort_year_month_grid_journal.png"),
         "Fair window: Mar-Oct 2021-2023")
grid_fig(p, all_months, file.path(OUT_REPORT, "effort_year_month_grid_report.png"),
         sprintf("All trips %d-%d", min(p$year, na.rm=TRUE), max(p$year, na.rm=TRUE)))

message(sprintf("Journal window trips: %d | Report (all) trips: %d", nrow(p_journal), nrow(p)))
message("Wrote effort_by_month_{journal,report}.{png,csv} + effort_year_month_grid_{journal,report}.png to journal_paper_2026/ + nps_report_2026/")
