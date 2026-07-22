# =============================================================
# Q13 -- Effort calendar: observations and trips by month
# beescabr / Cabrillo National Monument (CABR) native bees
#
# THE QUESTION: when across the year (and across years) does surveying actually
# happen? This is the effort backdrop every seasonal/phenology result sits on --
# apparent "activity" in a month is bounded by whether anyone surveyed that month.
#
# SOURCE: the per-survey log (one row = one survey trip/day). Per month we report
#   * trips           -- number of survey events (rows)
#   * observations    -- sum of recorded observations (n_obs)
#   * obs per trip    -- mean intensity
#   * intern vs beeple and lethal vs non-lethal trip splits
# Plus a year x month grid so coverage gaps are visible across seasons.
#
# The intern survey window is Mar-Sep; this calendar shows exactly how effort
# tapers outside it, so downstream seasonal comparisons stay honest.
# Descriptive counts -- no hypothesis test, so no p-value.
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
OUT_DIR       <- "data/analysis/phenology"
WINDOW_MONTHS <- 3:9
MONTH_ABB     <- month.abb
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
scope_cap <- function(scope, method, rank) sprintf("Scope: %s  |  Method: %s  |  Rank: %s",
                                                   scope, method, rank)

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
  filter(!is.na(month))
message(sprintf("Survey trips: %d across %d-%d", nrow(p), min(p$year, na.rm=TRUE), max(p$year, na.rm=TRUE)))

# ---- 2. monthly effort table -------------------------------------------------
by_month <- p %>% group_by(month) %>%
  summarise(trips           = n(),
            observations    = sum(n_obs, na.rm = TRUE),
            obs_per_trip     = round(mean(n_obs, na.rm = TRUE), 1),
            intern_trips     = sum(role == "intern"),
            beeple_trips     = sum(role == "beeple"),
            lethal_trips     = sum(method == "lethal"),
            nonlethal_trips  = sum(method == "non-lethal"),
            .groups = "drop") %>%
  right_join(data.frame(month = 1:12), by = "month") %>%
  mutate(across(-month, ~ ifelse(is.na(.), 0, .)),
         month_lab = factor(MONTH_ABB[month], levels = MONTH_ABB),
         in_window = month %in% WINDOW_MONTHS) %>%
  arrange(month)
write.csv(by_month[, c("month","month_lab","trips","observations","obs_per_trip",
                       "intern_trips","beeple_trips","lethal_trips","nonlethal_trips")],
          file.path(OUT_DIR, "effort_by_month.csv"), row.names = FALSE)
message("Trips in Mar-Sep window: ",
        sum(by_month$trips[by_month$in_window]), " of ", sum(by_month$trips),
        sprintf(" (%.0f%%)", 100*sum(by_month$trips[by_month$in_window])/sum(by_month$trips)))

# ---- 3. figure A: trips + observations by month (window shaded) --------------
long <- bind_rows(
  data.frame(month_lab = by_month$month_lab, metric = "Survey trips",  value = by_month$trips,        in_window = by_month$in_window),
  data.frame(month_lab = by_month$month_lab, metric = "Observations",  value = by_month$observations, in_window = by_month$in_window))
long$metric <- factor(long$metric, levels = c("Survey trips", "Observations"))
gA <- ggplot(long, aes(x = month_lab, y = value, fill = in_window)) +
  geom_col(width = 0.72) +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("TRUE" = "#1a9850", "FALSE" = "#cccccc"),
                    labels = c("TRUE" = "Mar-Sep window", "FALSE" = "outside window"), name = NULL) +
  labs(title = "Q13 - Survey effort calendar (CABR bees)",
       subtitle = str_wrap(scope_cap("per-survey log, all trips 2021-2026",
                            "lethal + non-lethal trips pooled", "n/a (effort)"), 80),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "#b2182b", size = 9),
        legend.position = "top", panel.grid.major.x = element_blank())
ggsave(file.path(OUT_DIR, "effort_by_month.png"), gA, width = 8.5, height = 6.5, dpi = 200, bg = "white")

# ---- 4. figure B: year x month trip-count grid (coverage gaps) ---------------
grid <- p %>% filter(!is.na(year)) %>% count(year, month, name = "trips") %>%
  right_join(expand.grid(year = sort(unique(p$year[!is.na(p$year)])), month = 1:12),
             by = c("year","month")) %>%
  mutate(trips = ifelse(is.na(trips), 0, trips),
         month_lab = factor(MONTH_ABB[month], levels = MONTH_ABB))
gB <- ggplot(grid, aes(x = month_lab, y = factor(year), fill = trips)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(trips > 0, trips, "")), size = 3, color = "grey15") +
  scale_fill_gradient(low = "#f7fcf5", high = "#1a9850", name = "trips") +
  labs(title = "Q13 - Survey trips by year x month (coverage gaps)",
       subtitle = scope_cap("per-survey log", "all trips", "n/a (effort)"),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "#b2182b"), panel.grid = element_blank())
ggsave(file.path(OUT_DIR, "effort_year_month_grid.png"), gB, width = 9, height = 4.6, dpi = 200, bg = "white")
message("Wrote effort_by_month.{csv,png} + effort_year_month_grid.png to ", OUT_DIR)
