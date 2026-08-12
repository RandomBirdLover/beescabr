# =============================================================
# analysis/coverage_effort_by_method.R
# beescabr -- EFFORT by method: number of SURVEY TRIPS, lethal (net) vs non-lethal (photo).
#
# Effort = the sampling WORK done (survey trips), NOT the catch. It is the honest
# denominator for interpreting yield: lethal and non-lethal did very different
# amounts of sampling, so raw yield / a naive per-record "efficiency" is misleading
# (see coverage_yield_by_method + the rarefaction figures). This figure states the
# effort plainly so the yield and rarefaction comparisons can be read fairly.
#
# JOURNAL ONLY (method comparison): restricted to the FAIR WINDOW (FAIR_MONTHS/
# FAIR_YEARS in config.R = Mar-Oct 2021-2023) so non-lethal isn't credited with trips
# outside the lethal-netting years.
# TRANSECT CAVEAT: a lethal (net) survey trip covers all 3 transects, whereas a
# non-lethal (photo) survey trip covers only one transect -- so raw trip counts
# understate lethal's per-trip coverage. Stated in the caption.
#
# SOURCE: the per-survey log (one row = one survey trip), which tags each trip's method.
# Run from the repo root:  Rscript scripts/analysis/coverage_effort_by_method.R
# Depends on: dplyr, stringr, ggplot2 (+ config.R).
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_DIR <- file.path(DIR_JOURNAL, "method_comparison/effort")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 1. trips per method from the per-survey log (FAIR WINDOW) ---------------
# Journal figure -> restrict to the shared fair window (FAIR_MONTHS/FAIR_YEARS in
# config.R) so non-lethal isn't credited with trips outside the lethal-netting years.
psf <- if (!is.null(PATHS$per_survey)) PATHS$per_survey else "data/project_info/master_per_survey_info.csv"
p <- read.csv(psf, stringsAsFactors = FALSE, check.names = FALSE)
p$method <- str_squish(tolower(p$method))
.d  <- as.Date(p$date)
p$mo <- as.integer(format(.d, "%m")); p$yr <- as.integer(format(.d, "%Y"))
tr <- p %>% filter(method %in% c("lethal", "non-lethal"),
                   mo %in% FAIR_MONTHS, yr %in% FAIR_YEARS) %>% count(method, name = "trips")
tr$method <- factor(tr$method, levels = c("lethal", "non-lethal"))
write.csv(tr, file.path(OUT_DIR, "coverage_effort_by_method.csv"), row.names = FALSE)
message(sprintf("Survey trips by method: %s",
                paste(sprintf("%s=%d", tr$method, tr$trips), collapse = "  ")))

# ---- 2. figure: survey trips per method -------------------------------------
fill_cols <- c(lethal = unname(BEE_METHOD_COL["lethal"]), "non-lethal" = unname(BEE_METHOD_COL["nonlethal"]))
g <- ggplot(tr, aes(x = method, y = trips, fill = method)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = trips), vjust = -0.35, size = 4.2, colour = BEE_INK$secondary) +
  scale_fill_manual(values = fill_cols, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(title = "Effort by Method",
       subtitle = sprintf("Non-lethal photos logged far more survey trips than lethal netting (%d vs %d) -- the effort behind the catch.",
                          tr$trips[tr$method == "non-lethal"], tr$trips[tr$method == "lethal"]),
       caption = paste0(
         str_wrap(paste0(
           "Note: a lethal (net) trip covers all 3 transects, while a non-lethal (photo) trip covers one -- ",
           "so raw trip counts understate lethal's per-trip coverage."), 108),
         "\n",
         scope_cap(scope  = "fair window: survey trips only, Mar-Oct 2021-2023 (excludes trips outside the lethal-netting years)",
                   method = "lethal vs non-lethal",
                   rank   = "trips (survey effort)")),
       x = NULL, y = "survey trips") +
  theme_beescabr(12) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank())
bee_ggsave(file.path(OUT_DIR, "coverage_effort_by_method.png"), g, width = 6.5, height = 5, bg = "white")
message("Wrote coverage_effort_by_method.{csv,png} to ", OUT_DIR)
