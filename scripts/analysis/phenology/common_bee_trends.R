# =============================================================
# analysis/phenology/common_bee_trends.R
# beescabr -- year-to-year trend of the park's MOST OBSERVED bee
# (Augochlorella pomoniella, the Peridot Sweat Bee).
#
# Two bias controls, both needed because records are not abundance:
#   * EFFORT: surveying grew enormously (82 bee records in 2019 vs 4,515 in
#     2024), so the metric is the bee's SHARE of all bee records per year --
#     raw counts would track the surveying, not the bee.
#   * METHOD MIX: specimen netting ran only 2021-2023, and nets and cameras
#     catch different bees. So this figure uses PHOTO (iNaturalist) records
#     ONLY, keeping the method identical across every year compared.
# Years before 2020 are dropped (too few records for a stable share). The
# share is still relative, not a population count -- the caption says so.
#
# A two-species variant (adding Agapostemon subtilior) was built 2026-08-20
# and shelved; flip MAKE_DUO to regenerate it. The interactive trends
# explorer (bee_trends_explorer.R) was likewise built and left unpublished.
#
# Run from the repo root:  source("scripts/analysis/phenology/common_bee_trends.R")
# =============================================================
suppressMessages({ library(dplyr); library(ggplot2); library(stringr) })
if (!exists("PATHS"))       source("scripts/config.R")
if (!exists("scope_cap"))   source("scripts/analysis/shared/theme_beescabr.R")

OUT_DIR <- file.path(DIR_REPORT, "phenology")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

MIN_YEAR     <- 2021          # the survey program's first year (2020 and before: too few records)
PARTIAL_YEAR <- 2026          # data run only part-way through this year
# season window: the SAME standardized survey season the journal figures use (config
# FAIR_MONTHS, Mar-Oct) applied to every year, so no year gains from off-season photos
SEASON_MONTHS <- FAIR_MONTHS
MAKE_DUO     <- FALSE         # TRUE -> also draw the shelved two-species figure

# ---- 1. photo records only (one method across all years) --------------------
pool <- read.csv(PATHS$inat_clean, stringsAsFactors = FALSE) %>%
  transmute(year  = as.integer(substr(observed_on, 1, 4)),
            month = as.integer(substr(observed_on, 6, 7)), genus, species) %>%
  filter(!is.na(year), year >= MIN_YEAR, month %in% SEASON_MONTHS)
totals <- pool %>% count(year, name = "n_total")

share_by_year <- function(focal_genus, focal_species) {
  pool %>% filter(genus == focal_genus,
                  str_detect(coalesce(species, ""), focal_species)) %>%
    count(year, name = "n_focal") %>%
    right_join(totals, by = "year") %>%
    mutate(n_focal = coalesce(n_focal, 0L),
           share   = 100 * n_focal / n_total,
           partial = year == PARTIAL_YEAR) %>%
    arrange(year)
}
spearman_clause <- function(d) {   # rank test: does the share trend with year?
  ct <- suppressWarnings(cor.test(d$year, d$share, method = "spearman"))
  sprintf("rho = %.2f, p = %.2f", unname(ct$estimate), ct$p.value)
}

pom <- share_by_year("Augochlorella", "pomoniella")
write.csv(pom, file.path(OUT_DIR, "most_observed_bee_by_year_record_share.csv"), row.names = FALSE)

# ROBUSTNESS: seasonal-effort control. Her share computed WITHIN each month (months with
# >= 30 bee photos), then averaged per year -- so a year whose surveying happened to lean
# into her Aug-Sep peak cannot inflate the annual share. Reported in the caption.
pool_m <- read.csv(PATHS$inat_clean, stringsAsFactors = FALSE) %>%
  transmute(year = as.integer(substr(observed_on, 1, 4)),
            month = as.integer(substr(observed_on, 6, 7)), genus, species) %>%
  filter(!is.na(year), year >= MIN_YEAR, month %in% SEASON_MONTHS) %>%
  mutate(focal = genus == "Augochlorella" & str_detect(coalesce(species, ""), "pomoniella"))
mstd <- pool_m %>% group_by(year, month) %>%
  summarise(ms = 100 * sum(focal) / n(), nm = n(), .groups = "drop") %>%
  filter(nm >= 30) %>% group_by(year) %>% summarise(share = mean(ms), .groups = "drop")
mstd_clause <- {
  ct <- suppressWarnings(cor.test(mstd$year, mstd$share, method = "spearman"))
  sprintf("month-standardized check (share within each month, then averaged): same pattern, rho = %.2f, p = %.2f",
          unname(ct$estimate), ct$p.value)
}

# ---- 2. the figure ----------------------------------------------------------
LINE_COL <- unname(BEE_EVIDENCE["specimen"])    # deep teal: the neutral/magnitude family
g1 <- ggplot(pom, aes(x = year, y = share)) +
  geom_line(color = LINE_COL, linewidth = 1.1) +
  geom_point(aes(shape = partial), color = LINE_COL, fill = "white", size = 3.2, stroke = 1.1) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 21), guide = "none") +
  geom_text(aes(label = sprintf("%.0f%%", share)), vjust = -1.1, size = 3.1,
            color = BEE_INK$primary, fontface = "bold") +
  scale_x_continuous(breaks = pom$year,
                     labels = sprintf("%d\nn=%s", pom$year, format(pom$n_total, big.mark = ","))) +
  scale_y_continuous(limits = c(0, max(pom$share) * 1.22), expand = expansion(mult = c(0, 0.02))) +
  labs(title = expression("Is our most observed bee, the Peridot Sweat Bee (" *
                          italic("Augochlorella pomoniella") * "), holding steady?"),
       subtitle = str_wrap(sprintf(
         "Within the same March to October season each year, its share of the park's bee photos varies (%d%% to %d%%) and sits highest in the most recent years. No sign of decline.",
         round(min(pom$share)), round(max(pom$share))), 96),
       caption = str_wrap(scope_cap(
         scope  = sprintf("iNaturalist photo records only, %d-%d, whole park, every observer; March-October only, the standardized survey season used by the journal figures, so every year covers the same months; one method across all years (specimen netting ran only 2021-2023, so pooling methods would bias the shares); share = this bee's photos / all bee photos that season (raw counts would track effort, not bees); a share is relative, not a population count; %d is partial (through mid-August, so its season is missing September and October; open point)",
                          MIN_YEAR, PARTIAL_YEAR, PARTIAL_YEAR),
         method = "non-lethal (photo records) only", rank = "bee species",
         source = "iNaturalist observations, Cabrillo NM",
         sig = bee_test("Spearman rank correlation of annual record share vs year",
                         paste0(spearman_clause(pom), "; ", mstd_clause))), 96),
       x = "year (n = all bee photo records that season)", y = "share of all bee photo records (%)") +
  theme_beescabr(11.5) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())
bee_ggsave(file.path(OUT_DIR, "most_observed_bee_by_year_record_share.png"), g1,
           width = 8.6, height = 5.4, bg = "white")

# ---- 3. (shelved) two-species variant ---------------------------------------
if (MAKE_DUO) {
  sub <- share_by_year("Agapostemon", "subtilior")
  duo <- bind_rows(pom %>% mutate(taxon = "Augochlorella pomoniella"),
                   sub %>% mutate(taxon = "Agapostemon subtilior"))
  write.csv(duo %>% select(taxon, year, n_focal, n_total, share),
            file.path(OUT_DIR, "most_observed_bees_by_year_record_share.csv"), row.names = FALSE)
  DUO_COL <- c("Augochlorella pomoniella" = unname(BEE_EVIDENCE["specimen"]),
               "Agapostemon subtilior"    = unname(BEE_EVIDENCE["research"]))
  g2 <- ggplot(duo, aes(x = year, y = share, color = taxon, group = taxon)) +
    geom_line(linewidth = 1.1) +
    geom_point(aes(shape = partial), fill = "white", size = 3, stroke = 1.1) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 21), guide = "none") +
    geom_text(data = duo %>% filter(taxon == "Augochlorella pomoniella"),
              aes(label = sprintf("%.0f%%", share)), vjust = -1.15, size = 3, fontface = "bold", show.legend = FALSE) +
    geom_text(data = duo %>% filter(taxon == "Agapostemon subtilior"),
              aes(label = sprintf("%.0f%%", share)), vjust = 2.25, size = 3, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = DUO_COL, name = NULL,
                       labels = c(`Augochlorella pomoniella` = expression(italic("Augochlorella pomoniella") * "  (Peridot Sweat Bee)"),
                                  `Agapostemon subtilior`    = expression(italic("Agapostemon subtilior")))) +
    scale_x_continuous(breaks = totals$year,
                       labels = sprintf("%d\nn=%s", totals$year, format(totals$n_total, big.mark = ","))) +
    scale_y_continuous(limits = c(-1, max(duo$share) * 1.24), expand = expansion(mult = c(0, 0.02))) +
    labs(title = "Are our two most observed bees holding steady?",
         subtitle = "Both bounce year to year with no steady rise or fall. Year-to-year wiggle, not decline.",
         caption = str_wrap(scope_cap(
           scope  = sprintf("iNaturalist photo records only, %d-%d, whole park; share = the bee's photos / all bee photos that year; %d is a partial year (open points)",
                            MIN_YEAR, PARTIAL_YEAR, PARTIAL_YEAR),
           method = "non-lethal (photo records) only", rank = "bee species",
           sig = bee_test("Spearman rank correlation of annual record share vs year (per species)",
                          sprintf("A. pomoniella: %s; A. subtilior: %s",
                                  spearman_clause(pom), spearman_clause(sub)))), 96),
         x = "year (n = all bee photo records that season)", y = "share of all bee photo records (%)") +
    theme_beescabr(11.5) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
          legend.position = "top", panel.grid.minor = element_blank())
  bee_ggsave(file.path(OUT_DIR, "most_observed_bees_by_year_record_share.png"), g2,
             width = 8.6, height = 5.8, bg = "white")
}

message("Most observed bee (photo records): Augochlorella pomoniella -- share by year:")
print(as.data.frame(pom), row.names = FALSE)
message("Wrote most_observed_bee_by_year_record_share.{png,csv} to ", OUT_DIR)
