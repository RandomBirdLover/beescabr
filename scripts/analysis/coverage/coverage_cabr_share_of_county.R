# =============================================================
# analysis/coverage/coverage_cabr_share_of_county.R
# beescabr -- how much of San Diego County's native-bee diversity does tiny
# Cabrillo National Monument carry?  (a "punches above its weight" stat.)
#
# THE POINT (for management): CABR is a sliver of the county by area, yet holds a
# large share of its bee diversity. We compare the CABR OFFICIAL checklist to the
# Holway San Diego County checklist (the county reference), for both SPECIES and
# GENERA, and set that against CABR's share of the county's land area.
#
# NOTE ON SCOPE: we deliberately do NOT compare to a Point Loma checklist -- the
# only Point Loma list is iNaturalist-only (no comprehensive specimen survey of the
# peninsula), so it is not a fair denominator. The county (Holway) is the yardstick.
#
# Areas are fixed facts: CABR = 143.9 acres (NPS / Wikipedia); San Diego County =
# ~4,526 sq mi. Everything else is counted from the checklists. Descriptive; no test.
#
# Run from the repo root:  source("scripts/analysis/coverage/coverage_cabr_share_of_county.R")
# Depends on: dplyr, stringr, ggplot2 (+ config.R).
# =============================================================

# Dependencies are CHECKED here, not installed: see beescabr_require() in config.R.
if (!exists("beescabr_require")) source("scripts/config.R")
beescabr_require()
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_SEQ")) source("scripts/analysis/shared/theme_beescabr.R")   # shared house style
OUT_DIR <- file.path(DIR_REPORT, "coverage/footprint")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
# scope_cap(): use the SHARED helper from theme_beescabr.R -- adds Source + data-as-of, one canonical order (no local override).

CHECKLIST_CABR   <- PATHS$checklist_cabr_official
CHECKLIST_HOLWAY <- PATHS$checklist_sd_holway
CABR_ACRES       <- 160.4      # measured from the NPS official CABR boundary shapefile
SD_COUNTY_SQMI   <- 4261       # measured from the San Diego County boundary shapefile
SPECIES_RANKS    <- c("species", "subspecies")

# ---- 1. count distinct species (binomials) + genera on each checklist --------
binoms <- function(chk) {
  d  <- chk[tolower(str_squish(chk$taxon_rank)) %in% SPECIES_RANKS, , drop = FALSE]
  sn <- str_squish(d$scientific_name)
  b  <- tolower(sub("^(\\S+\\s+\\S+).*$", "\\1", sn))     # first two words = binomial
  unique(b[grepl("\\s", b)])
}
genera_of <- function(chk) {                # any bee genus recorded on the checklist (all ranks)
  g <- tolower(str_squish(chk$genus)); unique(g[nzchar(g)])
}
cabr <- read.csv(CHECKLIST_CABR,   stringsAsFactors = FALSE, check.names = FALSE)
hol  <- read.csv(CHECKLIST_HOLWAY, stringsAsFactors = FALSE, check.names = FALSE)
cabr_sp <- binoms(cabr); hol_sp <- binoms(hol)
cabr_gn <- genera_of(cabr); hol_gn <- genera_of(hol)

n_cabr_sp <- length(cabr_sp); n_hol_sp <- length(hol_sp)
n_cabr_gn <- length(cabr_gn); n_hol_gn <- length(hol_gn)
n_shared  <- length(intersect(cabr_sp, hol_sp))          # CABR species also on Holway

# ---- 2. shares (CABR as % of the county) ------------------------------------
cabr_sqmi <- CABR_ACRES / 640
area_pct  <- 100 * cabr_sqmi / SD_COUNTY_SQMI
sp_pct    <- 100 * n_cabr_sp / n_hol_sp
gen_pct   <- 100 * n_cabr_gn / n_hol_gn
overrep   <- sp_pct / area_pct

stat_tbl <- data.frame(
  measure       = c("Land area", "Bee species", "Bee genera"),
  cabr          = c(round(cabr_sqmi, 3), n_cabr_sp, n_cabr_gn),
  county        = c(SD_COUNTY_SQMI, n_hol_sp, n_hol_gn),
  cabr_pct      = round(c(area_pct, sp_pct, gen_pct), c(3, 1, 1)),
  stringsAsFactors = FALSE)
write.csv(stat_tbl, file.path(OUT_DIR, "cabr_share_of_county.csv"), row.names = FALSE)
message(sprintf("CABR share of San Diego County: species %.1f%% (%d/%d), genera %.1f%% (%d/%d), land %.4f%%; ~%.0fx overrepresented",
                sp_pct, n_cabr_sp, n_hol_sp, gen_pct, n_cabr_gn, n_hol_gn, area_pct, overrep))
message(sprintf("  %d of %d CABR species (%.0f%%) are on the Holway county list; %d are not (new-to-county)",
                n_shared, n_cabr_sp, 100 * n_shared / n_cabr_sp, n_cabr_sp - n_shared))

# ---- 3. figure: CABR's share of the county, area vs diversity -----------------
plot_df <- data.frame(
  measure = factor(c("Land area", "Bee species", "Bee genera"),
                   levels = c("Land area", "Bee species", "Bee genera")),   # area bottom -> genera top
  pct = c(area_pct, sp_pct, gen_pct),
  lab = c(sprintf("%.3f%%   (%.0f acres of ~%s sq mi)", area_pct, CABR_ACRES, format(SD_COUNTY_SQMI, big.mark = ",", trim = TRUE)),
          sprintf("%.0f%%   (%d of %d species)", sp_pct, n_cabr_sp, n_hol_sp),
          sprintf("%.0f%%   (%d of %d genera)", gen_pct, n_cabr_gn, n_hol_gn)))
# lollipop (line + dot), NOT bars: land area is ~0.006%, so a bar is invisible and reads as
# "missing". A dot anchors every measure -- the land-area dot sits right at zero (the point of
# the figure), while the species/genus dots still show magnitude by position.
g <- ggplot(plot_df, aes(x = pct, y = measure, colour = pct)) +
  geom_segment(aes(x = 0, xend = pct, yend = measure), linewidth = 1.8) +
  geom_point(size = 5.5) +
  geom_text(aes(label = lab), hjust = 0, nudge_x = 1.1, size = 3.5, colour = BEE_INK$secondary) +
  scale_colour_gradientn(colors = NPS_SEQ, guide = "none") +   # keep the forest-green magnitude ramp (off the crimson family); rest of the styling is house default
  scale_x_continuous(limits = c(0, max(gen_pct) * 1.7), expand = expansion(mult = c(0.02, 0))) +
  labs(title = "Does Cabrillo punch above its weight for bees?",
       subtitle = sprintf("Roughly %sx the native-bee diversity you'd expect from its area -- and %.0f%% of the county's bee genera.",
                          format(round(overrep, -2), big.mark = ","), gen_pct),
       caption = scope_cap(scope = "CABR's share of San Diego County; area vs native-bee diversity",
                           method = "lethal + non-lethal pooled",
                           rank = "species + genus",
                           source = "official CABR checklist vs Holway SD County checklist (v3)"),
       x = "Cabrillo's share of San Diego County (%)", y = NULL) +
  theme_beescabr(12) +
  theme(legend.position = "none", panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
# RETIRED: every number here is on cabr_county_map.png, which also shows WHERE
# Cabrillo is. The over-representation figure moved into that map's subtitle.
# bee_ggsave(file.path(OUT_DIR, "cabr_share_of_county.png"), g, width = 9.5, height = 4.8, bg = "white")
message("Wrote cabr_share_of_county.{png,csv} to ", OUT_DIR)
