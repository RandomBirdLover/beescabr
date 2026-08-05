# =============================================================
# analysis/coverage_cabr_county_map.R
# beescabr -- LOCATOR MAP: tiny Cabrillo National Monument on the San Diego County
# map, marked with the biodiversity share it carries. The visual point: CABR is a
# speck of the county by area yet holds a large share of its native-bee diversity.
#
# Boundaries: NPS official CABR boundary + San Diego County boundary (same CRS).
# Area % is measured straight from the polygons; species/genus shares are counted
# from the CABR official checklist vs the Holway county checklist.
#
# Run from the repo root:  Rscript scripts/analysis/coverage_cabr_county_map.R
# Depends on: sf, ggplot2, dplyr, stringr (+ config.R).
# =============================================================

for (pkg in c("sf", "ggplot2", "cowplot")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(sf); library(dplyr); library(stringr); library(ggplot2) })
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_SEQ")) source("scripts/analysis/theme_beescabr.R")
OUT_DIR <- "data/analysis/coverage/footprint"; dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

BND_COUNTY <- "data/spatial/boundaries/san_diego_county/sd_county_boundary.shp"
BND_CABR   <- "data/spatial/boundaries/cabr/nps_official/cabr_boundary_nps_official.shp"
BND_PL     <- "data/spatial/boundaries/point_loma/point_loma_boundary.shp"
CHECKLIST_CABR   <- "data/checklists/cabr/cabr_official_native_bee_checklist.csv"
CHECKLIST_HOLWAY <- "data/checklists/sd_county/sd_holway_native_bee_checklist.csv"
SPECIES_RANKS <- c("species", "subspecies")

# ---- shares from the checklists ---------------------------------------------
binoms <- function(chk) { d <- chk[tolower(str_squish(chk$taxon_rank)) %in% SPECIES_RANKS, , drop = FALSE]
  b <- tolower(sub("^(\\S+\\s+\\S+).*$", "\\1", str_squish(d$scientific_name))); unique(b[grepl("\\s", b)]) }
genera_of <- function(chk) { g <- tolower(str_squish(chk$genus)); unique(g[nzchar(g)]) }
cabr_ck <- read.csv(CHECKLIST_CABR, stringsAsFactors = FALSE, check.names = FALSE)
hol_ck  <- read.csv(CHECKLIST_HOLWAY, stringsAsFactors = FALSE, check.names = FALSE)
n_cabr_sp <- length(binoms(cabr_ck)); n_hol_sp <- length(binoms(hol_ck))
n_cabr_gn <- length(genera_of(cabr_ck)); n_hol_gn <- length(genera_of(hol_ck))
sp_pct  <- 100 * n_cabr_sp / n_hol_sp
gen_pct <- 100 * n_cabr_gn / n_hol_gn

# ---- boundaries + measured areas --------------------------------------------
county <- st_read(BND_COUNTY, quiet = TRUE)
cabr   <- st_read(BND_CABR,   quiet = TRUE) |> st_transform(st_crs(county))
pl     <- st_read(BND_PL,     quiet = TRUE) |> st_transform(st_crs(county))
cabr_acres <- as.numeric(sum(st_area(cabr))) / 4046.856
county_sqmi <- as.numeric(sum(st_area(county))) / 2589988.11
area_pct <- 100 * as.numeric(sum(st_area(cabr))) / as.numeric(sum(st_area(county)))
overrep  <- sp_pct / area_pct
cabr_pt  <- st_centroid(st_union(cabr))
cc <- as.numeric(st_coordinates(cabr_pt))
bb <- st_bbox(county)

# ---- callout text -----------------------------------------------------------
call_txt <- sprintf("Cabrillo National Monument\n%.0f acres  =  %.3f%% of the county's land\n\n%.0f%%  of the county's native bee SPECIES\n%.0f%%  of its bee GENERA",
                    cabr_acres, area_pct, sp_pct, gen_pct)
# anchor the callout in open space NE of CABR (inland), leader curves down to the dot
ax <- cc[1] + (bb["xmax"] - bb["xmin"]) * 0.34
ay <- cc[2] + (bb["ymax"] - bb["ymin"]) * 0.42

# ---- map ---------------------------------------------------------------------
g <- ggplot() +
  geom_sf(data = county, fill = "#ECEAE4", color = "#AEAAA0", linewidth = 0.35) +
  geom_sf(data = cabr_pt, color = unname(BEE_SEQ[5]), fill = unname(BEE_SEQ[5]),
          shape = 21, size = 5.5, stroke = 0) +
  geom_sf(data = cabr_pt, color = "white", shape = 21, size = 5.5, stroke = 1.1, fill = NA) +
  annotate("curve", x = ax, y = ay, xend = cc[1] + 2500, yend = cc[2] + 1500,
           curvature = 0.28, linewidth = 0.5, color = BEE_INK$secondary,
           arrow = arrow(length = unit(0.14, "cm"), type = "closed")) +
  annotate("label", x = ax, y = ay, label = call_txt, hjust = 0, vjust = 0.5,
           size = 3.7, lineheight = 1.05, color = BEE_INK$primary, fill = "white") +
  annotate("text", x = cc[1] - 1500, y = cc[2] - 3500, label = "Point Loma", hjust = 1,
           size = 3, fontface = "italic", color = BEE_INK$muted) +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"] + (bb["xmax"] - bb["xmin"]) * 0.30),
           ylim = c(bb["ymin"], bb["ymax"]), expand = TRUE) +
  labs(title = sprintf("Cabrillo Holds %.0f%% of San Diego County's Native Bee Species", sp_pct),
       caption = str_wrap(sprintf("Cabrillo National Monument (%.0f acres) shown on the San Diego County boundary. It holds %.0f%% of the county's native bee species (%d of %d) and %.0f%% of its genera (%d of %d) on ~%.3f%% of its land -- roughly %sx its share by area. CABR official checklist vs Holway SD County checklist (v3).",
                    cabr_acres, sp_pct, n_cabr_sp, n_hol_sp, gen_pct, n_cabr_gn, n_hol_gn, area_pct, format(round(overrep, -2), big.mark = ",")), 78)) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5, colour = BEE_INK$primary, margin = margin(b = 6)),
        plot.caption = element_text(colour = BEE_INK$secondary, size = 9, hjust = 0, margin = margin(t = 8)),
        plot.caption.position = "plot", plot.title.position = "plot",
        plot.margin = margin(12, 12, 10, 12))
# ---- inset: CABR's real footprint on the Point Loma peninsula ---------------
plbb <- st_bbox(pl)
g_inset <- ggplot() +
  geom_sf(data = pl,   fill = "#F1EFEA", color = "#A7A399", linewidth = 0.3) +
  geom_sf(data = cabr, fill = unname(BEE_SEQ[5]), color = unname(BEE_SEQ[5]), linewidth = 0.15) +
  coord_sf(xlim = c(plbb["xmin"], plbb["xmax"]), ylim = c(plbb["ymin"], plbb["ymax"]), expand = FALSE) +
  labs(title = "CABR on Point Loma") +
  theme_void(base_size = 9) +
  theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5, colour = BEE_INK$secondary,
                                  margin = margin(t = 3, b = 2)),
        plot.background = element_rect(fill = "white", colour = "#CBC7BE", linewidth = 0.5),
        plot.margin = margin(2, 5, 5, 5))
if (requireNamespace("cowplot", quietly = TRUE)) {
  final <- cowplot::ggdraw(g) + cowplot::draw_plot(g_inset, x = 0.71, y = 0.15, width = 0.26, height = 0.32)
} else {
  message("  (cowplot not installed -- saving the county map without the Point Loma inset)"); final <- g
}
ggsave(file.path(OUT_DIR, "cabr_county_map.png"), final, width = 9, height = 8.2, dpi = 200, bg = "white")
message(sprintf("Wrote cabr_county_map.png | CABR %.0f ac = %.3f%% of county (%.0f sq mi); %.0f%% species, %.0f%% genera; ~%.0fx",
                cabr_acres, area_pct, county_sqmi, sp_pct, gen_pct, overrep))
