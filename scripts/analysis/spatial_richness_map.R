# =============================================================
# Whole-park spatial richness map  (Q8 -- companion to diversity_indices.R)
# beescabr / Cabrillo National Monument (CABR) native bees
#
# WHAT THIS IS:
#   A coverage/richness surface over the park. Every georeferenced bee record
#   (lethal specimens + non-lethal iNaturalist) is snapped to a square grid, and
#   each cell is summarised for:
#       * sampling effort   (number of records)
#       * genus richness    (distinct bee genera)
#       * species richness  (distinct bee species)
#       * rarefied richness (species richness rarefied to a common count, so the
#                            surface is not just a map of where people looked)
#
# SCOPE (deliberately different from the rest of diversity_indices.R):
#   ALL RECORDS, not survey-only -- a coverage map answers "where in the park
#   have bees been found", a whole-park question. BUT the fine lat/long grid is
#   built from iNaturalist ONLY: specimen coordinates are transect centroids
#   (~980 specimens on ~18 points), so gridding them would invent false hotspots.
#   iNat has real GPS, so it drives the grid; specimens are summarised BY TRANSECT
#   (their reliable spatial unit) in a companion table + bar chart. Every figure
#   carries a red caption stating its scope.
#
#   Raw richness per cell rises with how much a cell was sampled -- so the effort
#   map is rendered alongside, and a rarefied-richness map (effort-controlled) is
#   produced for cells with enough records to rarefy fairly.
#
# Run from the repo root:  Rscript scripts/analysis/spatial_richness_map.R
# Depends on: sf, dplyr, stringr, vegan, ggplot2 (+ config.R).
# =============================================================

for (pkg in c("sf", "ggplot2", "vegan")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(sf); library(ggplot2); library(vegan)
})

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_SEQ")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_DIR       <- "data/analysis/richness/diversity"
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
CRS_LATLON    <- 4326
CRS_UTM       <- 32611          # UTM zone 11N -- CABR / San Diego, metres
CELL_M        <- 75             # grid cell size (metres); accuracy median ~4 m
MAX_ACCURACY  <- 250            # drop iNat points looser than this (metres); NA kept
RAREFY_N      <- 20             # rarefy cells with >= this many records to this count
TRANSECTS     <- c("BST", "UPMON", "TP", "OT")   # for the per-transect richness summary
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"
scope_cap <- function(scope, method, rank) sprintf("Scope: %s  |  Method: %s  |  Rank: %s",
                                                   scope, method, rank)

# ---- 1. ALL bee records with coordinates + taxonomy keys ---------------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

prep <- function(df, method) {
  acc <- if ("positional_accuracy" %in% names(df)) suppressWarnings(as.numeric(df$positional_accuracy)) else NA_real_
  data.frame(
    method    = method,
    latitude  = suppressWarnings(as.numeric(df$latitude)),
    longitude = suppressWarnings(as.numeric(df$longitude)),
    accuracy  = acc,
    taxon_rank = df$taxon_rank,
    genus     = df$genus,
    species   = df$species,
    stringsAsFactors = FALSE) %>%
    mutate(
      species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                             !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
      genus_key   = ifelse(taxon_rank %in% GENUS_RANKS & !is.na(genus) & genus != "", genus, NA))
}
# iNaturalist only drives the fine grid -- it has real GPS. Specimens are
# transect centroids (see the per-transect summary below), so gridding them
# would fabricate hotspots.
rec <- prep(inat, "nonlethal") %>%
  filter(!is.na(latitude), !is.na(longitude))
n_all <- nrow(rec)
rec <- rec %>% filter(is.na(accuracy) | accuracy <= MAX_ACCURACY)
message(sprintf("Georeferenced iNaturalist records: %d kept of %d (dropped %d looser than %dm)",
                nrow(rec), n_all, n_all - nrow(rec), MAX_ACCURACY))

# ---- 2. project + build the grid --------------------------------------------
pts <- st_as_sf(rec, coords = c("longitude", "latitude"), crs = CRS_LATLON) %>%
  st_transform(CRS_UTM)
grid_geom <- st_make_grid(pts, cellsize = CELL_M, square = TRUE)
grid <- st_sf(cell_id = seq_along(grid_geom), geometry = grid_geom)

joined <- st_join(pts, grid, join = st_within)
cell_tbl <- st_drop_geometry(joined) %>%
  group_by(cell_id) %>%
  summarise(n_records       = n(),
            genus_richness   = n_distinct(genus_key[!is.na(genus_key)]),
            species_richness = n_distinct(species_key[!is.na(species_key)]),
            .groups = "drop")

# ---- 2b. rarefied species richness per cell (effort-controlled) --------------
# build cell x species count matrix, rarefy every cell with >= RAREFY_N records
sp_long <- st_drop_geometry(joined) %>% filter(!is.na(species_key))
rare_tbl <- data.frame(cell_id = integer(0), rarefied_richness = numeric(0))
if (nrow(sp_long) > 0) {
  M <- as.matrix(table(sp_long$cell_id, sp_long$species_key))
  keep <- rowSums(M) >= RAREFY_N
  if (any(keep)) {
    rr <- vegan::rarefy(M[keep, , drop = FALSE], sample = RAREFY_N)
    rare_tbl <- data.frame(cell_id = as.integer(rownames(M)[keep]),
                           rarefied_richness = round(as.numeric(rr), 2))
  }
}

# ---- 3. assemble cell polygons that actually contain records -----------------
cells <- grid %>%
  inner_join(cell_tbl, by = "cell_id") %>%
  left_join(rare_tbl, by = "cell_id")
cent <- st_transform(st_centroid(st_geometry(cells)), CRS_LATLON) %>% st_coordinates()
cells$centroid_lon <- round(cent[, 1], 6)
cells$centroid_lat <- round(cent[, 2], 6)

out_csv <- st_drop_geometry(cells) %>%
  select(cell_id, centroid_lon, centroid_lat, n_records,
         genus_richness, species_richness, rarefied_richness) %>%
  arrange(desc(species_richness), desc(n_records))
write.csv(out_csv, file.path(OUT_DIR, "spatial_richness_grid.csv"), row.names = FALSE)
message(sprintf("Occupied cells: %d (cell size %dm). Rarefied to %d: %d cells.",
                nrow(cells), CELL_M, RAREFY_N, sum(!is.na(cells$rarefied_richness))))

# ---- 4. maps -----------------------------------------------------------------
base_theme <- theme_beescabr(11) +
  theme(axis.text = element_text(size = 7, colour = BEE_INK$muted))

draw_map <- function(fill_col, title, legend_lab, file, palette = "viridis", trans = "identity") {
  dat <- cells[!is.na(cells[[fill_col]]), ]
  rank_lab <- if (grepl("genus", fill_col)) "genus" else
              if (fill_col == "n_records") "n/a (record count)" else "species"
  cap <- str_wrap(scope_cap(sprintf("iNaturalist only (real GPS), %dm grid", CELL_M),
                            "non-lethal; specimens summarised by transect", rank_lab), width = 62)
  g <- ggplot(dat) +
    geom_sf(aes(fill = .data[[fill_col]]), color = "white", linewidth = 0.15) +
    scale_fill_gradientn(colours = BEE_SEQ, name = legend_lab, trans = trans) +   # magnitude = house blue ramp (palette arg now unused)
    labs(title = title, subtitle = cap, x = NULL, y = NULL) +
    coord_sf(datum = CRS_UTM) +
    base_theme
  ggsave(file, g, width = 7.4, height = 8.2, dpi = 200, bg = "white")
}

# Grid-cell maps intentionally NOT drawn -- they were hard to read (no basemap, UTM axes) and are
# superseded by the per-transect richness/effort bar charts below. The per-cell values still live in
# spatial_richness_grid.csv (written above). To bring a map back, call draw_map(...) (kept above), e.g.
#   draw_map("rarefied_richness", "CABR bee richness, rarefied", "species/cell",
#            file.path(OUT_DIR, "map_rarefied_richness.png"))

# ---- 5. per-transect richness (BOTH methods) -- specimens' reliable unit ------
# Specimen coordinates are transect centroids, so specimens are summarised BY
# TRANSECT here rather than gridded above. Both methods pooled, all-records; the
# four transects only (off-transect records carry no transect and are excluded).
tr_key <- function(df, method) df %>% transmute(
  method = method, transect = toupper(str_squish(transect)),
  species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                         !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
  genus_key   = ifelse(taxon_rank %in% GENUS_RANKS & !is.na(genus) & genus != "", genus, NA))
tr_tbl <- bind_rows(tr_key(spec, "lethal"), tr_key(inat, "nonlethal")) %>%
  filter(transect %in% TRANSECTS) %>%
  group_by(transect) %>%
  summarise(n_records         = n(),
            genus_richness    = n_distinct(genus_key[!is.na(genus_key)]),
            species_richness  = n_distinct(species_key[!is.na(species_key)]),
            records_lethal    = sum(method == "lethal"),
            records_nonlethal = sum(method == "nonlethal"),
            .groups = "drop") %>%
  arrange(desc(species_richness))
write.csv(tr_tbl, file.path(OUT_DIR, "transect_richness.csv"), row.names = FALSE)
message("Per-transect richness (both methods): ",
        paste(sprintf("%s=%dsp", tr_tbl$transect, tr_tbl$species_richness), collapse = "  "))
# TWO separate, legible per-transect bar charts (a clearer replacement for the raw grid maps):
#   transect_richness.png -- distinct species + genera per transect
#   transect_effort.png   -- record count per transect, split lethal (net) vs non-lethal (photo)
# Both share the same transect order (richest first).
tr_lvl <- as.character(tr_tbl$transect[order(-tr_tbl$species_richness)])   # shared x order (richest first)
tt <- tr_tbl; tt$transect <- factor(tt$transect, levels = tr_lvl)
lab_col <- BEE_INK$secondary

rich_long <- bind_rows(
  data.frame(transect = tt$transect, rank = "species", value = tt$species_richness),
  data.frame(transect = tt$transect, rank = "genus",   value = tt$genus_richness))
rich_long$rank <- factor(rich_long$rank, levels = c("species", "genus"))
gA <- ggplot(rich_long, aes(transect, value, fill = rank)) +
  geom_col(position = position_dodge(0.72), width = 0.66) +
  geom_text(aes(label = value), position = position_dodge(0.72), vjust = -0.35, size = 3, colour = lab_col) +
  scale_fill_manual(values = c(species = "#3C3B36", genus = "#C0BBB0"), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.13))) +
  labs(title = "CABR bee richness by transect",
       subtitle = str_wrap(scope_cap("all records, per transect (off-transect excluded)", "lethal + non-lethal", "genus + species"), 72),
       x = NULL, y = "distinct taxa") +
  base_theme + theme(legend.position = "top", plot.subtitle = element_text(size = 8.5))

eff_long <- bind_rows(
  data.frame(transect = tt$transect, method = "non-lethal (photo)", value = tt$records_nonlethal),
  data.frame(transect = tt$transect, method = "lethal (net)",       value = tt$records_lethal))
eff_long$method <- factor(eff_long$method, levels = c("non-lethal (photo)", "lethal (net)"))
gB <- ggplot(eff_long, aes(transect, value, fill = method)) +
  geom_col(width = 0.66) +
  geom_text(data = tt, aes(transect, n_records, label = n_records), vjust = -0.35, size = 3, colour = lab_col, inherit.aes = FALSE) +
  scale_fill_manual(values = setNames(unname(BEE_METHOD_COL[c("nonlethal", "lethal")]),
                                      c("non-lethal (photo)", "lethal (net)")), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.13))) +
  labs(title = "CABR bee sampling effort by transect",
       subtitle = str_wrap(scope_cap("all records, per transect (off-transect excluded)", "net vs photo", "records"), 72),
       x = NULL, y = "records") +
  base_theme + theme(legend.position = "top", plot.subtitle = element_text(size = 8.5))

ggsave(file.path(OUT_DIR, "transect_richness.png"), gA, width = 6.4, height = 5, dpi = 200, bg = "white")
ggsave(file.path(OUT_DIR, "transect_effort.png"),   gB, width = 6.4, height = 5, dpi = 200, bg = "white")

message("Per-transect richness + effort charts (+ spatial_richness_grid.csv) written to ", OUT_DIR)
