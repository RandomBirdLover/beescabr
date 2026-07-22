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
#   This map uses ALL RECORDS, not survey-only. A coverage map answers "where in
#   the park have bees been found / looked for", which is a whole-park question,
#   so restricting to standardized survey effort would throw away exactly the
#   off-transect coverage the map is meant to show. Both methods are pooled.
#   Every figure carries a red caption stating this.
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
OUT_DIR       <- "data/analysis/diversity"
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
CRS_LATLON    <- 4326
CRS_UTM       <- 32611          # UTM zone 11N -- CABR / San Diego, metres
CELL_M        <- 75             # grid cell size (metres); accuracy median ~4 m
MAX_ACCURACY  <- 250            # drop iNat points looser than this (metres); NA kept
RAREFY_N      <- 20             # rarefy cells with >= this many records to this count
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
rec <- bind_rows(prep(spec, "lethal"), prep(inat, "nonlethal")) %>%
  filter(!is.na(latitude), !is.na(longitude))
n_all <- nrow(rec)
rec <- rec %>% filter(is.na(accuracy) | accuracy <= MAX_ACCURACY)
message(sprintf("Georeferenced bee records: %d kept of %d (dropped %d looser than %dm)",
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
cent <- st_transform(st_centroid(cells), CRS_LATLON) %>% st_coordinates()
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
base_theme <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "#b2182b", size = 9),
        axis.text = element_text(size = 7),
        panel.grid = element_line(color = "grey92"))

draw_map <- function(fill_col, title, legend_lab, file, palette = "viridis", trans = "identity") {
  dat <- cells[!is.na(cells[[fill_col]]), ]
  rank_lab <- if (grepl("genus", fill_col)) "genus" else
              if (fill_col == "n_records") "n/a (record count)" else "species"
  cap <- str_wrap(scope_cap(sprintf("ALL records (not survey-only), %dm grid", CELL_M),
                            "lethal + non-lethal pooled", rank_lab), width = 62)
  g <- ggplot(dat) +
    geom_sf(aes(fill = .data[[fill_col]]), color = "white", linewidth = 0.15) +
    scale_fill_viridis_c(option = palette, name = legend_lab, trans = trans) +
    labs(title = title, subtitle = cap, x = NULL, y = NULL) +
    coord_sf(datum = CRS_UTM) +
    base_theme
  ggsave(file, g, width = 7.4, height = 8.2, dpi = 200, bg = "white")
}

draw_map("species_richness", "CABR bee species richness by grid cell",
         "species", file.path(OUT_DIR, "map_species_richness.png"))
draw_map("genus_richness", "CABR bee genus richness by grid cell",
         "genera", file.path(OUT_DIR, "map_genus_richness.png"), palette = "mako")
draw_map("n_records", "CABR bee sampling effort by grid cell",
         "records", file.path(OUT_DIR, "map_sampling_effort.png"),
         palette = "inferno", trans = "log10")
if (any(!is.na(cells$rarefied_richness)))
  draw_map("rarefied_richness",
           sprintf("CABR bee richness, rarefied to %d records/cell", RAREFY_N),
           sprintf("species / %d recs", RAREFY_N),
           file.path(OUT_DIR, "map_rarefied_richness.png"), palette = "viridis")

message("Spatial richness maps written to ", OUT_DIR)
