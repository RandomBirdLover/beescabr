# =============================================================
# analysis/interactions_network.R
# beescabr pipeline -- plant-bee visitation networks (Q4 / Q3 support)
# Created: 2026-07-21
#
# Q4: "Which flowers do which bees visit -- and which plants do the rarer bees
# depend on?" Builds bipartite plant-bee networks from the cleaned records'
# resolved plant columns (`plant_genus`), pooling BOTH methods (specimen + iNat).
#
# TWO NETWORKS (like the accumulation run, since not every bee is ID'd to
# species):
#   * GENUS network  -- plant genus x BEE GENUS. Uses every genus-resolved
#     record; the complete/robust view of who-visits-what.
#   * SPECIES network -- plant genus x BEE SPECIES (species-level bees only).
#     Sparser, but the only rank where "specialist / rare-bee" reads are valid.
#
# Node convention (per the Shizuka ecological-networks tutorial):
#   plant genus = rows, bee = columns; a cell counts co-occurrence records.
#
# OUTPUTS
#   * interaction matrices (plant genus x bee) for both ranks -- CSV
#   * heatmaps of both matrices (dependency-free; the always-works view)
#   * bee-genus "shared-forage" network -- igraph one-mode projection: two bee
#     genera linked if they visit the same plant genera (surfaces guild structure)
#   * per-bee-species specialization table: how many plant genera each visits,
#     its main forage, and rare/specialist flags (rare = low relative frequency)
#   * (optional) bipartite::plotweb figures IF the `bipartite` package is present
#
# Run from the repo root:  Rscript scripts/analysis/interactions_network.R
# Depends on: dplyr, stringr, igraph  (+ optional bipartite). config.R for paths.
# =============================================================

# ---- dependency guard (install-guarded HERE, not in utils.R -- the pipeline
#      never needs network packages) -----------------------------------------
for (pkg in c("igraph", "bipartite")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(igraph)
})
HAVE_BIPARTITE <- requireNamespace("bipartite", quietly = TRUE)

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
OUT_DIR       <- "data/analysis"
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
SPECIALIST_MAX_PLANTS <- 2      # visits <= this many plant genera -> "specialist"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 1. interaction records (both methods pooled) ---------------------------
cols <- c("genus", "species", "taxon_rank", "plant_genus")
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

inter <- bind_rows(spec[cols], inat[cols]) %>%
  filter(!is.na(plant_genus), plant_genus != "", !is.na(genus), genus != "")
message(sprintf("Interaction records with a plant genus + bee genus: %d", nrow(inter)))

# plant-genus x bee weighted incidence matrix (cells = # co-occurrence records)
to_matrix <- function(df, bee_col) {
  t <- xtabs(stats::reformulate(c("plant_genus", bee_col)), data = df)
  matrix(as.integer(t), nrow = nrow(t), dimnames = dimnames(t))
}

genus_df <- inter %>% filter(taxon_rank %in% GENUS_RANKS) %>%
  transmute(plant_genus, bee = genus)
species_df <- inter %>% filter(taxon_rank %in% SPECIES_RANKS, species != "") %>%
  transmute(plant_genus, bee = paste(genus, word(species, -1)))

Mg <- to_matrix(genus_df,   "bee")   # plant genus x bee genus
Ms <- to_matrix(species_df, "bee")   # plant genus x bee species
message(sprintf("  genus network:   %d plant genera x %d bee genera",  nrow(Mg), ncol(Mg)))
message(sprintf("  species network: %d plant genera x %d bee species", nrow(Ms), ncol(Ms)))

write.csv(data.frame(plant_genus = rownames(Mg), Mg, check.names = FALSE),
          file.path(OUT_DIR, "interactions_genus_matrix.csv"), row.names = FALSE)
write.csv(data.frame(plant_genus = rownames(Ms), Ms, check.names = FALSE),
          file.path(OUT_DIR, "interactions_species_matrix.csv"), row.names = FALSE)

# ---- 2. heatmaps (dependency-free; top plant genera for legibility) ----------
heatmap_png <- function(M, file, rank_label, top_plants = 30) {
  ord_p <- order(rowSums(M), decreasing = TRUE)
  ord_b <- order(colSums(M), decreasing = TRUE)
  M2 <- M[head(ord_p, top_plants), ord_b, drop = FALSE]
  M2 <- M2[nrow(M2):1, , drop = FALSE]                     # top plant at top of image
  png(file, width = max(1400, 60 * ncol(M2) + 500),
      height = max(1100, 34 * nrow(M2) + 350), res = 200)
  op <- par(mar = c(10, 9, 3, 1))
  image(x = seq_len(ncol(M2)), y = seq_len(nrow(M2)), z = t(log1p(M2)),
        col = hcl.colors(24, "YlGnBu", rev = TRUE), axes = FALSE,
        xlab = "", ylab = "",
        main = sprintf("Plant genus x bee %s -- visitation intensity (log records)", rank_label))
  axis(1, seq_len(ncol(M2)), colnames(M2), las = 2, cex.axis = 0.7)
  axis(2, seq_len(nrow(M2)), rownames(M2), las = 1, cex.axis = 0.7)
  mtext(sprintf("top %d plant genera by total visits", nrow(M2)), side = 3, cex = 0.8)
  par(op); dev.off()
}
heatmap_png(Mg, file.path(OUT_DIR, "interactions_heatmap_genus.png"),   "genus")
heatmap_png(Ms, file.path(OUT_DIR, "interactions_heatmap_species.png"), "species")

# ---- 3. bee-genus shared-forage network (igraph one-mode projection) ---------
# two bee genera are linked if they visit the same plant genera; edge weight =
# number of shared plant genera. Node size = plant-genera breadth (big central
# hub = generalists; small rim nodes = narrow-forage specialists). Only edges
# sharing >= MIN_SHARED plant genera are drawn, to cut the generalist blur.
MIN_SHARED <- 3
incidence_graph <- function(m) {                           # igraph-version-safe
  if (exists("graph_from_biadjacency_matrix"))
    graph_from_biadjacency_matrix(m) else graph_from_incidence_matrix(m)
}
bg     <- incidence_graph(Mg > 0)                          # plants=FALSE, bees=TRUE
proj   <- bipartite_projection(bg, multiplicity = TRUE)
beebee <- proj$proj2                                       # the bee-bee graph
breadth <- colSums(Mg > 0)[V(beebee)$name]
V(beebee)$size  <- 3 + 13 * (breadth - min(breadth)) / (max(breadth) - min(breadth))
beebee_plot <- delete_edges(beebee, E(beebee)[E(beebee)$weight < MIN_SHARED])
png(file.path(OUT_DIR, "interactions_bee_genus_network.png"),
    width = 1500, height = 1450, res = 200)
set.seed(1)
plot(beebee_plot,
     vertex.color = "#2166ac", vertex.frame.color = "white",
     vertex.label.color = "black", vertex.label.cex = 0.6, vertex.label.dist = 0.5,
     edge.width = pmin(0.25 * E(beebee_plot)$weight, 3),
     edge.color = adjustcolor("grey60", 0.5), layout = layout_with_fr,
     main = sprintf("Bee genera linked by shared plant genera (>= %d shared)\nnode size = plant-genera breadth; rim = specialists", MIN_SHARED))
dev.off()

# ---- 4. per-bee-species specialization / rarity table -----------------------
deg  <- colSums(Ms > 0)                    # distinct plant genera visited
recs <- colSums(Ms)                        # interaction records
rel  <- recs / sum(recs)
top_plant <- rownames(Ms)[apply(Ms, 2, which.max)]
q25 <- as.numeric(stats::quantile(rel, 0.25))
spec_tbl <- data.frame(
  bee_species        = names(deg),
  n_plant_genera     = as.integer(deg),
  n_records          = as.integer(recs),
  rel_frequency      = round(rel, 4),
  top_plant_genus    = top_plant,
  specialist         = deg <= SPECIALIST_MAX_PLANTS,   # narrow forage breadth
  lesser_seen        = rel <= q25,                     # low relative frequency (rare)
  row.names = NULL)
spec_tbl <- spec_tbl[order(spec_tbl$n_plant_genera, -spec_tbl$n_records), ]
write.csv(spec_tbl, file.path(OUT_DIR, "interactions_bee_specialization.csv"), row.names = FALSE)

# ---- 5. OPTIONAL bipartite::plotweb figures (only if the package is present) --
if (HAVE_BIPARTITE) {
  message("bipartite present -- writing plotweb figures.")
  plotweb_png <- function(M, file, rank_label) {
    png(file, width = max(1800, 26 * ncol(M) + 400), height = 1200, res = 170)
    bipartite::plotweb(M, method = "normal",
                       col.low = "#762a83", col.high = "#1b7837",
                       text.rot = 90, labsize = 0.7)
    title(main = sprintf("Plant genus (bottom) - bee %s (top) visitation web", rank_label))
    dev.off()
  }
  plotweb_png(Mg, file.path(OUT_DIR, "interactions_plotweb_genus.png"),   "genus")
  plotweb_png(Ms, file.path(OUT_DIR, "interactions_plotweb_species.png"), "species")
} else {
  message("bipartite NOT installed -- skipped plotweb (heatmaps cover the same data).")
}

# ---- 6. console summary -----------------------------------------------------
message("\nTop 8 bee genera by plant-genera breadth:")
brd <- sort(colSums(Mg > 0), decreasing = TRUE)
print(utils::head(brd, 8))
message("\nSpecialist bee species (visit <= ", SPECIALIST_MAX_PLANTS, " plant genera): ",
        sum(spec_tbl$specialist), " of ", nrow(spec_tbl))
message("Wrote matrices, heatmaps, bee-genus network, and specialization table to ",
        normalizePath(OUT_DIR))
