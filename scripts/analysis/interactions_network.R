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
for (pkg in c("igraph", "bipartite", "ggplot2", "vegan")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(igraph)
})
HAVE_BIPARTITE <- requireNamespace("bipartite", quietly = TRUE)
HAVE_GGPLOT    <- requireNamespace("ggplot2",   quietly = TRUE)
HAVE_VEGAN     <- requireNamespace("vegan",     quietly = TRUE)

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
OUT_DIR       <- "data/analysis/interactions"
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

# ---- 2. FULL interaction heatmaps -- EVERY plant genus x EVERY bee taxon -----
# The whole network, legibly: a ggplot2 tile map of the complete matrix (busiest
# plant/bee sorted to the corner). Falls back to a base-R top-30 image if ggplot2
# is somehow unavailable.
heatmap_gg <- function(M, file, rank_label) {
  df <- as.data.frame(as.table(M)); names(df) <- c("plant_genus", "bee", "n")
  df$n[df$n == 0] <- NA                                       # blank the empty cells
  df$plant_genus <- factor(df$plant_genus, levels = names(sort(rowSums(M))))
  df$bee         <- factor(df$bee,         levels = names(sort(colSums(M), decreasing = TRUE)))
  g <- ggplot2::ggplot(df, ggplot2::aes(bee, plant_genus, fill = n)) +
    ggplot2::geom_tile(color = "grey90", linewidth = 0.1) +
    ggplot2::scale_fill_viridis_c(option = "D", trans = "log", na.value = "white",
                                  name = "visit\nrecords", breaks = c(1, 5, 25, 100)) +
    ggplot2::labs(
      title = sprintf("Plant genus × bee %s — visitation network (all taxa)", rank_label),
      subtitle = sprintf("%d plant genera × %d bee %s pooled across both methods",
                         nrow(M), ncol(M), rank_label),
      x = paste("bee", rank_label), y = "plant genus") +
    ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
      axis.text.y = ggplot2::element_text(size = 6),
      panel.grid  = ggplot2::element_blank(),
      plot.title  = ggplot2::element_text(face = "bold"))
  ggplot2::ggsave(file, g, dpi = 200, limitsize = FALSE,
                  width  = max(6, 0.17 * ncol(M) + 3),
                  height = max(6, 0.13 * nrow(M) + 2))
}
heatmap_base <- function(M, file, rank_label, top_plants = 30) {   # fallback if no ggplot2
  ord_p <- order(rowSums(M), decreasing = TRUE); ord_b <- order(colSums(M), decreasing = TRUE)
  M2 <- M[head(ord_p, top_plants), ord_b, drop = FALSE]; M2 <- M2[nrow(M2):1, , drop = FALSE]
  png(file, width = max(1400, 60 * ncol(M2) + 500), height = max(1100, 34 * nrow(M2) + 350), res = 200)
  op <- par(mar = c(10, 9, 3, 1))
  image(seq_len(ncol(M2)), seq_len(nrow(M2)), t(log1p(M2)),
        col = hcl.colors(24, "YlGnBu", rev = TRUE), axes = FALSE, xlab = "", ylab = "",
        main = sprintf("Plant genus x bee %s (top %d plants)", rank_label, top_plants))
  axis(1, seq_len(ncol(M2)), colnames(M2), las = 2, cex.axis = 0.7)
  axis(2, seq_len(nrow(M2)), rownames(M2), las = 1, cex.axis = 0.7); par(op); dev.off()
}
write_heatmap <- if (HAVE_GGPLOT) heatmap_gg else heatmap_base
write_heatmap(Mg, file.path(OUT_DIR, "interactions_heatmap_genus.png"),   "genus")
write_heatmap(Ms, file.path(OUT_DIR, "interactions_heatmap_species.png"), "species")

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

# ---- 4b. NETWORK-LEVEL STATISTICS + significance tests -----------------------
# Reportable structure metrics for each network:
#   * connectance          -- realized fraction of possible links (descriptive)
#   * NODF nestedness       -- do specialists interact with subsets of what
#     generalists use? Tested against a null model (vegan::oecosimu, quasiswap =
#     fixed row & column totals) -> p-value.
#   * H2' specialization    -- network-level niche partitioning (Bluthgen et al.
#     2006); 0 = no specialization, 1 = complete. Computed self-contained (no
#     bipartite dependency) and tested for significance against a fixed-marginal
#     null model, so the p-value asks: is the web MORE specialized than webs with
#     the same plant & bee totals but interactions shuffled at random?
#
# H2' internals (all fully self-contained):
#   H2   = two-dimensional Shannon entropy of the weighted interaction matrix.
#   H2max = row-entropy + col-entropy -- the exact maximum joint entropy given the
#           marginal totals (joint entropy is maximized under independence).
#   H2min = entropy of the most concentrated matrix with those marginals, built by
#           the sorted-margin north-west-corner packing.
#   H2'  = (H2max - H2) / (H2max - H2min), bounded [0, 1].
#   Null = r2dtable() (Patefield's algorithm: random tables with the SAME row and
#          column totals) -- the same fixed-marginal null bipartite uses by default.
#   p    = fraction of null webs whose H2' is >= observed (one-sided: specialized).
.shannon <- function(x) { p <- x / sum(x); p <- p[p > 0]; -sum(p * log(p)) }
.h2_entropy <- function(M) .shannon(as.numeric(M))
.h2min_entropy <- function(rs, cs) {           # min entropy: NW-corner concentrated fill
  ri <- sort(rs, decreasing = TRUE); cj <- sort(cs, decreasing = TRUE)
  i <- 1L; j <- 1L; vals <- numeric(0)
  while (i <= length(ri) && j <= length(cj)) {
    x <- min(ri[i], cj[j]); vals <- c(vals, x)
    ri[i] <- ri[i] - x; cj[j] <- cj[j] - x
    if (ri[i] == 0) i <- i + 1L
    if (j <= length(cj) && cj[j] == 0) j <- j + 1L
  }
  .shannon(vals)
}
h2prime_test <- function(M, nsim = 999) {
  M <- M[rowSums(M) > 0, colSums(M) > 0, drop = FALSE]
  rs <- rowSums(M); cs <- colSums(M)
  H2obs <- .h2_entropy(M)
  H2max <- .shannon(rs) + .shannon(cs)
  # observed matrix is achievable, so true H2min <= H2obs -> bound the packing
  # heuristic by it; keeps H2' in [0,1]. (No effect on these networks, where the
  # packing minimum is already below observed.)
  H2min <- min(.h2min_entropy(rs, cs), H2obs)
  h2p   <- function(H2) if (H2max > H2min) (H2max - H2) / (H2max - H2min) else NA_real_
  obs   <- h2p(H2obs)
  sims  <- r2dtable(nsim, rs, cs)
  nullv <- vapply(sims, function(x) h2p(.h2_entropy(x)), numeric(1))
  list(H2prime = obs, null_mean = mean(nullv), null_sd = sd(nullv),
       p = (1 + sum(nullv >= obs)) / (nsim + 1))
}
network_stats <- function(M, label) {
  Mb <- (M > 0) * 1L
  conn <- sum(Mb) / prod(dim(Mb))
  out <- data.frame(network = label, n_plant_genera = nrow(M), n_bee_taxa = ncol(M),
                    links = sum(Mb), connectance = round(conn, 3),
                    NODF = NA_real_, NODF_null_mean = NA_real_, NODF_p = NA_real_,
                    H2prime = NA_real_, H2prime_null_mean = NA_real_, H2prime_p = NA_real_)
  if (HAVE_VEGAN) {
    # alternative = "greater": one-sided ("more nested than the null"), matching the
    # one-sided H2' test below -- both ask "more structured than chance?", same tail.
    on <- try(vegan::oecosimu(Mb, vegan::nestednodf, method = "quasiswap", nsimul = 499,
                              alternative = "greater"),
              silent = TRUE)
    if (!inherits(on, "try-error")) {
      i <- which(names(on$statistic$statistic) == "NODF")
      out$NODF           <- round(on$statistic$statistic[["NODF"]], 2)
      out$NODF_null_mean <- round(mean(on$oecosimu$simulated[i, ]), 2)
      out$NODF_p         <- signif(on$oecosimu$pval[i], 3)
    }
  }
  h2 <- try(h2prime_test(M, nsim = 999), silent = TRUE)
  if (!inherits(h2, "try-error")) {
    out$H2prime           <- round(h2$H2prime, 3)
    out$H2prime_null_mean <- round(h2$null_mean, 3)
    out$H2prime_p         <- signif(h2$p, 3)
  }
  out
}
set.seed(1)   # reproducible null-model p-values (NODF quasiswap + H2' r2dtable)
net_stats <- rbind(network_stats(Mg, "genus"), network_stats(Ms, "species"))
write.csv(net_stats, file.path(OUT_DIR, "interactions_network_stats.csv"), row.names = FALSE)
message("\nNetwork-level statistics:")
print(net_stats, row.names = FALSE)

# ---- 5. bipartite visitation web (plants bottom, bees top) -- base R ---------
# The classic plotweb look, DEPENDENCY-FREE (no bipartite needed): plant genera as
# bars along the bottom, bee taxa as bars along the top, links weighted by visit
# count, seriated by reciprocal averaging to reduce crossings. Trimmed to the
# busiest plants x bees so labels stay legible (full data live in the matrices).
web_plot <- function(M, file, rank_label, top_plants = 30, top_bees = 30) {
  M <- M[order(rowSums(M), decreasing = TRUE), , drop = FALSE]
  if (nrow(M) > top_plants) M <- M[seq_len(top_plants), , drop = FALSE]
  M <- M[, order(colSums(M), decreasing = TRUE), drop = FALSE]
  if (ncol(M) > top_bees) M <- M[, seq_len(top_bees), drop = FALSE]
  M <- M[rowSums(M) > 0, colSums(M) > 0, drop = FALSE]
  np <- nrow(M); nb <- ncol(M)
  pr <- seq_len(np); bc <- seq_len(nb)                 # reciprocal-averaging seriation
  for (it in 1:8) {
    bc <- rank((t(M) %*% pr) / colSums(M), ties.method = "first")
    pr <- rank((M %*% bc) / rowSums(M),   ties.method = "first")
  }
  M  <- M[order(pr), order(bc), drop = FALSE]
  px <- if (np > 1) seq(0.03, 0.97, length.out = np) else 0.5
  bx <- if (nb > 1) seq(0.03, 0.97, length.out = nb) else 0.5
  yP <- 0.04; yB <- 0.96; wmax <- max(M)
  png(file, width = max(1700, 58 * max(np, nb)), height = 1700, res = 200)
  op <- par(mar = c(9, 1, 9, 1), xpd = NA)
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
  for (i in seq_len(np)) for (j in seq_len(nb)) if (M[i, j] > 0)
    segments(px[i], yP, bx[j], yB, lwd = 0.4 + 3.4 * M[i, j] / wmax,
             col = adjustcolor("#c9a227", 0.35))
  pw <- 0.008 + 0.02 * sqrt(rowSums(M) / max(rowSums(M)))
  bw <- 0.008 + 0.02 * sqrt(colSums(M) / max(colSums(M)))
  rect(px - pw, yP - 0.012, px + pw, yP + 0.012, col = "#1a9850", border = "white")
  rect(bx - bw, yB - 0.012, bx + bw, yB + 0.012, col = "#4575b4", border = "white")
  text(px, yP - 0.02, rownames(M), srt = 90, adj = 1, cex = 0.62, col = "#1a6b39")
  text(bx, yB + 0.02, colnames(M), srt = 90, adj = 0, cex = 0.62, col = "#2c5aa0", font = 3)
  mtext(sprintf("Plant genus (bottom) - bee %s (top): visitation web  [top %d x %d]",
                rank_label, np, nb), side = 3, line = 6.5, font = 2, cex = 1.05)
  par(op); dev.off()
}
web_plot(Mg, file.path(OUT_DIR, "interactions_web_genus.png"),   "genus",   30, 28)
web_plot(Ms, file.path(OUT_DIR, "interactions_web_species.png"), "species", 30, 30)

# ---- 5b. OPTIONAL bipartite::plotweb figures (only if the package is present) --
if (HAVE_BIPARTITE) {
  message("bipartite present -- writing plotweb figures.")
  WEB_TOP_PLANTS <- 12    # fewer taxa -> more room between labels (full data in the CSVs)
  WEB_TOP_BEES   <- 12    # bipartite::plotweb packs labels at bar centres, so keep it lean
  # bipartite >= 2.2x rewrote plotweb with NEW arg names (sorting / higher_color /
  # lower_color / link_color / text_size / srt); the classic ones (method / col.high /
  # col.low / text.rot / labsize) were removed. Use the new API, fall back to the
  # legacy API, then to a bare call, so any bipartite version produces a figure.
  plotweb_png <- function(M, file, rank_label) {
    pr <- head(order(rowSums(M), decreasing = TRUE), WEB_TOP_PLANTS)
    bc <- head(order(colSums(M), decreasing = TRUE), WEB_TOP_BEES)
    Mt <- M[sort(pr), sort(bc), drop = FALSE]
    Mt <- Mt[rowSums(Mt) > 0, colSums(Mt) > 0, drop = FALSE]
    ttl <- sprintf("Top plant genera (bottom) x bee %s (top) -- visitation web", rank_label)
    png(file, width = 2600, height = 1600, res = 150)   # wide + tall so vertical labels don't collide
    on.exit(dev.off())
    tryCatch({                                            # new bipartite API (>= 2.2x)
      bipartite::plotweb(Mt, sorting = "normal",
                         higher_color = "#4575b4", lower_color = "#1a9850",
                         link_color = adjustcolor("#d8b365", 0.7),
                         text_size = 0.7, srt = 90,
                         higher_italic = TRUE, lower_italic = TRUE, main = ttl)
    }, error = function(e) tryCatch({                     # legacy bipartite API
      message("  new-API plotweb failed (", conditionMessage(e), ") -- legacy args.")
      bipartite::plotweb(Mt, method = "normal", text.rot = 90,
                         col.high = "#4575b4", col.low = "#1a9850",
                         col.interaction = adjustcolor("#d8b365", 0.7), labsize = 0.7)
      title(main = ttl)
    }, error = function(e2) tryCatch({                    # bare call -- proven to run
      message("  styled plotweb failed -- bare call.")
      bipartite::plotweb(Mt); title(main = ttl)
    }, error = function(e3)
      message("  plotweb skipped for ", rank_label, ": ", conditionMessage(e3)))))
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
