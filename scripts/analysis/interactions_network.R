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
#
# Run from the repo root:  Rscript scripts/analysis/interactions_network.R
# Depends on: dplyr, stringr, igraph, vegan, ggplot2. config.R for paths.
# =============================================================

# ---- dependency guard (install-guarded HERE, not in utils.R -- the pipeline
#      never needs network packages) -----------------------------------------
for (pkg in c("igraph", "ggplot2", "vegan")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(igraph)
})
HAVE_GGPLOT    <- requireNamespace("ggplot2",   quietly = TRUE)
HAVE_VEGAN     <- requireNamespace("vegan",     quietly = TRUE)

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_SEQ")) source("scripts/analysis/theme_beescabr.R")   # shared house style
if (!exists("plant_label")) source("scripts/analysis/plant_names.R")  # shared plant common-name labels
OUT_DIR       <- file.path(DIR_REPORT, "interactions/networks")
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
    ggplot2::scale_fill_gradientn(colours = BEE_SEQ, trans = "log", na.value = "white",
                                  name = "visit\nrecords", breaks = c(1, 5, 25, 100)) +   # magnitude = house blue ramp
    ggplot2::labs(
      title = "Plant and Bee Visitation Heatmap",
      subtitle = sprintf("%d plant genera by %d bee %s, both methods pooled",
                         nrow(M), ncol(M), rank_label),
      x = paste("bee", rank_label), y = "plant (common name)") +
    ggplot2::scale_y_discrete(labels = function(x) plant_label(x)) +   # common name (Latin) on the plant axis
    theme_beescabr(8) +
    ggplot2::theme(
      plot.title  = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
      axis.text.y = ggplot2::element_text(size = 6),
      panel.grid  = ggplot2::element_blank())
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
        col = grDevices::colorRampPalette(BEE_SEQ)(24), axes = FALSE, xlab = "", ylab = "",   # house blue ramp
        main = "Plant and Bee Visitation Heatmap")
  axis(1, seq_len(ncol(M2)), colnames(M2), las = 2, cex.axis = 0.7)
  axis(2, seq_len(nrow(M2)), plant_label(rownames(M2)), las = 1, cex.axis = 0.7); par(op); dev.off()
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
     vertex.color = "#A63D95", vertex.frame.color = "white",
     vertex.label.color = "black", vertex.label.cex = 0.6, vertex.label.dist = 0.5,
     edge.width = pmin(0.25 * E(beebee_plot)$weight, 3),
     edge.color = adjustcolor("grey60", 0.5), layout = layout_with_fr,
     main = "Bee Genera Linked by Shared Plant Genera")
mtext(sprintf(">= %d shared plant genera   |   node size = plant-genera breadth   |   rim = specialists", MIN_SHARED),
      side = 3, line = 0.3, cex = 0.75, col = BEE_INK$secondary)
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

# ---- 4b. forage SELECTIVITY -> which bee genera get a colour in the webs -------
# A bee genus is coloured only if it favours certain plants BEYOND what's available:
# the shared forage_selectivity module runs a Monte-Carlo chi-square of each genus's
# plant-visit counts vs the community-wide plant-use marginal. Selective (p<0.05, enough
# records) -> a distinct colour; sparse or availability-driven -> neutral grey. The SAME
# module also drives the genus field guide's preference column, so figures + guide agree.
if (!exists("selective_genera")) source("scripts/analysis/forage_selectivity.R")
MIN_COLOR_REC <- SELECT_MIN_REC
GREY_LINK     <- "#B8B4AC"
sig_genera <- intersect(selective_genera(), colnames(Mg))         # module orders most-recorded first
# maximally-distinct, dependency-free qualitative palette (Okabe-Ito + Kelly-style extras)
BEE_GENUS_PALETTE <- c("#E69F00","#56B4E9","#009E73","#0072B2","#D55E00","#CC79A7",
                       "#B03A2E","#7D3C98","#117A65","#8B4513","#2C3E50","#E7298A",
                       "#66A61E","#A6761D","#1F78B4","#F0A202","#0AA6A6","#5E35B1",
                       "#7E6E00","#8D6E63")
bee_col <- setNames(rep(GREY_LINK, ncol(Mg)), colnames(Mg))
if (length(sig_genera))
  bee_col[sig_genera] <- BEE_GENUS_PALETTE[((seq_along(sig_genera) - 1) %% length(BEE_GENUS_PALETTE)) + 1]
GENUS_COLOR <- function(g) { v <- unname(bee_col[g]); v[is.na(v)] <- GREY_LINK; v }
message(sprintf("Selective (coloured) bee genera [n>=%d & p<0.05]: %d -- %s",
                MIN_COLOR_REC, length(sig_genera), paste(sig_genera, collapse = ", ")))

# ---- 4c. forage-selectivity SUMMARY table (per-genus findings, one row/genus) ---
# Companion to the interaction webs (same test), in the project's *_summary.csv style:
# the statistics (chi-square p, records) + the finding (Selective/Generalist and, for
# selective genera, the plant favoured most RELATIVE to availability vs the plant merely
# recorded most). This is the readable, sortable digest of what drives the web colours.
.sel <- selectivity_table()
# only report a "preferred plant" where selectivity is statistically supported -- otherwise
# the ratio isn't a real preference (matches the guide's "Generalist / too few" cells).
.pref_lab <- plant_label(.sel$preferred_plant); .pref_lab[is.na(.sel$preferred_plant) | !.sel$selective] <- "-"
.pref_ratio <- .sel$preferred_ratio; .pref_ratio[!.sel$selective] <- NA_real_
sel_summary <- data.frame(
  genus                    = .sel$genus,
  visit_records            = .sel$n_records,
  years_spanned            = .sel$n_years,                                # how many distinct years the records cover
  top_year_pct             = .sel$top_year_pct,                           # % of records in its single biggest year
  plant_genera_used        = .sel$n_plants,
  forage_pattern           = ifelse(.sel$selective, "Selective",
                              ifelse(.sel$n_records >= SELECT_MIN_REC, "Generalist", "Too few records")),
  chi_p_matched            = round(.sel$chi_p, 4),                        # PRIMARY: availability matched to the genus's month + year + method
  chi_p_abundance          = round(.sel$chi_p_abundance, 4),             # reference: overall-abundance test (ignores timing/year/method)
  preferred_plant          = .pref_lab,                                  # most-visited vs season+year-expected (selective genera only)
  preferred_vs_available_x = .pref_ratio,                                # e.g. 103 = 103x its availability in the same year-months
  top_plant_recorded       = plant_label(.sel$top_plant),                # most-recorded (availability-blended; all genera)
  stringsAsFactors = FALSE)
write.csv(sel_summary, file.path(OUT_DIR, "forage_selectivity_summary.csv"), row.names = FALSE)
message(sprintf("Wrote forage_selectivity_summary.csv (%d genera: %d selective, %d generalist, %d too few)",
                nrow(sel_summary), sum(sel_summary$forage_pattern == "Selective"),
                sum(sel_summary$forage_pattern == "Generalist"),
                sum(sel_summary$forage_pattern == "Too few records")))

# ---- 4d. each selective genus's FAVOURITE plant (availability-corrected) -> heart line -----
# The thick colour lines show where a genus's visits pile up (raw); the red heart line marks
# the plant it FAVOURS MOST beyond availability in its flight window (from the same test that
# drives the colours). For a genus like Bombus these differ (most-visited buckwheat vs favoured
# milkvetch), which is the whole point of showing both.
FAVORITE_COL <- "#E8000B"   # PLACEHOLDER heart-line colour (pure red) -- parallel session owns colours: fold into a theme_beescabr.R BEE_WEB token
pref_of <- setNames(.sel$preferred_plant, .sel$genus)          # module's phenology-corrected favourite per genus
pref_of[!(names(pref_of) %in% sig_genera)] <- NA_character_    # heart only for statistically selective genera

# ---- 5. bipartite visitation web (plants bottom, bees top) -- base R ---------
# The classic plotweb look, DEPENDENCY-FREE (no bipartite needed): plant genera as
# bars along the bottom, bee taxa as bars along the top, links weighted by visit
# count, seriated by reciprocal averaging to reduce crossings. Trimmed to the
# busiest plants x bees so labels stay legible (full data live in the matrices).
# draw a small filled heart (parametric curve) at (cx, cy) -- base-R fonts lack a ♥ glyph.
.draw_heart <- function(cx, cy, s = 0.013, col = FAVORITE_COL) {
  t  <- seq(0, 2 * pi, length.out = 60)
  hx <- (16 * sin(t)^3) / 17 * 0.6            # narrow the x-extent so the heart isn't stretched wide
  hy <- (13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t)) / 15
  polygon(cx + s * hx, cy + s * hy, col = col, border = NA, xpd = NA)
}

# col_of_bee: named vector (bee column name -> colour); grey where absent/NA. thickness:
#   "share" -> line width = fraction of THAT bee's visits on the plant (preference vs its
#              other plants); "count" -> width = raw visit records (shared/common plants).
# legend_map: optional named vector (label -> colour) drawn as a colour key (for the
# species web, whose top bars are species but colour encodes GENUS).
web_plot <- function(M, file, rank_label, top_plants = 30, top_bees = 30,
                     col_of_bee = NULL, thickness = c("share", "count"), legend_map = NULL,
                     favorite_of = NULL, show_grey_links = FALSE, sparse_omitted = FALSE) {
  thickness <- match.arg(thickness)
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
  yP <- 0.04; yB <- 0.96; wmax <- max(M); csum <- colSums(M)

  # per-bee colour + whether it is a "grey" (background) bee
  bcol <- if (is.null(col_of_bee)) setNames(rep(GREY_LINK, nb), colnames(M))
          else { v <- unname(col_of_bee[colnames(M)]); v[is.na(v)] <- GREY_LINK; setNames(v, colnames(M)) }
  is_grey <- toupper(bcol) == toupper(GREY_LINK)

  png(file, width = max(1700, 58 * max(np, nb)), height = 1700, res = 200)
  bee_base_par()                                    # house-style sans font
  op <- par(mar = c(9, 1, 9, 1), xpd = NA)
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
  link_lwd <- function(v, j) if (thickness == "share") 0.3 + 7.5 * (v / csum[j]) else 0.4 + 3.4 * (v / wmax)
  draw_col <- function(j, grey_layer) {              # draw one bee's links
    for (i in seq_len(np)) if (M[i, j] > 0) {
      lw <- link_lwd(M[i, j], j)
      segments(px[i], yP, bx[j], yB,
               lwd = if (grey_layer) lw * 0.7 else lw,
               col = adjustcolor(bcol[j], if (grey_layer) 0.20 else 0.60))
    }
  }
  # non-selective ("not reliable yet") bees: show no links by default -- keep only their bars/labels
  if (show_grey_links) for (j in which(is_grey)) draw_col(j, TRUE)
  for (j in which(!is_grey)) draw_col(j, FALSE)      # coloured (selective) bees carry the lines

  pw <- 0.008 + 0.02 * sqrt(rowSums(M) / max(rowSums(M)))
  bw <- 0.008 + 0.02 * sqrt(csum / max(csum))
  rect(px - pw, yP - 0.012, px + pw, yP + 0.012, col = "#3E7D43", border = "white")   # plants = superbloom green (forage side)
  rect(bx - bw, yB - 0.012, bx + bw, yB + 0.012, col = bcol, border = "white")        # bees coloured by selectivity (grey = not selective)
  text(px, yP - 0.02, plant_label(rownames(M)), srt = 90, adj = 1, cex = 0.62, col = "#2C2A26")   # plant labels = common name (Latin), black (matches bee labels)
  bee_lab_col <- ifelse(is_grey, "#8a867d", "#2C2A26")
  text(bx, yB + 0.02, colnames(M), srt = 90, adj = 0, cex = 0.62, col = bee_lab_col, font = 3)

  # RED HEART LINE: each selective genus's availability-corrected FAVOURITE plant (may differ
  # from its thickest/most-visited line). Drawn last so it reads on top of the visit lines.
  n_heart <- 0
  if (!is.null(favorite_of)) {
    for (j in seq_len(nb)) {
      fav <- favorite_of[colnames(M)[j]]
      if (is.na(fav)) next
      i <- match(fav, rownames(M)); if (is.na(i)) next            # favourite outside the plotted top plants -> skip
      segments(px[i], yP + 0.012, bx[j], yB - 0.028, col = "white", lwd = 3.2)        # white casing so the red separates from the links behind
      segments(px[i], yP + 0.012, bx[j], yB - 0.028, col = FAVORITE_COL, lwd = 1.8)
      .draw_heart(bx[j], yB - 0.030, s = 0.0145, col = "white")                       # white halo
      .draw_heart(bx[j], yB - 0.030, s = 0.0115, col = FAVORITE_COL)                  # pure-red heart at the bee end
      n_heart <- n_heart + 1
    }
  }

  mtext("Plant and Bee Visitation Network", side = 3, line = 8.3, font = 2, cex = 1.15, col = BEE_INK$primary)
  mtext(sprintf("plant genus (bottom), bee %s (top)   [top %d x %d]", rank_label, np, nb),
        side = 3, line = 7.6, cex = 0.7, col = BEE_INK$secondary)
  thick <- if (thickness == "share") "Thickness = share of that bee's visits (where visits pile up)."
           else "Thickness = number of visit records."
  if (sparse_omitted) {
    mtext("Colour = genus that favours plants beyond availability -- matched to the same month, year & survey method it was recorded (matched chi-square, p<0.05).",
          side = 3, line = 7.0, cex = 0.6, col = BEE_INK$secondary)
    mtext(sprintf("Generalists and sparse (<%d-record) genera are omitted; sparse ones auto-return as data grows.   Thickness = share of that bee's visits.",
                  MIN_COLOR_REC), side = 3, line = 6.2, cex = 0.6, col = BEE_INK$secondary)
  } else {
    grey_note <- if (show_grey_links) "grey = not selective / too few records." else "Grey = not reliable yet -- bars only, no links."
    mtext(sprintf("Colour = genus that favours plants beyond availability in its flight window (phenology-weighted chi-square, p<0.05, >=%d records).",
                  MIN_COLOR_REC), side = 3, line = 7.0, cex = 0.6, col = BEE_INK$secondary)
    mtext(paste0(grey_note, "   ", thick), side = 3, line = 6.2, cex = 0.6, col = BEE_INK$secondary)
  }
  if (n_heart > 0)
    mtext("Red heart line = each selective genus's single FAVOURITE plant (most-visited vs. availability in the same month/year/method) -- often NOT its thickest line.",
          side = 3, line = 5.4, cex = 0.6, col = FAVORITE_COL)
  if (!is.null(legend_map) && length(legend_map))
    legend("top", inset = c(0, -0.075), horiz = FALSE, ncol = min(8, length(legend_map)),
           legend = names(legend_map), fill = unname(legend_map), border = NA,
           bty = "n", cex = 0.6, text.col = BEE_INK$primary, x.intersp = 0.6)
  par(op); dev.off()
}
# genus web: colour by selective bee genus; thickness = each genus's plant-preference share.
# Show ONLY genera with a real (statistically significant) plant preference. This drops both
# the too-sparse genera AND the generalists (enough data but no favourite, e.g. Megachile /
# Nomada). Fully automatic: a genus appears the moment it shows a significant preference.
show_genera <- .sel$genus[.sel$selective]
Mg_show <- Mg[, intersect(colnames(Mg), show_genera), drop = FALSE]
web_plot(Mg_show, file.path(OUT_DIR, "interactions_web_genus.png"), "genus", 30, Inf,
         col_of_bee = bee_col, thickness = "share", favorite_of = pref_of, sparse_omitted = TRUE)   # red heart = favourite; sparse genera dropped
# species web: colour links by the species' GENUS (same palette); thickness = share.
# Show ONLY species whose GENUS has a real plant preference -- the "not reliable yet" species
# are DROPPED entirely (not greyed), matching the genus web; then top 30 of what remains.
sp_reliable <- word(colnames(Ms), 1) %in% .sel$genus[.sel$selective]
Ms_show <- Ms[, sp_reliable, drop = FALSE]
sp_col  <- setNames(GENUS_COLOR(word(colnames(Ms_show), 1)), colnames(Ms_show))
web_plot(Ms_show, file.path(OUT_DIR, "interactions_web_species.png"), "species", 30, 30,
         col_of_bee = sp_col, thickness = "share", sparse_omitted = TRUE)   # colour = genus; unreliable species omitted

# (bipartite::plotweb figures were removed -- the dependency-free web_plot figures
#  above cover the same data legibly; plotweb's label packing was unreadable.)

# ---- 6. console summary -----------------------------------------------------
message("\nTop 8 bee genera by plant-genera breadth:")
brd <- sort(colSums(Mg > 0), decreasing = TRUE)
print(utils::head(brd, 8))
message("\nSpecialist bee species (visit <= ", SPECIALIST_MAX_PLANTS, " plant genera): ",
        sum(spec_tbl$specialist), " of ", nrow(spec_tbl))
message("Wrote matrices, heatmaps, bee-genus network, and specialization table to ",
        normalizePath(OUT_DIR))
