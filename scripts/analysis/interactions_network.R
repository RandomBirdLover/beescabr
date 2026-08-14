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
cols <- c("genus", "species", "taxon_rank", "plant_genus", "family")
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
          file.path(OUT_DIR, "interactions_matrix_genus.csv"), row.names = FALSE)
write.csv(data.frame(plant_genus = rownames(Ms), Ms, check.names = FALSE),
          file.path(OUT_DIR, "interactions_matrix_species.csv"), row.names = FALSE)

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
      title = "Which plant-bee pairs anchor the network?",
      subtitle = "A few plant-bee pairs dominate; most cells are sparse, so a handful of generalist hubs anchor the network.",
      caption = scope_cap(
        scope  = sprintf("all records; %d plant genera by %d bee %s", nrow(M), ncol(M), rank_label),
        method = "lethal + non-lethal pooled", rank = rank_label),
      x = paste("bee", rank_label), y = "plant (common name)") +
    ggplot2::scale_y_discrete(labels = function(x) plant_label(x)) +   # common name (Latin) on the plant axis
    theme_beescabr(8) +
    ggplot2::theme(
      plot.title  = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
      axis.text.y = ggplot2::element_text(size = 6),
      panel.grid  = ggplot2::element_blank())
  bee_ggsave(file, g, limitsize = FALSE,
                  width  = max(6, 0.17 * ncol(M) + 3),
                  height = max(6, 0.13 * nrow(M) + 2))
}
heatmap_base <- function(M, file, rank_label, top_plants = 30) {   # fallback if no ggplot2
  ord_p <- order(rowSums(M), decreasing = TRUE); ord_b <- order(colSums(M), decreasing = TRUE)
  M2 <- M[head(ord_p, top_plants), ord_b, drop = FALSE]; M2 <- M2[nrow(M2):1, , drop = FALSE]
  bee_png(file, width = max(1400, 60 * ncol(M2) + 500), height = max(1100, 34 * nrow(M2) + 350), res = 200)
  op <- par(mar = c(12, 9, 4.3, 1), xpd = NA)
  image(seq_len(ncol(M2)), seq_len(nrow(M2)), t(log1p(M2)),
        col = grDevices::colorRampPalette(BEE_SEQ)(24), axes = FALSE, xlab = "", ylab = "",   # non-urgent magnitude = teal ramp
        main = "")
  mtext("Which plant-bee pairs anchor the network?", side = 3, line = 2.5, font = 2, cex = 1.1, col = BEE_INK$primary)
  mtext("A few plant-bee pairs dominate; most cells are sparse -- a handful of generalist hubs anchor the network.",
        side = 3, line = 1.1, cex = 0.78, col = BEE_INK$secondary)   # takeaway
  axis(1, seq_len(ncol(M2)), colnames(M2), las = 2, cex.axis = 0.7)
  axis(2, seq_len(nrow(M2)), plant_label(rownames(M2)), las = 1, cex.axis = 0.7)
  cap <- scope_cap(scope = sprintf("all records; top %d plant genera x %d bee %s", nrow(M2), ncol(M2), rank_label),
                   method = "lethal + non-lethal pooled", rank = rank_label, width = 10000)
  mc  <- max(60, floor(par("pin")[1] / strwidth("n", cex = 0.56, units = "inches") * 0.97))
  cl  <- strsplit(str_wrap(cap, mc), "\n")[[1]]
  for (k in seq_along(cl)) mtext(cl[k], side = 1, line = 8.5 + 1.0 * (k - 1), cex = 0.56, col = BEE_INK$secondary)
  par(op); dev.off()
}
write_heatmap <- if (HAVE_GGPLOT) heatmap_gg else heatmap_base
write_heatmap(Mg, file.path(OUT_DIR, "interactions_heatmap_genus.png"),   "genus")
write_heatmap(Ms, file.path(OUT_DIR, "interactions_heatmap_species.png"), "species")

# ---- 3. bee-genus shared-forage network (igraph one-mode projection) ---------
# DISABLED (kept for reference, not rendered) -- project decision: the shared-forage
# network is a descriptive/exploratory visual whose apparent "generalism" is confounded
# with sampling effort and inflated by the one-mode projection, so it can't PROVE
# specialization structure. We rely on the specialization/selectivity table (Sec. 4) +
# NODF nestedness for the actual claims instead. Flip `if (FALSE)` -> `if (TRUE)` to
# bring both network figures (Sec. 3 + 3b) back.
if (FALSE) {
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
bee_png(file.path(OUT_DIR, "interactions_bee_genus_network.png"),
    width = 1500, height = 1450, res = 200)
set.seed(1)
op <- par(mar = c(7, 1, 4.3, 1), xpd = NA)
plot(beebee_plot,
     vertex.color = BEE_WEB[["bee"]], vertex.frame.color = "white",
     vertex.label.color = "black", vertex.label.cex = 0.6, vertex.label.dist = 0.5,
     edge.width = pmin(0.25 * E(beebee_plot)$weight, 3),
     edge.color = adjustcolor("grey60", 0.5), layout = layout_with_fr,
     main = "")
mtext("Which bee genera forage on the same plants?", side = 3, line = 2.5, font = 2, cex = 1.1, col = BEE_INK$primary)
mtext("Bee genera that forage on the same plants cluster together -- broad generalists in the hub, specialists on the edges.",
      side = 3, line = 1.2, cex = 0.78, col = BEE_INK$secondary)   # takeaway
# ONE combined caption (figure note + standardized scope), together in a single block below the graph
.note <- sprintf(">= %d shared plant genera drawn; node size = plant-genera breadth (small = specialist, visits <= %d plant genera)", MIN_SHARED, SPECIALIST_MAX_PLANTS)
.prov <- scope_cap(scope = sprintf("all records, whole park; bee genera linked when they share >= %d plant genera", MIN_SHARED),
                   method = "lethal + non-lethal pooled",
                   rank = "genus (shared-forage one-mode projection)", width = 10000)
.mc  <- max(60, floor(par("pin")[1] / strwidth("n", cex = 0.56, units = "inches") * 0.97))   # wrap to fit this canvas
.cl  <- c(strsplit(str_wrap(.note, .mc), "\n")[[1]], strsplit(str_wrap(.prov, .mc), "\n")[[1]])   # figure note first, then standardized provenance
for (.k in seq_along(.cl)) mtext(.cl[.k], side = 1, line = 1.8 + 1.0 * (.k - 1), cex = 0.56, col = BEE_INK$secondary)
par(op); dev.off()

# ---- 3b. same network with the UNLINKED genera removed (companion view) -------
# Genera that don't share >= MIN_SHARED plant genera with anyone lose all their edges
# and get parked on the rim by the force layout. This companion drops those isolates so
# only the connected core (genera that actually co-forage) is shown.
iso         <- V(beebee_plot)[degree(beebee_plot) == 0]
beebee_conn <- delete_vertices(beebee_plot, iso)
message(sprintf("Bee-genus network (connected-only view): %d of %d genera kept; %d unlinked (< %d shared plant genera) removed",
                vcount(beebee_conn), vcount(beebee_plot), length(iso), MIN_SHARED))
bee_png(file.path(OUT_DIR, "interactions_bee_genus_network_connected.png"),
    width = 1500, height = 1450, res = 200)
set.seed(1)
op <- par(mar = c(7, 1, 4.3, 1), xpd = NA)
plot(beebee_conn,
     vertex.color = BEE_WEB[["bee"]], vertex.frame.color = "white",
     vertex.label.color = "black", vertex.label.cex = 0.6, vertex.label.dist = 0.5,
     edge.width = pmin(0.25 * E(beebee_conn)$weight, 3),
     edge.color = adjustcolor("grey60", 0.5), layout = layout_with_fr,
     main = "")
mtext("Which bee genera forage on the same plants?", side = 3, line = 2.5, font = 2, cex = 1.1, col = BEE_INK$primary)
mtext("The connected core only -- genera that share no plants with others are dropped, so the co-forage structure reads cleanly.",
      side = 3, line = 1.2, cex = 0.78, col = BEE_INK$secondary)   # takeaway
.note2 <- sprintf(">= %d shared plant genera drawn; %d unlinked genera omitted; node size = plant-genera breadth (small = specialist, visits <= %d plant genera)",
                  MIN_SHARED, length(iso), SPECIALIST_MAX_PLANTS)
.prov2 <- scope_cap(scope = sprintf("all records, whole park; bee genera linked when they share >= %d plant genera (unlinked genera removed)", MIN_SHARED),
                    method = "lethal + non-lethal pooled",
                    rank = "genus (shared-forage one-mode projection)", width = 10000)
.mc2 <- max(60, floor(par("pin")[1] / strwidth("n", cex = 0.56, units = "inches") * 0.97))
.cl2 <- c(strsplit(str_wrap(.note2, .mc2), "\n")[[1]], strsplit(str_wrap(.prov2, .mc2), "\n")[[1]])
for (.k in seq_along(.cl2)) mtext(.cl2[.k], side = 1, line = 1.8 + 1.0 * (.k - 1), cex = 0.56, col = BEE_INK$secondary)
par(op); dev.off()
}  # end DISABLED shared-forage network block (Sec. 3 + 3b) -- see note above

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
GREY_LINK     <- BEE_GENUS_GREY   # non-selective/sparse grey -- single source from theme_beescabr.R
sig_genera <- intersect(selective_genera(), colnames(Mg))         # module orders most-recorded first
# genus palette comes from the theme (single source) -- was a local BEE_GENUS_PALETTE
bee_col <- setNames(rep(GREY_LINK, ncol(Mg)), colnames(Mg))
if (length(sig_genera))
  bee_col[sig_genera] <- BEE_GENUS[((seq_along(sig_genera) - 1) %% length(BEE_GENUS)) + 1]
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

# ---- 4d. each selective bee's FAVOURITE plant (availability-corrected) -> heart line -----
# The thick colour lines show where a bee's visits pile up (raw); the red heart line marks the
# plant it FAVOURS MOST beyond availability in its flight window (same test that drives the colours)
# -- often NOT its thickest line. Hearts are drawn on the SPECIES web ONLY (James: a favourite is a
# species-level read; a genus is an aggregate), from the SPECIES-level selectivity test.
FAVORITE_COL <- BEE_FAVORITE   # favourite-plant heart colour -- single source from theme_beescabr.R
pref_of <- setNames(.sel$preferred_plant, .sel$genus)          # genus favourite (kept for the summary CSV; no longer drawn on the genus web)
pref_of[!(names(pref_of) %in% sig_genera)] <- NA_character_
# SPECIES-level favourite -> the heart on the species web. A species earns a heart only if IT is
# statistically selective (same matched chi-square, >= SELECT_MIN_REC records) -- it does NOT inherit
# its genus's heart. Keys ("Genus epithet") match the species-web node names.
.sel_sp <- selectivity_table_species()
pref_of_species <- setNames(.sel_sp$preferred_plant, .sel_sp$taxon)
pref_of_species[!(.sel_sp$selective %in% TRUE) | is.na(.sel_sp$preferred_plant)] <- NA_character_

# ---- 5. bipartite visitation web (plants bottom, bees top) -- base R ---------
# The classic plotweb look, DEPENDENCY-FREE (no bipartite needed): plant genera as
# bars along the bottom, bee taxa as bars along the top, links weighted by visit
# count, seriated by reciprocal averaging to reduce crossings. Trimmed to the
# busiest plants x bees so labels stay legible (full data live in the matrices).
# draw a small filled heart (parametric curve) at (cx, cy) -- base-R fonts lack a ♥ glyph.
.draw_heart <- function(cx, cy, s = 0.013, col = FAVORITE_COL) {
  t   <- seq(0, 2 * pi, length.out = 60)
  asp <- { p <- par("pin"); if (length(p) == 2 && p[1] > 0) p[2] / p[1] else 0.6 }  # height/width: squash x so the heart stays proportional on a wide device
  hx  <- (16 * sin(t)^3) / 17 * asp
  hy  <- (13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t)) / 15
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

  bee_png(file, width = max(1700, 58 * max(np, nb)), height = 1700, res = 200)
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
  rect(px - pw, yP - 0.012, px + pw, yP + 0.012, col = BEE_WEB["plant"], border = "white")   # plants = forage green (theme token)
  rect(bx - bw, yB - 0.012, bx + bw, yB + 0.012, col = bcol, border = "white")        # bees coloured by selectivity (grey = not selective)
  text(px, yP - 0.02, plant_label(rownames(M)), srt = 90, adj = 1, cex = 0.62, col = BEE_INK$primary)   # plant labels = common name (Latin), black (matches bee labels)
  bee_lab_col <- ifelse(is_grey, BEE_INK$muted, BEE_INK$primary)
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
    mtext(sprintf("Red heart line = each selective %s's single FAVOURITE plant (most-visited vs. availability in the same month/year/method) -- often NOT its thickest line.", rank_label),
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
# ---- family helpers (shared by the genus web + the family-species web below) -----------------
FAMILY_COL   <- BEE_FAMILY          # single source from theme_beescabr.R
FAMILY_ORDER <- BEE_FAMILY_ORDER
.gf     <- inter %>% distinct(genus, family) %>% filter(!is.na(family), family != "")
gen2fam <- setNames(.gf$family, .gf$genus)

# genus web: ALL genera, ordered into labelled FAMILY blocks. SELECTIVE genera are drawn in their
# FAMILY colour (matching the species web); generalist / sparse (<50-record) genera go grey (faint
# links + grey labels) so the full community shows while the selective ones still pop.
web_plot_genus_fam <- function(M, file, col_of_bee, top_plants = 30) {
  M <- M[order(rowSums(M), decreasing = TRUE), , drop = FALSE]
  if (nrow(M) > top_plants) M <- M[seq_len(top_plants), , drop = FALSE]
  M <- M[rowSums(M) > 0, colSums(M) > 0, drop = FALSE]
  gen <- colnames(M)
  fam <- ifelse(gen %in% names(gen2fam), unname(gen2fam[gen]), "Other")
  famL <- intersect(FAMILY_ORDER, unique(fam))
  ord <- order(match(fam, famL), -colSums(M))     # family blocks; within a family, by total visits
  M <- M[, ord, drop = FALSE]; gen <- gen[ord]; fam <- fam[ord]
  np <- nrow(M); nb <- ncol(M)
  pr <- rank((M %*% seq_len(nb)) / rowSums(M), ties.method = "first")
  for (it in 1:6) pr <- rank((M %*% rank((t(M) %*% pr) / colSums(M), ties.method = "first")) / rowSums(M), ties.method = "first")
  M <- M[order(pr), , drop = FALSE]
  GG <- 0.8; FG <- 1.8; gap <- rep(0, nb)          # small gap between genera, bigger gap between families
  for (k in seq_len(nb)[-1]) gap[k] <- if (fam[k] != fam[k - 1]) FG else GG
  pos <- numeric(nb); for (k in seq_len(nb)[-1]) pos[k] <- pos[k - 1] + 1 + gap[k]
  bx <- 0.03 + (pos / max(pos)) * 0.94
  px <- if (np > 1) seq(0.03, 0.97, length.out = np) else 0.5
  yP <- 0.05; yB <- 0.74; csum <- colSums(M)
  bcol <- unname(col_of_bee[gen]); bcol[is.na(bcol)] <- GREY_LINK
  is_grey <- toupper(bcol) == toupper(GREY_LINK)   # col_of_bee flags WHICH genera are selective (grey = not)
  bcol[!is_grey] <- unname(FAMILY_COL[fam])[!is_grey]   # selective genera -> their FAMILY colour (consistent with the species web)
  bee_png(file, width = max(2000, 88 * nb), height = 1800, res = 200)
  bee_base_par(); op <- par(mar = c(11.5, 1, 6.5, 1), xpd = NA)   # extra bottom margin for the scope caption moved below the labels
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
  for (j in order(is_grey, decreasing = TRUE)) for (i in seq_len(np)) if (M[i, j] > 0)   # grey drawn first (behind)
    segments(px[i], yP, bx[j], yB, lwd = 0.3 + 7.5 * (M[i, j] / csum[j]),
             col = adjustcolor(bcol[j], if (is_grey[j]) 0.22 else 0.55))
  pw <- 0.007 + 0.016 * sqrt(rowSums(M) / max(rowSums(M)))
  bw <- 0.006 + 0.011 * sqrt(csum / max(csum))
  rect(px - pw, yP - 0.012, px + pw, yP + 0.012, col = BEE_WEB["plant"], border = "white")   # plants = forage green
  rect(bx - bw, yB - 0.012, bx + bw, yB + 0.012, col = bcol, border = "white")         # selective genera coloured; rest grey
  text(px, yP - 0.02, plant_label_expr(rownames(M)), srt = 90, adj = 1, cex = 0.58, col = BEE_INK$primary)
  text(bx, yB + 0.02, gen, srt = 90, adj = 0, cex = 0.6, col = ifelse(is_grey, BEE_INK$muted, BEE_INK$primary), font = 3)
  yfb <- yB + 0.175                                 # labelled family brackets, each with a down-tick over its genera and an ALWAYS-drawn stem up to the label (consistent with the species web)
  for (ff in famL) {
    idx <- which(fam == ff); x0 <- bx[min(idx)] - bw[min(idx)]; x1 <- bx[max(idx)] + bw[max(idx)]; xc <- (x0 + x1) / 2
    segments(x0, yfb, x1, yfb, col = FAMILY_COL[ff], lwd = 3)
    segments(x0, yfb, x0, yfb - 0.013, col = FAMILY_COL[ff], lwd = 3)
    segments(x1, yfb, x1, yfb - 0.013, col = FAMILY_COL[ff], lwd = 3)
    ly <- yfb + 0.030
    segments(xc, yfb, xc, ly - 0.012, col = FAMILY_COL[ff], lwd = 1.2)
    text(xc, ly, ff, col = FAMILY_COL[ff], font = 2, cex = 0.85)
  }
  mtext("Are the park's bee genera generalists or specialists?", side = 3, line = 5.1, font = 2, cex = 1.12, col = BEE_INK$primary)
  mtext("Most bee genera are generalists (grey); a colored few concentrate their visits on specific plants beyond mere availability.",
        side = 3, line = 4.2, cex = 0.68, col = BEE_INK$secondary)   # takeaway
  mtext(sprintf("plant genus (bottom), bee genus (top, grouped by family)   [top %d plants x %d genera]", np, nb),
        side = 3, line = 3.4, cex = 0.62, col = BEE_INK$secondary)
  mtext("Coloured = genus that favours plants beyond availability (matched chi-square, p<0.05).  Grey = generalist or sparse (<50 records).",
        side = 3, line = 2.8, cex = 0.56, col = BEE_INK$secondary)
  mtext("Thickness = share of that genus's visits.", side = 3, line = 2.2, cex = 0.56, col = BEE_INK$secondary)
  # standardized scope caption at the BOTTOM (side = 1), below the plant labels -- like every other figure
  mtext(scope_cap(scope = sprintf("all records, whole park; %d plant genera x %d bee genera, grouped by family", np, nb),
                  method = "lethal + non-lethal pooled",
                  rank = "genus (colour = selective genera, p<0.05)",
                  control = "plant availability, matched to month x year x method",
                  sig = bee_test("forage selectivity vs availability (Monte-Carlo chi-square)"), width = 300),
        side = 1, line = 9.3, cex = 0.56, col = BEE_INK$secondary)
  par(op); dev.off()
}
web_plot_genus_fam(Mg, file.path(OUT_DIR, "interactions_web_genus.png"), col_of_bee = bee_col, top_plants = 30)   # ALL genera; bee_col colours the selective ones, generalist/sparse go grey
# (the selectivity-coloured SPECIES web was dropped -- redundant with the family-species web below,
#  which shows every species with the same species-level hearts. pref_of_species is still used there.)

# ---- species web nested by GENUS & FAMILY (a la Prendergast et al. 2024) --------------------
# Bee SPECIES on top, ordered into family blocks and, within a family, into genus blocks (small
# gaps separate genera; a bigger gap separates families). Thin genus brackets + thick labelled
# family brackets show the family > genus > species nesting. Colour = family. Red heart = each
# selective SPECIES' availability-corrected favourite plant (species-level test, same as above).
# (FAMILY_COL / FAMILY_ORDER / gen2fam are defined once above, with the genus web.)

web_plot_sgf <- function(M, file, top_plants = 20, favorite_of = NULL,
                         title = "Do bee species share foraging niches?",
                         desc  = "Colour = family.  Thickness = share of that species' visits.  Red heart = each selective species' favourite plant.",
                         scope = "all records, whole park; bee species nested by genus & family") {
  M <- M[order(rowSums(M), decreasing = TRUE), , drop = FALSE]
  if (nrow(M) > top_plants) M <- M[seq_len(top_plants), , drop = FALSE]
  M <- M[rowSums(M) > 0, colSums(M) > 0, drop = FALSE]
  sp  <- colnames(M); gen <- word(sp, 1)
  fam <- ifelse(gen %in% names(gen2fam), unname(gen2fam[gen]), "Other")
  famL <- intersect(FAMILY_ORDER, unique(fam))
  ord <- order(match(fam, famL), gen, -colSums(M))        # family > genus > total visits
  M <- M[, ord, drop = FALSE]; sp <- sp[ord]; gen <- gen[ord]; fam <- fam[ord]
  np <- nrow(M); nb <- ncol(M)
  pr <- rank((M %*% seq_len(nb)) / rowSums(M), ties.method = "first")   # order plants against the fixed bee order
  for (it in 1:6) pr <- rank((M %*% rank((t(M) %*% pr) / colSums(M), ties.method = "first")) / rowSums(M), ties.method = "first")
  M <- M[order(pr), , drop = FALSE]
  # grouped x-positions: small gap between genera, bigger gap between families
  GG <- 0.6; FG <- 1.8; gap <- rep(0, nb)
  for (k in seq_len(nb)[-1]) gap[k] <- if (fam[k] != fam[k - 1]) FG else if (gen[k] != gen[k - 1]) GG else 0
  pos <- numeric(nb); for (k in seq_len(nb)[-1]) pos[k] <- pos[k - 1] + 1 + gap[k]
  bx <- 0.03 + (pos / max(pos)) * 0.94
  px <- if (np > 1) seq(0.03, 0.97, length.out = np) else 0.5
  yP <- 0.05; yB <- 0.70; csum <- colSums(M); bcol <- unname(FAMILY_COL[fam])

  bee_png(file, width = max(2400, 54 * nb), height = 1850, res = 200)
  bee_base_par(); op <- par(mar = c(13.5, 1, 7.5, 1), xpd = NA)   # extra bottom margin for the scope caption moved below the labels
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
  for (j in seq_len(nb)) for (i in seq_len(np)) if (M[i, j] > 0)
    segments(px[i], yP, bx[j], yB, lwd = 0.3 + 7.5 * (M[i, j] / csum[j]), col = adjustcolor(bcol[j], 0.55))
  pw <- 0.0035 + 0.005 * sqrt(rowSums(M) / max(rowSums(M)))   # capped so adjacent plant boxes never merge (half the plant spacing at 50 plants ~ 0.0096)
  bw <- 0.006 + 0.012 * sqrt(csum / max(csum))
  rect(px - pw, yP - 0.012, px + pw, yP + 0.012, col = BEE_WEB["plant"], border = "white")   # plants = forage green
  rect(bx - bw, yB - 0.010, bx + bw, yB + 0.010, col = bcol, border = "white")         # species coloured by family
  text(px, yP - 0.02, plant_label_expr(rownames(M)), srt = 90, adj = 1, cex = 0.6, col = BEE_INK$primary)
  text(bx, yB + 0.015, sp, srt = 90, adj = 0, cex = 0.44, col = BEE_INK$primary, font = 3)
  # red heart line: each selective species' availability-corrected favourite plant
  if (!is.null(favorite_of)) for (j in seq_len(nb)) {
    f <- favorite_of[sp[j]]; if (is.na(f)) next
    i <- match(f, rownames(M)); if (is.na(i)) next
    segments(px[i], yP + 0.012, bx[j], yB - 0.028, col = "white", lwd = 4.2)     # thick white casing so the red line reads clearly
    segments(px[i], yP + 0.012, bx[j], yB - 0.028, col = FAVORITE_COL, lwd = 2.6)
    .draw_heart(bx[j], yB - 0.030, s = 0.013, col = "white")                      # white halo behind the heart
    .draw_heart(bx[j], yB - 0.030, s = 0.010, col = FAVORITE_COL)                 # red heart glyph at the bee end
  }
  # nested family > genus tree: each genus has a little bracket over its species with a stem UP to
  # the family bar; each family bar spans its genus stems with a stem UP to the family label. Every
  # part is connected, and the label stem is ALWAYS drawn (consistent across families and both webs).
  ygb <- yB + 0.195                                 # genus-bracket height (clear of the tallest species label, ~yB+0.165 at cex 0.44)
  yfb <- yB + 0.250                                 # family-bar height
  gcen <- function(gg) { id <- which(gen == gg); (bx[min(id)] + bx[max(id)]) / 2 }   # centre x of a genus block
  for (gg in unique(gen)) {                          # genus tier
    idx <- which(gen == gg); xa <- bx[min(idx)]; xb <- bx[max(idx)]; xcg <- (xa + xb) / 2
    if (xb - xa < 0.006) { xa <- xcg - 0.003; xb <- xcg + 0.003 }   # min width so single-species genera still show a bracket
    gcol <- adjustcolor(FAMILY_COL[fam[min(idx)]], 0.9)
    segments(xa, ygb, xb, ygb, col = gcol, lwd = 2)          # bracket over the genus's species
    segments(xcg, ygb, xcg, yfb, col = gcol, lwd = 1.2)      # stem UP, connecting the genus to its family bar
  }
  for (ff in famL) {                                 # family tier
    gens <- unique(gen[which(fam == ff)]); cxs <- vapply(gens, gcen, numeric(1))
    x0 <- min(cxs); x1 <- max(cxs); xc <- (x0 + x1) / 2
    if (x1 - x0 < 0.006) { x0 <- xc - 0.003; x1 <- xc + 0.003 }
    segments(x0, yfb, x1, yfb, col = FAMILY_COL[ff], lwd = 3)             # family bar spanning its genus stems
    ly <- yfb + 0.030
    segments(xc, yfb, xc, ly - 0.012, col = FAMILY_COL[ff], lwd = 1.2)    # stem UP to the label (always)
    text(xc, ly, ff, col = FAMILY_COL[ff], font = 2, cex = 0.85)
  }
  mtext(title, side = 3, line = 5.6, font = 2, cex = 1.12, col = BEE_INK$primary)
  mtext("Bee species cluster by family into shared foraging niches -- a selective few (hearts) favour specific plants.",
        side = 3, line = 4.7, cex = 0.68, col = BEE_INK$secondary)   # takeaway
  mtext(sprintf("bee species (top; small gaps separate genera, thick brackets = family), plant genus (bottom)   [%d plants x %d species]", np, nb),
        side = 3, line = 3.9, cex = 0.62, col = BEE_INK$secondary)
  mtext(desc, side = 3, line = 3.3, cex = 0.58, col = BEE_INK$secondary)
  # standardized scope caption at the BOTTOM (side = 1), below the plant labels -- like every other figure
  mtext(scope_cap(scope = scope,
                  method = "lethal + non-lethal pooled", rank = "species (favourite = species-level test)",
                  control = "plant availability, matched to month x year x method",
                  sig = bee_test("forage selectivity vs availability (Monte-Carlo chi-square)"), width = 300),
        side = 1, line = 11.3, cex = 0.56, col = BEE_INK$secondary)
  par(op); dev.off()
}
web_plot_sgf(Ms, file.path(OUT_DIR, "interactions_web_family_species.png"), top_plants = 50, favorite_of = pref_of_species)   # 50 plants keeps all 78 species (the last few visit only uncommon plants)
message("Wrote interactions_web_family_species.png (species nested by genus & family)")

# ---- specialists-only species web: ONLY the statistically selective species ------------------
# Same nested family > genus > species layout, but restricted to bee species that forage beyond
# plant availability (matched chi-square, p<0.05) -- every species shown here has a red-heart
# favourite. Lets the selective signal stand out without the generalist majority around it.
sel_species <- .sel_sp$taxon[.sel_sp$selective %in% TRUE & !is.na(.sel_sp$preferred_plant)]
Ms_sel <- Ms[, colnames(Ms) %in% sel_species, drop = FALSE]
if (ncol(Ms_sel) >= 2) {
  web_plot_sgf(Ms_sel, file.path(OUT_DIR, "interactions_web_specialists.png"),
               top_plants = 50, favorite_of = pref_of_species,
               title = "Which bees are the true specialists, and on what?",
               desc  = sprintf("Only the %d bee species that forage beyond plant availability (matched chi-square, p<0.05).  Colour = family.  Red heart = each species' favourite plant.", ncol(Ms_sel)),
               scope = sprintf("selective species only (%d of %d); whole park; nested by genus & family", ncol(Ms_sel), ncol(Ms)))
  message(sprintf("Wrote interactions_web_specialists.png (%d selective species)", ncol(Ms_sel)))
} else {
  message("Skipped interactions_web_specialists.png (fewer than 2 selective species)")
}

# (bipartite::plotweb figures were removed -- the dependency-free web_plot figures
#  above cover the same data legibly; plotweb's label packing was unreadable.)

# ---- 6. console summary -----------------------------------------------------
message("\nTop 8 bee genera by plant-genera breadth:")
brd <- sort(colSums(Mg > 0), decreasing = TRUE)
print(utils::head(brd, 8))
message("\nSpecialist bee species (visit <= ", SPECIALIST_MAX_PLANTS, " plant genera): ",
        sum(spec_tbl$specialist), " of ", nrow(spec_tbl))
message("Wrote matrices, heatmaps, webs, and specialization table to ",
        normalizePath(OUT_DIR))
