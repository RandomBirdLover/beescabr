# =============================================================
# Per-bee-genus species -> plant-genus webs  (zoom-in on Q4)
# beescabr / Cabrillo National Monument (CABR) native bees
#
# THE QUESTION: within a single bee genus, do its SPECIES divide up the plant
# genera (niche partitioning -- each species on a different flower), or do they
# all pile onto the same plants (redundant generalists)? This is the Q4 network
# opened up one bee genus at a time.
#
# FOR EACH qualifying bee genus (>= 2 species resolved to species level and
# >= MIN_REC visitation records) we produce:
#   * a bipartite web: that genus's bee species (top) linked to plant genera
#     (bottom), links weighted by visits -- one PNG per genus;
#   * a within-genus H2' specialization score (0 = its species all visit plants
#     the same way; 1 = each species on its own distinct plants), tested against a
#     fixed-marginal r2dtable null for a p-value;
#   * a per-species breadth row (how many plant genera, top plant, % on it).
# Plus an overview bar chart of H2' across genera (who partitions most).
#
# SCOPE: all records, both methods pooled (matches the Q4 network). Only records
# resolved to bee species AND with a plant genus enter. Descriptive webs; the H2'
# p-value is the one inferential piece.
#
# Run from the repo root:  Rscript scripts/analysis/interactions_genus_species_webs.R
# Depends on: dplyr, stringr, ggplot2 (+ config.R).
# =============================================================

for (pkg in c("ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
OUT_DIR   <- "data/analysis/interactions"
WEB_DIR   <- file.path(OUT_DIR, "genus_species_webs")
SPECIES_RANKS <- c("species", "subspecies")
MIN_SPECIES <- 2      # a bee genus needs at least this many species to compare them
MIN_REC     <- 20     # ... and at least this many species+plant records to draw a web
TOP_PLANTS  <- 30     # cap plant genera shown per web for legibility
dir.create(WEB_DIR, recursive = TRUE, showWarnings = FALSE)
set.seed(1)
scope_cap <- function(scope, method, rank) sprintf("Scope: %s  |  Method: %s  |  Rank: %s",
                                                   scope, method, rank)

# ---- H2' specialization + fixed-marginal null (self-contained) ---------------
.shannon <- function(x) { p <- x / sum(x); p <- p[p > 0]; -sum(p * log(p)) }
.h2min_entropy <- function(rs, cs) {
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
  if (nrow(M) < 2 || ncol(M) < 2) return(list(H2prime = NA, null_mean = NA, p = NA))
  rs <- rowSums(M); cs <- colSums(M)
  H2obs <- .shannon(as.numeric(M))
  H2max <- .shannon(rs) + .shannon(cs)
  # H2min: the sorted-margin packing is a heuristic for the minimum-entropy layout;
  # the observed matrix is itself an achievable configuration, so the true minimum
  # can be no larger than H2obs -> bound by it. This keeps H2' in [0, 1] (H2' = 1 =
  # as concentrated as any layout found here), used identically for obs and nulls.
  H2min <- min(.h2min_entropy(rs, cs), H2obs)
  if (!(H2max > H2min)) return(list(H2prime = NA, null_mean = NA, p = NA))
  h2p  <- function(H2) (H2max - H2) / (H2max - H2min)
  obs  <- h2p(H2obs)
  nullv <- vapply(r2dtable(nsim, rs, cs), function(x) h2p(.shannon(as.numeric(x))), numeric(1))
  list(H2prime = obs, null_mean = mean(nullv), p = (1 + sum(nullv >= obs)) / (nsim + 1))
}

# ---- bipartite web for one genus (plant genera bottom, bee species top) ------
genus_web <- function(M, file, genus, h2lab) {
  M <- M[order(rowSums(M), decreasing = TRUE), , drop = FALSE]
  if (nrow(M) > TOP_PLANTS) M <- M[seq_len(TOP_PLANTS), , drop = FALSE]
  M <- M[rowSums(M) > 0, colSums(M) > 0, drop = FALSE]
  np <- nrow(M); nb <- ncol(M)
  pr <- seq_len(np); bc <- seq_len(nb)                     # reciprocal-averaging seriation
  for (it in 1:8) {
    bc <- rank((t(M) %*% pr) / colSums(M), ties.method = "first")
    pr <- rank((M %*% bc) / rowSums(M),   ties.method = "first")
  }
  M  <- M[order(pr), order(bc), drop = FALSE]
  px <- if (np > 1) seq(0.04, 0.96, length.out = np) else 0.5
  bx <- if (nb > 1) seq(0.10, 0.90, length.out = nb) else 0.5
  yP <- 0.05; yB <- 0.95; wmax <- max(M)
  epithet <- sub("^\\S+\\s+", "", colnames(M))             # drop the genus, show species epithet
  png(file, width = max(1500, 150 * nb), height = 1700, res = 200)
  op <- par(mar = c(9, 1, 8, 1), xpd = NA)
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
  for (i in seq_len(np)) for (j in seq_len(nb)) if (M[i, j] > 0)
    segments(px[i], yP, bx[j], yB, lwd = 0.5 + 5 * M[i, j] / wmax,
             col = adjustcolor("#c9a227", 0.45))
  pw <- 0.010 + 0.022 * sqrt(rowSums(M) / max(rowSums(M)))
  bw <- 0.012 + 0.030 * sqrt(colSums(M) / max(colSums(M)))
  rect(px - pw, yP - 0.013, px + pw, yP + 0.013, col = "#1a9850", border = "white")
  rect(bx - bw, yB - 0.014, bx + bw, yB + 0.014, col = "#4575b4", border = "white")
  text(px, yP - 0.022, rownames(M), srt = 90, adj = 1, cex = 0.62, col = "#1a6b39")
  text(bx, yB + 0.024, epithet, srt = 45, adj = 0, cex = 0.78, col = "#2c5aa0", font = 3)
  mtext(sprintf("%s: species (top, blue) x plant genus (bottom, green)  --  %d species, %d plant genera",
                genus, nb, np), side = 3, line = 5.3, font = 2, cex = 1.0)
  mtext(h2lab, side = 1, line = 7.2, cex = 0.8, col = "#b2182b")
  par(op); dev.off()
}

# ---- 1. interaction records: bee species + plant genus -----------------------
read_prep <- function(f) {
  d <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  pg <- if ("plant_genus" %in% names(d)) d$plant_genus else NA
  data.frame(bee_genus = str_squish(d$genus), taxon_rank = str_squish(tolower(d$taxon_rank)),
             species = d$species, plant_genus = str_squish(pg), stringsAsFactors = FALSE)
}
rec <- bind_rows(read_prep(PATHS$inat_clean), read_prep(PATHS$specimen_clean)) %>%
  mutate(bee_species = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(species) & species != "" &
                                !is.na(bee_genus) & bee_genus != "",
                              paste(bee_genus, word(species, -1)), NA)) %>%
  filter(!is.na(bee_species), !is.na(plant_genus), plant_genus != "")

genus_summary <- rec %>% group_by(bee_genus) %>%
  summarise(n_species = n_distinct(bee_species), n_plant_genera = n_distinct(plant_genus),
            n_records = n(), .groups = "drop")
keep <- genus_summary %>% filter(n_species >= MIN_SPECIES, n_records >= MIN_REC) %>%
  arrange(desc(n_species), desc(n_records))
dropped <- genus_summary %>% filter(n_species >= MIN_SPECIES, n_records < MIN_REC)
message(sprintf("Bee genera with >=%d species & >=%d records: %d (drawing webs). Too sparse: %s",
                MIN_SPECIES, MIN_REC, nrow(keep),
                if (nrow(dropped)) paste(sprintf("%s(%d)", dropped$bee_genus, dropped$n_records), collapse=", ") else "none"))

# ---- 2. per-genus: web + H2' + per-species breadth ---------------------------
h2_rows <- list(); sp_rows <- list()
for (g in keep$bee_genus) {
  d <- rec %>% filter(bee_genus == g)
  M <- as.matrix(table(d$plant_genus, d$bee_species))            # plant genus x bee species
  ht <- h2prime_test(M, nsim = 999)
  h2lab <- if (is.na(ht$H2prime)) "H2' undefined (needs >= 2 plant genera and >= 2 species)"
           else sprintf("within-genus H2' = %.2f  (null %.2f, p = %s) - %s",
                        ht$H2prime, ht$null_mean, signif(ht$p, 2),
                        ifelse(!is.na(ht$p) && ht$p < 0.05, "species partition plants more than chance",
                               "no more partitioned than chance"))
  genus_web(M, file.path(WEB_DIR, paste0(g, ".png")), g, h2lab)

  h2_rows[[g]] <- data.frame(bee_genus = g, n_species = ncol(M), n_plant_genera = nrow(M),
                             n_records = sum(M), H2prime = round(ht$H2prime, 3),
                             H2prime_null = round(ht$null_mean, 3), H2prime_p = signif(ht$p, 3))
  sp_rows[[g]] <- d %>% group_by(bee_genus, bee_species) %>%
    summarise(n_records = n(), n_plant_genera = n_distinct(plant_genus),
              top_plant_genus = names(sort(table(plant_genus), decreasing = TRUE))[1],
              pct_on_top_plant = round(100 * max(table(plant_genus)) / n(), 1), .groups = "drop")
}
h2_tbl <- bind_rows(h2_rows) %>% arrange(desc(H2prime))
sp_tbl <- bind_rows(sp_rows) %>% arrange(bee_genus, desc(n_records))
write.csv(h2_tbl, file.path(OUT_DIR, "interactions_genus_h2.csv"), row.names = FALSE)
write.csv(sp_tbl, file.path(OUT_DIR, "interactions_genus_species_specialization.csv"), row.names = FALSE)
message("Webs written to ", WEB_DIR, " (", nrow(keep), " genera)")
print(h2_tbl, row.names = FALSE)

# ---- 3. overview: within-genus H2' across genera -----------------------------
ov <- h2_tbl %>% filter(!is.na(H2prime)) %>%
  mutate(bee_genus = factor(bee_genus, levels = rev(bee_genus)),
         sig = ifelse(!is.na(H2prime_p) & H2prime_p < 0.05, "p < 0.05", "n.s."))
g <- ggplot(ov, aes(x = H2prime, y = bee_genus, fill = sig)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = sprintf("%.2f", H2prime)), hjust = -0.2, size = 3.2) +
  scale_fill_manual(values = c("p < 0.05" = "#1a9850", "n.s." = "#b8b8b8"), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Within-genus niche partitioning: do a genus's species split up plant genera?",
       subtitle = str_wrap(scope_cap("all records, species-resolved",
                            "lethal + non-lethal pooled", "bee species x plant genus, per bee genus"), 84),
       x = "within-genus H2'  (0 = species overlap on plants, 1 = each on its own)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "#b2182b", size = 9),
        legend.position = "top", panel.grid.major.y = element_blank())
ggsave(file.path(OUT_DIR, "interactions_genus_h2_overview.png"), g, width = 9, height = 5.8, dpi = 200, bg = "white")
message("Wrote interactions_genus_h2.csv, interactions_genus_species_specialization.csv, interactions_genus_h2_overview.png")
