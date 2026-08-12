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
if (!exists("BEE_INK")) source("scripts/analysis/theme_beescabr.R")   # shared house style
if (!exists("plant_label")) source("scripts/analysis/plant_names.R")  # shared plant common-name labels
if (!exists("selectivity_table_species")) source("scripts/analysis/forage_selectivity.R")  # species-level selectivity (for the hearts)

# species-level favourite plant, ONLY for statistically selective species (matched chi-square, p<0.05);
# NA otherwise. Drives the red heart marking each selective species' availability-corrected favourite.
.sel_sp <- selectivity_table_species()
pref_of_species <- setNames(.sel_sp$preferred_plant, .sel_sp$taxon)
pref_of_species[!(.sel_sp$selective %in% TRUE) | is.na(.sel_sp$preferred_plant)] <- NA_character_

# red heart glyph (same as the main species web): parametric heart, x squashed by the device aspect
FAVORITE_COL <- BEE_FAVORITE   # single source from theme_beescabr.R
.draw_heart <- function(cx, cy, s = 0.013, col = FAVORITE_COL) {
  t   <- seq(0, 2 * pi, length.out = 60)
  asp <- { p <- par("pin"); if (length(p) == 2 && p[1] > 0) p[2] / p[1] else 0.6 }
  hx  <- (16 * sin(t)^3) / 17 * asp
  hy  <- (13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t)) / 15
  polygon(cx + s * hx, cy + s * hy, col = col, border = NA, xpd = NA)
}

# one distinct colour per bee species within a genus web -> BEE_SPECIES from the theme (single source)
OUT_DIR   <- file.path(DIR_REPORT, "interactions/networks")
WEB_DIR   <- file.path(OUT_DIR, "genus_species_webs")
SPECIES_RANKS <- c("species", "subspecies")
MIN_SPECIES <- 2      # a bee genus needs at least this many species to compare them
MIN_REC     <- 25     # ... and at least this many species+plant records to draw a web
TOP_PLANTS  <- 30     # cap plant genera shown per web for legibility
dir.create(WEB_DIR, recursive = TRUE, showWarnings = FALSE)
set.seed(1)
# scope_cap(): use the SHARED helper from theme_beescabr.R -- adds Source + data-as-of, one canonical order (no local override).

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
# CONFOUNDER-AWARE H2' null. The classic r2dtable null only fixes the row/column marginals, so
# it counts as "niche partitioning" any case where a genus's species visit different plants --
# even if that's just because the species fly in different seasons (different plants bloom) or
# were sampled by different methods. This null instead PERMUTES bee-species labels WITHIN
# (month, method) strata: it asks whether species partition plants MORE than expected once you
# hold constant when-in-season and how each record was collected. Both marginals are still
# preserved (each record keeps its plant; species counts within each stratum are preserved), so
# the same H2max/H2min normalisation applies to obs and null.
#
# Stratifying by MONTH x METHOD (not also year) is deliberate: a genus's own species overlap in
# years, so year is a weak within-genus confound, and adding it would leave most strata with a
# single record -> nothing to permute -> no power. Season is the dominant confound and is
# controlled. n_permutable (records in multi-record strata) is reported so low-power cases show.
h2prime_test_strat <- function(d, nsim = 999) {
  d <- d[!is.na(d$plant_genus) & d$plant_genus != "" & !is.na(d$bee_species), , drop = FALSE]
  plevels <- sort(unique(d$plant_genus)); slevels <- sort(unique(d$bee_species))
  pgf  <- factor(d$plant_genus, plevels)
  Mobs <- matrix(as.numeric(table(pgf, factor(d$bee_species, slevels))),
                 nrow = length(plevels), dimnames = list(plevels, slevels))
  Mobs <- Mobs[rowSums(Mobs) > 0, colSums(Mobs) > 0, drop = FALSE]
  if (nrow(Mobs) < 2 || ncol(Mobs) < 2) return(list(H2prime = NA, null_mean = NA, p = NA, n_permutable = 0L))
  rs <- rowSums(Mobs); cs <- colSums(Mobs)
  H2max <- .shannon(rs) + .shannon(cs)
  H2min <- min(.h2min_entropy(rs, cs), .shannon(as.numeric(Mobs)))
  if (!(H2max > H2min)) return(list(H2prime = NA, null_mean = NA, p = NA, n_permutable = 0L))
  h2p <- function(H2) (H2max - H2) / (H2max - H2min)
  obs <- h2p(.shannon(as.numeric(Mobs)))

  idx      <- split(seq_len(nrow(d)), paste(d$month, d$method, sep = "|"))
  perm_idx <- idx[vapply(idx, length, 1L) > 1]                 # only multi-record strata can shuffle
  sp0      <- d$bee_species
  nullv <- vapply(seq_len(nsim), function(s) {
    sp <- sp0
    for (ix in perm_idx) sp[ix] <- sample(sp0[ix])            # permute species labels within season x method
    h2p(.shannon(as.numeric(table(pgf, factor(sp, slevels)))))
  }, numeric(1))
  list(H2prime = obs, null_mean = mean(nullv), p = (1 + sum(nullv >= obs)) / (nsim + 1),
       n_permutable = sum(vapply(perm_idx, length, 1L)))
}

# ---- bipartite web for one genus (plant genera bottom, bee species top) ------
genus_web <- function(M, file, genus, h2lab, favorite_of = NULL, family = NA) {
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
  epithet   <- sub("^\\S+\\s+", "", colnames(M))            # drop the genus, show species epithet
  scol      <- BEE_SPECIES[((seq_len(nb) - 1) %% length(BEE_SPECIES)) + 1]   # one colour per species (theme token)
  bee_png(file, width = max(1500, 150 * nb), height = 1700, res = 200)
  bee_base_par()                                    # house-style sans font
  op <- par(mar = c(17.5, 1, 10, 1), xpd = NA)      # tall bottom margin (plant labels + scope caption at the bottom) and tall top (title + takeaway + legend lines)
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
  # links coloured by BEE SPECIES; thickness = raw visit records (per-genus view uses counts)
  for (j in seq_len(nb)) for (i in seq_len(np)) if (M[i, j] > 0)
    segments(px[i], yP, bx[j], yB, lwd = 0.5 + 5 * M[i, j] / wmax,
             col = adjustcolor(scol[j], 0.55))
  pw <- 0.010 + 0.022 * sqrt(rowSums(M) / max(rowSums(M)))
  bw <- 0.012 + 0.030 * sqrt(colSums(M) / max(colSums(M)))
  rect(px - pw, yP - 0.013, px + pw, yP + 0.013, col = BEE_WEB["plant"], border = "white")   # plants = forage green (theme token)
  rect(bx - bw, yB - 0.014, bx + bw, yB + 0.014, col = scol, border = "white")        # bee species each its own colour
  text(px, yP - 0.022, plant_label_expr(rownames(M)), srt = 90, adj = 1, cex = 0.62, col = BEE_INK$primary)   # plant labels: common name upright, Latin italic in parentheses
  text(bx, yB + 0.024, epithet, srt = 45, adj = 0, cex = 0.74, col = BEE_INK$primary, font = 3)               # bee species epithet (italic)
  # red heart line: each SELECTIVE species' availability-corrected favourite plant (matched chi-square, p<0.05)
  if (!is.null(favorite_of)) for (j in seq_len(nb)) {
    f <- favorite_of[colnames(M)[j]]; if (is.na(f)) next
    i <- match(f, rownames(M)); if (is.na(i)) next
    segments(px[i], yP + 0.013, bx[j], yB - 0.030, col = "white", lwd = 4.2)      # white casing
    segments(px[i], yP + 0.013, bx[j], yB - 0.030, col = FAVORITE_COL, lwd = 2.6)
    .draw_heart(bx[j], yB - 0.032, s = 0.013, col = "white")                       # white halo
    .draw_heart(bx[j], yB - 0.032, s = 0.010, col = FAVORITE_COL)                  # red heart at the bee end
  }
  fam_txt <- if (!is.na(family) && nzchar(family)) sprintf("family %s", family) else "family unresolved"
  mtext(sprintf("Which Plants Each %s Species Uses", genus),
        side = 3, line = 8.2, font = 2, cex = 1.1, col = BEE_INK$primary)
  mtext(sprintf("%s's species carve up the flora -- each favours its own plants, not the same few.", genus),
        side = 3, line = 7.1, cex = 0.72, col = BEE_INK$secondary)   # takeaway
  mtext(sprintf("%s   |   %s   |   %d species, %d plant genera",
                fam_txt, "species (top), plant genus (bottom)", nb, np),
        side = 3, line = 6.2, cex = 0.66, col = BEE_INK$secondary)
  mtext("Thickness = visit records (thicker = more).  Red heart = a selective species' favourite plant (matched chi-square, p<0.05).",
        side = 3, line = 5.5, cex = 0.62, col = BEE_INK$secondary)
  # ONE standardized scope caption at the BOTTOM (side = 1), below the plant labels -- like every other figure.
  # The within-genus H2' test is folded in as the Analysis: component (no separate red note).
  cap <- scope_cap(scope = sprintf("all records, whole park; %s species vs plant genera", genus),
                   method = "lethal + non-lethal pooled",
                   rank = "species (within-genus partitioning; favourite = species-level test)",
                   sig = h2lab, width = 10000)
  maxchars  <- max(60, floor(par("pin")[1] / strwidth("n", cex = 0.56, units = "inches") * 0.97))
  cap_lines <- strsplit(str_wrap(cap, maxchars), "\n")[[1]]
  for (k in seq_along(cap_lines))
    mtext(cap_lines[k], side = 1, line = 13.8 + 0.9 * (k - 1), cex = 0.56, col = BEE_INK$secondary)
  par(op); dev.off()
}

# ---- 1. interaction records: bee species + plant genus -----------------------
read_prep <- function(f) {
  d <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  pg <- if ("plant_genus" %in% names(d)) d$plant_genus else NA
  sm <- if ("survey_method" %in% names(d)) tolower(str_squish(d$survey_method)) else NA_character_
  fm <- if ("family" %in% names(d)) str_squish(d$family) else NA_character_
  data.frame(bee_genus = str_squish(d$genus), taxon_rank = str_squish(tolower(d$taxon_rank)),
             species = d$species, plant_genus = str_squish(pg), family = fm,
             month  = suppressWarnings(as.integer(substr(d$observed_on, 6, 7))),
             year   = suppressWarnings(as.integer(substr(d$observed_on, 1, 4))),
             method = ifelse(is.na(sm) | sm == "", "unknown", sm),
             stringsAsFactors = FALSE)
}
rec <- bind_rows(read_prep(PATHS$inat_clean), read_prep(PATHS$specimen_clean)) %>%
  mutate(bee_species = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(species) & species != "" &
                                !is.na(bee_genus) & bee_genus != "",
                              paste(bee_genus, word(species, -1)), NA)) %>%
  filter(!is.na(bee_species), !is.na(plant_genus), plant_genus != "")

# bee genus -> family (most-frequent non-empty family per genus) for the web subtitle
fam_of     <- rec %>% filter(!is.na(family), family != "") %>% count(bee_genus, family) %>%
  group_by(bee_genus) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup()
fam_lookup <- setNames(fam_of$family, fam_of$bee_genus)

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
# Clear stale webs first so only the CURRENTLY-significant genera remain (the significant set is
# data-driven and can change as records accrue).
invisible(file.remove(list.files(WEB_DIR, pattern = "\\.png$", full.names = TRUE)))
h2_rows <- list(); sp_rows <- list(); ns_dropped <- character(0)
for (g in keep$bee_genus) {
  d <- rec %>% filter(bee_genus == g)
  M <- as.matrix(table(d$plant_genus, d$bee_species))            # plant genus x bee species
  ht <- h2prime_test_strat(d, nsim = 999)                        # season x method-controlled null
  h2lab <- if (is.na(ht$H2prime)) "H2' undefined (needs >= 2 plant genera and >= 2 species)"
           else sprintf("within-genus H2' = %.2f  (season+method-controlled null %.2f, p = %s) - %s",
                        ht$H2prime, ht$null_mean, signif(ht$p, 2),
                        ifelse(!is.na(ht$p) && ht$p < 0.05, "species partition plants more than expected from timing/method alone",
                               "no more than timing/method already explains"))
  if (!is.na(ht$p) && ht$p < 0.05) {
    genus_web(M, file.path(WEB_DIR, paste0(g, ".png")), g, h2lab, favorite_of = pref_of_species, family = unname(fam_lookup[g]))   # draw only genera that significantly partition
  } else if (!is.na(ht$H2prime)) {
    ns_dropped <- c(ns_dropped, g)                                 # tested, but timing/method already explains it
  }

  h2_rows[[g]] <- data.frame(bee_genus = g, n_species = ncol(M), n_plant_genera = nrow(M),
                             n_records = sum(M), H2prime = round(ht$H2prime, 3),
                             H2prime_null = round(ht$null_mean, 3), H2prime_p = signif(ht$p, 3),
                             n_permutable = ht$n_permutable)
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

# ---- 3. overview: within-genus H2' ------------------------------------------
# ONLY significant bars are drawn: genera whose species SIGNIFICANTLY partition plants (p<0.05).
# Nothing non-significant appears as a bar -- generalists (tested, p>=0.05) and untestable genera
# (>= 2 species but < MIN_REC records) are NAMED in the caption instead, never plotted.
n_tested <- sum(!is.na(h2_tbl$H2prime))
ov <- h2_tbl %>% filter(!is.na(H2prime), !is.na(H2prime_p), H2prime_p < 0.05) %>%
  transmute(bee_genus, x = H2prime, lab = sprintf("%.2f", H2prime))
ov$bee_genus <- factor(ov$bee_genus, levels = rev(ov$bee_genus[order(-ov$x)]))
gen_note <- if (length(ns_dropped))
    sprintf("  Generalists -- species overlap on the same plants, no significant partitioning once flight-season & method are controlled (not shown): %s.",
            paste(sort(ns_dropped), collapse = ", ")) else ""
untest_note <- if (nrow(dropped))
    sprintf("  Untestable (>= %d species but < %d records, too few to test; not shown): %s.",
            MIN_SPECIES, MIN_REC, paste(sort(dropped$bee_genus), collapse = ", ")) else ""
g <- ggplot(ov, aes(x = x, y = bee_genus)) +
  geom_col(width = 0.72, fill = BEE_NEUTRAL[["dark"]]) +
  geom_text(aes(label = lab), hjust = -0.12, size = 3.0, colour = BEE_INK$secondary) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.30))) +
  labs(title = "Which Bee Genera Have Specialist Species?",
       subtitle = sprintf("%d bee genera have species that carve up different plants -- true niche specialists, not chance.", nrow(ov)),
       caption = paste0(str_wrap(paste0(
         sprintf("Higher bar = species partition plants more (H2', controlled for flight season & method). Only the %d of %d testable (>= %d species) genera whose species significantly specialise (p<0.05) are drawn.",
                 nrow(ov), n_tested, MIN_SPECIES), gen_note, untest_note), 96), "\n",
         scope_cap(scope = "all records, whole park; within-genus niche partitioning across bee genera",
                   method = "lethal + non-lethal pooled",
                   rank = "genus (within-genus H2', season+method-controlled null)", width = 96)),
       x = "within-genus H2'   (low = generalists overlap   |   high = specialists partition)", y = NULL) +
  theme_beescabr(11) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        panel.grid.major.y = element_blank())
bee_ggsave(file.path(OUT_DIR, "interactions_genus_h2_overview.png"), g,
       width = 10.5, height = max(3.2, 0.5 * nrow(ov) + 2.0), bg = "white")
message("Wrote interactions_genus_h2.csv, interactions_genus_species_specialization.csv, interactions_genus_h2_overview.png")
