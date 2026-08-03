# =============================================================
# analysis/interactions_top_plants.R
# beescabr pipeline -- top plants bees visit (Q3)
# Created: 2026-07-21
#
# Q3: "Which plants do bees visit the most -- across the year, and per month --
# so we know what to plant more of?" Counts of bee records per visited plant
# genus (the resolved `plant_genus`). CABR only.
#
# THREE SCOPES (per the stakeholder decision):
#   * whole-park  -- every bee record with a plant genus
#   * survey-only -- is_survey == TRUE
#   * by method   -- lethal (specimen/net) vs non-lethal (iNat/photo)
# Plus a per-MONTH breakdown of the top plants (forage seasonality).
#
# "Just by counts." NOTE: counts reflect sampling effort + detectability, not a
# bee's preference -- a common plant can top the list because it's everywhere and
# easy to photograph. And the season window matters: interns survey ~Mar-Sep,
# beeple (non-lethal) run year-round, so month coverage is uneven.
#
# Run from the repo root:  Rscript scripts/analysis/interactions_top_plants.R
# Depends on: dplyr, stringr (+ config.R). Base-R plots -- no extra packages.
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr) })

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/theme_beescabr.R")   # shared house style
if (!exists("plant_label")) source("scripts/analysis/plant_names.R")          # shared plant common-name labels
OUT_DIR   <- "data/analysis/interactions"
TOP_N     <- 10          # top plants for the headline table/figure
TOP_MONTH <- 12          # plants shown in the month heatmap
COL_LETHAL <- unname(BEE_METHOD_COL["lethal"]); COL_NONLETHAL <- unname(BEE_METHOD_COL["nonlethal"])  # purple net / vermillion photo
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"

# ---- 1. bee records that name a plant genus (both methods) -------------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
rec <- bind_rows(
  spec %>% transmute(plant_genus, method = "lethal",
                     is_survey = is_true(is_survey), date = observed_on),
  inat %>% transmute(plant_genus, method = "nonlethal",
                     is_survey = is_true(is_survey), date = observed_on)
) %>% filter(!is.na(plant_genus), plant_genus != "")
rec$month <- suppressWarnings(as.integer(substr(rec$date, 6, 7)))
message(sprintf("Bee-visit records with a plant genus: %d", nrow(rec)))

# ---- 2. top plants, three scopes --------------------------------------------
tally <- function(df, nm) df %>% count(plant_genus, name = nm)
tbl <- tally(rec, "whole_park") %>%
  full_join(tally(filter(rec, is_survey),            "survey_only"),  by = "plant_genus") %>%
  full_join(tally(filter(rec, method == "lethal"),   "lethal"),       by = "plant_genus") %>%
  full_join(tally(filter(rec, method == "nonlethal"),"nonlethal"),    by = "plant_genus") %>%
  mutate(across(-plant_genus, ~ replace(., is.na(.), 0L))) %>%
  arrange(desc(whole_park))
tbl$rank_whole_park <- seq_len(nrow(tbl))
write.csv(tbl, file.path(OUT_DIR, "interactions_top_plants.csv"), row.names = FALSE)

top <- head(tbl, TOP_N)
message(sprintf("\nTop %d plant genera (whole-park visits):", TOP_N))
print(top[, c("plant_genus", "whole_park", "survey_only", "lethal", "nonlethal")],
      row.names = FALSE)

# ---- 3. figure A: top-N plants, method-split bars ---------------------------
png(file.path(OUT_DIR, "interactions_top_plants.png"),
    width = 2050, height = 1150, res = 200)
bee_base_par()                                    # house-style fonts + muted axis colours
op <- par(mar = c(5.5, 16, 3.5, 1))               # wide left margin for "Common Name (Genus)" labels
M <- rbind(nonlethal = top$nonlethal, lethal = top$lethal)   # stacked
colnames(M) <- paste0(plant_label(top$plant_genus), bee_low_n_mark(top$whole_park))   # common name (Latin); '*' on thinly-sampled plants
M <- M[, ncol(M):1, drop = FALSE]                            # #1 at top
barplot(M, horiz = TRUE, las = 1, border = NA, cex.names = 0.82,
        col = c(nonlethal = COL_NONLETHAL, lethal = COL_LETHAL),
        xlab = "Bee-visit records (whole park)",
        main = sprintf("Top %d plant genera visited by bees at CABR\n(bar split by survey method)", TOP_N))
legend("bottomright", bty = "n", fill = c(COL_NONLETHAL, COL_LETHAL), text.col = BEE_INK$primary,
       legend = c("non-lethal (photo/iNat)", "lethal (net/specimen)"))
if (any(bee_low_n(top$whole_park)))
  mtext(BEE_LOW_N_NOTE, side = 1, line = 4.2, cex = 0.7, adj = 0, col = BEE_INK$secondary)
par(op); dev.off()

# ---- 4. per-month breakdown of the top plants -------------------------------
top_m   <- head(tbl$plant_genus, TOP_MONTH)
top_tot <- head(tbl$whole_park, TOP_MONTH)                  # total bee-visit records = the "favourite" ranking
mm <- rec %>% filter(!is.na(month), plant_genus %in% top_m) %>%
  count(plant_genus, month)
Mmon <- matrix(0L, nrow = length(top_m), ncol = 12,
               dimnames = list(top_m, month.abb))
for (i in seq_len(nrow(mm)))
  Mmon[mm$plant_genus[i], mm$month[i]] <- mm$n[i]
write.csv(data.frame(favourite_rank = seq_along(top_m), plant_genus = rownames(Mmon),
                     total_visits = top_tot, Mmon, check.names = FALSE),
          file.path(OUT_DIR, "interactions_top_plants_by_month.csv"), row.names = FALSE)

# y-axis numbered by the bees' FAVOURITE: rank 1 = most total visit records. Labels carry rank + count.
# plant genus shown as its common name (Latin) via the shared label helper; names() stay the raw
# genus so the matrix-rowname indexing below still lines up.
rank_lab <- setNames(sprintf("%d. %s (%s)", seq_along(top_m), plant_label(top_m),
                             format(top_tot, big.mark = ",")), top_m)

png(file.path(OUT_DIR, "interactions_top_plants_by_month.png"),
    width = 2150, height = 1050, res = 200)
bee_base_par()
op <- par(mar = c(4, 19, 4, 1))                   # wide left margin for "rank. Common Name (Genus) (n)" labels
Mplot <- Mmon[nrow(Mmon):1, , drop = FALSE]
image(x = 1:12, y = seq_len(nrow(Mplot)), z = t(log1p(Mplot)),
      col = grDevices::colorRampPalette(BEE_SEQ)(24), axes = FALSE, xlab = "", ylab = "",   # magnitude = house blue ramp
      main = sprintf("When are the top %d plants visited? (log records/month)\ny-axis ranked by the bees' favourite -- 1 = most visit records", TOP_MONTH))
axis(1, 1:12, month.abb, las = 2, cex.axis = 0.8)
axis(2, seq_len(nrow(Mplot)), rank_lab[rownames(Mplot)], las = 1, cex.axis = 0.66)
mtext("interns survey ~Mar-Sep; beeple year-round -- month coverage is uneven",
      side = 1, line = 2.6, cex = 0.75, col = BEE_INK$secondary)
par(op); dev.off()

# ---- 5. COMBINED figure: top-N ranking + WHEN they're visited, shared rows ---
# One figure, two aligned panels sharing the same plant rows (rank 1 at top):
#   LEFT  = total bee-visit records (method-split bar)  -> magnitude / the ranking
#   RIGHT = month heatmap (log records/month) + a diamond on each plant's peak month -> timing
topc <- head(tbl, TOP_N)
Mc <- matrix(0L, nrow = nrow(topc), ncol = 12, dimnames = list(topc$plant_genus, month.abb))
mmc <- rec %>% filter(!is.na(month), plant_genus %in% topc$plant_genus) %>% count(plant_genus, month)
for (r in seq_len(nrow(mmc))) Mc[mmc$plant_genus[r], mmc$month[r]] <- mmc$n[r]
n    <- nrow(topc)
ord  <- rev(seq_len(n))                                   # plot rank 1 at the TOP (highest y)
lab  <- sprintf("%d. %s", seq_len(n), plant_label(topc$plant_genus))
peak <- max.col(Mc, ties.method = "first")               # each plant's peak (busiest) month
ramp <- grDevices::colorRampPalette(BEE_SEQ)(32); mx <- log1p(max(Mc))
cidx <- function(v) pmax(1, pmin(32, ceiling(31 * log1p(v) / mx) + 1))

# ONE merged graph: each plant is a row carrying BOTH its total-visits bar (left)
# and its 12 monthly cells (right), split by a divider -- magnitude + timing in a single chart.
BARW <- 4.0; GAP <- 1.9; x0 <- BARW + GAP; maxc <- max(topc$whole_park)   # bar region 0..BARW, months start at x0
png(file.path(OUT_DIR, "interactions_top_plants_timing.png"), width = 2450, height = 1180, res = 200)
bee_base_par()
par(mar = c(5, 15.5, 4, 1))
plot.new(); plot.window(xlim = c(0, x0 + 12), ylim = c(0.5, n + 0.5), yaxs = "i")
for (k in seq_len(n)) { p <- ord[k]
  # -- magnitude: method-split total-visits bar, scaled to the bar region
  xn <- topc$nonlethal[p] / maxc * BARW; xl <- topc$lethal[p] / maxc * BARW
  rect(0, k - 0.34, xn, k + 0.34, col = COL_NONLETHAL, border = NA)
  rect(xn, k - 0.34, xn + xl, k + 0.34, col = COL_LETHAL, border = NA)
  text(xn + xl, k, format(topc$whole_park[p], big.mark = ","), pos = 4, offset = 0.2, cex = 0.66, col = BEE_INK$secondary, xpd = NA)
  # -- timing: 12 monthly cells + a diamond on the peak month
  for (mo in 1:12) { v <- Mc[p, mo]; xl2 <- x0 + (mo - 1)
    rect(xl2, k - 0.42, xl2 + 1, k + 0.42, col = if (v == 0) "#f4f2ee" else ramp[cidx(v)], border = "white", lwd = 0.6) }
  points(x0 + peak[p] - 0.5, k, pch = 18, cex = 0.8, col = BEE_INK$primary) }
abline(v = x0 - GAP / 2, col = "#d8d3ca", lwd = 1)                        # divider between the two halves
axis(2, at = seq_len(n), labels = rev(lab), las = 1, cex.axis = 0.72, tick = FALSE)
axis(1, at = x0 + (1:12) - 0.5, labels = month.abb, las = 2, cex.axis = 0.72, tick = FALSE)
mtext("total visits", side = 1, at = BARW / 2, line = 2.6, cex = 0.68, col = BEE_INK$secondary)
mtext("when visited  (shade = log records/month · diamond = peak)", side = 1, at = x0 + 6, line = 3.0, cex = 0.68, col = BEE_INK$secondary)
title(main = sprintf("Top %d plants bees visit -- and when", TOP_N), cex.main = 1.1)
legend("bottomleft", inset = c(0, -0.16), horiz = TRUE, bty = "n", fill = c(COL_NONLETHAL, COL_LETHAL),
       text.col = BEE_INK$primary, cex = 0.72, legend = c("non-lethal (photo)", "lethal (net)"), xpd = NA)
dev.off()

message("\nWrote interactions_top_plants.csv (+_by_month.csv) and three figures to ",
        normalizePath(OUT_DIR))
