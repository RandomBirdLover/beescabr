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
OUT_DIR   <- "data/analysis/interactions"
TOP_N     <- 10          # top plants for the headline table/figure
TOP_MONTH <- 12          # plants shown in the month heatmap
COL_LETHAL <- "#1b7837"; COL_NONLETHAL <- "#762a83"
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
    width = 1700, height = 1150, res = 200)
op <- par(mar = c(4.5, 9, 3.5, 1))
M <- rbind(nonlethal = top$nonlethal, lethal = top$lethal)   # stacked
colnames(M) <- top$plant_genus
M <- M[, ncol(M):1, drop = FALSE]                            # #1 at top
barplot(M, horiz = TRUE, las = 1, border = NA,
        col = c(nonlethal = COL_NONLETHAL, lethal = COL_LETHAL),
        xlab = "Bee-visit records (whole park)",
        main = sprintf("Top %d plant genera visited by bees at CABR\n(bar split by survey method)", TOP_N))
legend("bottomright", bty = "n", fill = c(COL_NONLETHAL, COL_LETHAL),
       legend = c("non-lethal (photo/iNat)", "lethal (net/specimen)"))
par(op); dev.off()

# ---- 4. per-month breakdown of the top plants -------------------------------
top_m <- head(tbl$plant_genus, TOP_MONTH)
mm <- rec %>% filter(!is.na(month), plant_genus %in% top_m) %>%
  count(plant_genus, month)
Mmon <- matrix(0L, nrow = length(top_m), ncol = 12,
               dimnames = list(top_m, month.abb))
for (i in seq_len(nrow(mm)))
  Mmon[mm$plant_genus[i], mm$month[i]] <- mm$n[i]
write.csv(data.frame(plant_genus = rownames(Mmon), Mmon, check.names = FALSE),
          file.path(OUT_DIR, "interactions_top_plants_by_month.csv"), row.names = FALSE)

png(file.path(OUT_DIR, "interactions_top_plants_by_month.png"),
    width = 1700, height = 1050, res = 200)
op <- par(mar = c(4, 10, 3.5, 1))
Mplot <- Mmon[nrow(Mmon):1, , drop = FALSE]
image(x = 1:12, y = seq_len(nrow(Mplot)), z = t(log1p(Mplot)),
      col = hcl.colors(24, "YlOrRd", rev = TRUE), axes = FALSE, xlab = "", ylab = "",
      main = sprintf("When are the top %d plants visited? (log records/month)", TOP_MONTH))
axis(1, 1:12, month.abb, las = 2, cex.axis = 0.8)
axis(2, seq_len(nrow(Mplot)), rownames(Mplot), las = 1, cex.axis = 0.75)
mtext("interns survey ~Mar-Sep; beeple year-round -- month coverage is uneven",
      side = 1, line = 2.6, cex = 0.75)
par(op); dev.off()

message("\nWrote interactions_top_plants.csv (+_by_month.csv) and two figures to ",
        normalizePath(OUT_DIR))
