# =============================================================
# analysis/records_per_genus_by_evidence.R
# beescabr pipeline -- CABR native bees: how much evidence backs each GENUS,
# split by the two survey METHODS.
# Created: 2026-07-30
#
# THE QUESTION: for every bee genus recorded at Cabrillo, how many records do we
# hold, and which METHOD produced them?
#   * lethal      -- a physical specimen (net collection). Voucher-backed.
#   * non-lethal  -- an iNaturalist photo (research-grade AND needs-ID pooled together).
#
# This is the park-wide companion to coverage_cabr_vs_holway.R (which shows only
# the not-on-Holway taxa): here EVERY genus is shown, so a reader can see at a glance
# which genera rest on netted specimens vs which rest on photos.
#
# A genus with few TOTAL records is flagged (*) -- its evidence base is thin, so
# read its composition with care (matches the project's >=10-record rule of thumb).
#
# Run from the repo root:  Rscript scripts/analysis/records_per_genus_by_evidence.R
# Depends on: dplyr, stringr, ggplot2 (+ config.R, theme_beescabr.R).
# =============================================================

for (pkg in c("ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_DIR <- "data/analysis/coverage/records_by_evidence"
MIN_SHOWN <- 50    # figure shows only genera with >= this many total records (long tail dropped; full list in CSV)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
norm <- function(x) str_squish(as.character(x))

# ---- 1. pool records, tag METHOD (lethal = specimen, non-lethal = any iNat photo) ----
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

spec_ev <- spec %>%
  filter(!is.na(genus), norm(genus) != "") %>%
  transmute(genus = norm(genus), method = "lethal")
inat_ev <- inat %>%                                    # research-grade + needs-ID pooled = non-lethal
  filter(!is.na(genus), norm(genus) != "") %>%
  transmute(genus = norm(genus), method = "nonlethal")
rec <- bind_rows(spec_ev, inat_ev)
message(sprintf("Records with a genus: %d (lethal/specimen %d, non-lethal/iNat %d)",
                nrow(rec), sum(rec$method == "lethal"), sum(rec$method == "nonlethal")))

# ---- 2. per-genus method composition + total --------------------------------
wide <- rec %>%
  count(genus, method, name = "n") %>%
  tidyr::pivot_wider(names_from = method, values_from = n, values_fill = 0)
for (col in c("lethal", "nonlethal"))
  if (is.null(wide[[col]])) wide[[col]] <- 0L          # guard: a method may be entirely absent
wide <- wide %>%
  mutate(total = lethal + nonlethal,
         thin  = bee_low_n(total)) %>%                 # shared <10-record flag
  arrange(desc(total))
write.csv(wide, file.path(OUT_DIR, "records_per_genus_by_evidence.csv"), row.names = FALSE)

message("\nTop genera by records:")
print(head(wide[, c("genus", "lethal", "nonlethal", "total")], 10), row.names = FALSE)

# ---- 3. figure: stacked bars, one row per genus, coloured by METHOD ----------
# stack order lethal (rose, base) -> non-lethal (periwinkle, tip); genera by total.
# figure shows only genera with >= MIN_SHOWN total records; the long tail is dropped for
# readability -- the FULL list stays in the CSV written above.
wf  <- wide %>% filter(total >= MIN_SHOWN)
lab <- wf$genus
long <- bind_rows(
  data.frame(genus = wf$genus, lab = lab, total = wf$total, method = "lethal",    n = wf$lethal),
  data.frame(genus = wf$genus, lab = lab, total = wf$total, method = "nonlethal", n = wf$nonlethal))
long$method <- factor(long$method, levels = c("lethal", "nonlethal"))
long$lab    <- factor(long$lab, levels = rev(lab))     # biggest total at the top

g <- ggplot(long, aes(x = n, y = lab, fill = method)) +
  geom_col(width = 0.74, position = position_stack(reverse = TRUE)) +   # lethal (base) -> non-lethal (tip)
  geom_text(data = wf, aes(x = total, y = factor(genus, levels = rev(lab)), label = total),
            hjust = -0.2, size = 2.7, colour = BEE_INK$secondary, inherit.aes = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(values = BEE_METHOD_COL, labels = BEE_METHOD_LABEL, name = "method") +
  labs(title = "Total Records of Bee Genera",
       subtitle = sprintf("Genera with >= %d records (%d of %d shown; full list in the CSV)", MIN_SHOWN, nrow(wf), nrow(wide)),
       x = "Number of records", y = NULL) +
  theme_beescabr(11) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank())
ggsave(file.path(OUT_DIR, "records_per_genus_by_evidence.png"), g,
       width = 9, height = max(5, 0.34 * nrow(wf) + 1.7), dpi = 200, bg = "white")

message("\nWrote records_per_genus_by_evidence.{csv,png} to ", OUT_DIR)
