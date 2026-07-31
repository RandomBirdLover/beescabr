# =============================================================
# analysis/records_per_genus_by_evidence.R
# beescabr pipeline -- CABR native bees: how much evidence backs each GENUS,
# and of what quality?
# Created: 2026-07-30
#
# THE QUESTION: for every bee genus recorded at Cabrillo, how many records do we
# hold, and how strong is the evidence behind them? Three evidence tiers, strongest
# to weakest (the shared house 'evidence' ramp -- voucher dark -> needs-ID faint):
#   * specimen voucher       -- a physical specimen exists (net). Strongest.
#   * iNat research-grade    -- 2+ community IDs agree on the photo. Solid.
#   * iNat needs-ID / casual -- one person's photo ID, not yet community-vetted. Weakest.
#
# This is the park-wide companion to coverage_cabr_vs_holway.R (which shows only
# the not-on-Holway taxa): here EVERY genus is shown, so a reader can see at a glance
# which genera rest on vouchers vs which rest on unvetted photos.
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
if (!exists("BEE_EVIDENCE")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_DIR <- "data/analysis/coverage"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
norm <- function(x) str_squish(as.character(x))

# ---- 1. pool records, tag evidence tier, keep only those with a genus --------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

spec_ev <- spec %>%
  filter(!is.na(genus), norm(genus) != "") %>%
  transmute(genus = norm(genus), evidence = "specimen")
inat_ev <- inat %>%
  filter(!is.na(genus), norm(genus) != "") %>%
  transmute(genus = norm(genus),
            evidence = ifelse(tolower(norm(quality_grade)) == "research", "research", "needs_id"))
rec <- bind_rows(spec_ev, inat_ev)
message(sprintf("Records with a genus: %d (specimen %d, research %d, needs-ID %d)",
                nrow(rec), sum(rec$evidence == "specimen"),
                sum(rec$evidence == "research"), sum(rec$evidence == "needs_id")))

# ---- 2. per-genus evidence composition + total ------------------------------
wide <- rec %>%
  count(genus, evidence, name = "n") %>%
  tidyr::pivot_wider(names_from = evidence, values_from = n, values_fill = 0)
for (col in c("specimen", "research", "needs_id"))
  if (is.null(wide[[col]])) wide[[col]] <- 0L          # guard: a tier may be entirely absent
wide <- wide %>%
  mutate(total = specimen + research + needs_id,
         thin  = bee_low_n(total)) %>%                 # shared <10-record flag
  arrange(desc(total))
write.csv(wide, file.path(OUT_DIR, "records_per_genus_by_evidence.csv"), row.names = FALSE)

message("\nTop genera by records:")
print(head(wide[, c("genus", "specimen", "research", "needs_id", "total")], 10), row.names = FALSE)

# ---- 3. figure: stacked bars, one row per genus, coloured by evidence --------
# stack order specimen (dark, base) -> research -> needs_id (faint, tip); genera by total.
lab <- paste0(wide$genus, bee_low_n_mark(wide$total))   # append '*' to thin genera
long <- bind_rows(
  data.frame(genus = wide$genus, lab = lab, total = wide$total, evidence = "specimen", n = wide$specimen),
  data.frame(genus = wide$genus, lab = lab, total = wide$total, evidence = "research", n = wide$research),
  data.frame(genus = wide$genus, lab = lab, total = wide$total, evidence = "needs_id", n = wide$needs_id))
long$evidence <- factor(long$evidence, levels = c("specimen", "research", "needs_id"))
long$lab      <- factor(long$lab, levels = rev(lab))    # biggest total at the top

anyn <- any(wide$thin)
g <- ggplot(long, aes(x = n, y = lab, fill = evidence)) +
  geom_col(width = 0.74, position = position_stack(reverse = TRUE)) +   # specimen (dark) at the base -> needs-ID (faint) at the tip
  geom_text(data = wide, aes(x = total, y = factor(lab, levels = rev(lab)), label = total),
            hjust = -0.2, size = 2.7, colour = BEE_INK$secondary, inherit.aes = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_fill_evidence() +                               # shared teal ordinal ramp
  labs(title = "Total Records of Bee Genera",
       #subtitle = "ordinal confidence ramp: specimen voucher (dark) -> iNat research -> iNat needs-ID (faint)",
       x = "Number of records", y = NULL,
       caption = if (anyn) sprintf("* fewer than %d records total -- thin evidence base; read the composition with care", BEE_MIN_RECORDS) else NULL) +
  theme_beescabr(11) +
  theme(panel.grid.major.y = element_blank())
ggsave(file.path(OUT_DIR, "records_per_genus_by_evidence.png"), g,
       width = 9, height = max(5, 0.30 * nrow(wide) + 1.6), dpi = 200, bg = "white")

message("\nWrote records_per_genus_by_evidence.{csv,png} to ", OUT_DIR)
