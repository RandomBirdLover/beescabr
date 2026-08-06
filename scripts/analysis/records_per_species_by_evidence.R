# =============================================================
# analysis/records_per_species_by_evidence.R
# beescabr pipeline -- CABR native bees: how many records back each SPECIES,
# split by the two survey METHODS. Species-level companion to
# records_per_genus_by_evidence.R (same look, one row per species).
#
#   * lethal      -- a physical specimen (net collection). Voucher-backed.
#   * non-lethal  -- an iNaturalist photo (research-grade AND needs-ID pooled together).
#
# Only records identified to species (species/subspecies rank) appear -- a genus-only
# record can't be placed on a species row. A species with few TOTAL records is flagged
# (*) -- thin evidence base, read its composition with care (>=10-record rule of thumb).
#
# Run from the repo root:  Rscript scripts/analysis/records_per_species_by_evidence.R
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
OUT_DIR       <- "data/analysis/coverage/records_by_evidence"
SPECIES_RANKS <- c("species", "subspecies")
MIN_SHOWN     <- 50    # figure shows only species with >= this many total records (long tail dropped; full list in CSV)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
norm <- function(x) str_squish(as.character(x))

# species binomial (Genus epithet) for species/subspecies-rank rows; NA otherwise
sp_key <- function(df) {
  ok <- tolower(norm(df$taxon_rank)) %in% SPECIES_RANKS & !is.na(df$genus) & norm(df$genus) != "" &
        !is.na(df$species) & norm(df$species) != ""
  ifelse(ok, paste(norm(df$genus), word(norm(df$species), -1)), NA_character_)
}

# ---- 1. pool records, tag METHOD, keep species-level rows -------------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

rec <- bind_rows(
  data.frame(species = sp_key(spec), method = "lethal",    stringsAsFactors = FALSE),
  data.frame(species = sp_key(inat), method = "nonlethal", stringsAsFactors = FALSE)) %>%
  filter(!is.na(species))
message(sprintf("Species-level records: %d (lethal %d, non-lethal %d) across %d species",
                nrow(rec), sum(rec$method == "lethal"), sum(rec$method == "nonlethal"),
                dplyr::n_distinct(rec$species)))

# ---- 2. per-species method composition + total ------------------------------
wide <- rec %>%
  count(species, method, name = "n") %>%
  tidyr::pivot_wider(names_from = method, values_from = n, values_fill = 0)
for (col in c("lethal", "nonlethal")) if (is.null(wide[[col]])) wide[[col]] <- 0L
wide <- wide %>%
  mutate(total = lethal + nonlethal, thin = bee_low_n(total)) %>%
  arrange(desc(total))
write.csv(wide, file.path(OUT_DIR, "records_per_species_by_evidence.csv"), row.names = FALSE)

message("\nTop species by records:")
print(head(wide[, c("species", "lethal", "nonlethal", "total")], 10), row.names = FALSE)

# ---- 3. figure: stacked bars, one row per species, coloured by METHOD --------
# figure shows only species with >= MIN_SHOWN total records; the long tail of 1-2 record
# species is dropped for readability -- the FULL list stays in the CSV written above.
wf  <- wide %>% filter(total >= MIN_SHOWN)
lab <- wf$species
long <- bind_rows(
  data.frame(lab = lab, total = wf$total, method = "lethal",    n = wf$lethal),
  data.frame(lab = lab, total = wf$total, method = "nonlethal", n = wf$nonlethal))
long$method <- factor(long$method, levels = c("lethal", "nonlethal"))
long$lab    <- factor(long$lab, levels = rev(lab))        # biggest total at the top

g <- ggplot(long, aes(x = n, y = lab, fill = method)) +
  geom_col(width = 0.74, position = position_stack(reverse = TRUE)) +
  geom_text(data = wf, aes(x = total, y = factor(species, levels = rev(lab)),
                           label = total), hjust = -0.2, size = 2.7, colour = BEE_INK$secondary, inherit.aes = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(values = BEE_METHOD_COL, labels = BEE_METHOD_LABEL, name = "method") +
  labs(title = "Total Records of Bee Species",
       subtitle = sprintf("Species with >= %d records (%d of %d shown; full list in the CSV)", MIN_SHOWN, nrow(wf), nrow(wide)),
       x = "Number of records", y = NULL) +
  theme_beescabr(11) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 8, face = "italic"))
ggsave(file.path(OUT_DIR, "records_per_species_by_evidence.png"), g,
       width = 9.5, height = max(5, 0.26 * nrow(wf) + 1.7), dpi = 200, bg = "white", limitsize = FALSE)

message("\nWrote records_per_species_by_evidence.{csv,png} to ", OUT_DIR)
