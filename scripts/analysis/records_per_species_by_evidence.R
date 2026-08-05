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
lab <- paste0(wide$species, bee_low_n_mark(wide$total))   # append '*' to thin species
long <- bind_rows(
  data.frame(lab = lab, total = wide$total, method = "lethal",    n = wide$lethal),
  data.frame(lab = lab, total = wide$total, method = "nonlethal", n = wide$nonlethal))
long$method <- factor(long$method, levels = c("lethal", "nonlethal"))
long$lab    <- factor(long$lab, levels = rev(lab))        # biggest total at the top

anyn <- any(wide$thin)
g <- ggplot(long, aes(x = n, y = lab, fill = method)) +
  geom_col(width = 0.74, position = position_stack(reverse = TRUE)) +
  geom_text(data = wide, aes(x = total, y = factor(paste0(species, bee_low_n_mark(total)), levels = rev(lab)),
                             label = total), hjust = -0.2, size = 2.4, colour = BEE_INK$secondary, inherit.aes = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(values = BEE_METHOD_COL, labels = BEE_METHOD_LABEL, name = "method") +
  labs(title = "Total Records of Bee Species",
       x = "Number of records", y = NULL,
       caption = if (anyn) sprintf("* fewer than %d records total -- thin evidence base; read the composition with care", BEE_MIN_RECORDS) else NULL) +
  theme_beescabr(11) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 7.5, face = "italic"))
ggsave(file.path(OUT_DIR, "records_per_species_by_evidence.png"), g,
       width = 9.5, height = max(6, 0.20 * nrow(wide) + 1.6), dpi = 200, bg = "white", limitsize = FALSE)

message("\nWrote records_per_species_by_evidence.{csv,png} to ", OUT_DIR)
