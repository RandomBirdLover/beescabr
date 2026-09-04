# =============================================================
# analysis/coverage/coverage_offtransect.R
# Q6 -- Bees off-transect: what does the park hold that the transects miss?
# beescabr / Cabrillo National Monument (CABR) native bees
#
# THE QUESTION: standardized transect surveys sample fixed routes. Casual,
# off-transect recording (is_survey = FALSE -- park-wide iNaturalist not tied to a
# survey walk) covers the rest of the park. Which bees turn up OFF-transect that
# the surveys are NOT catching, and vice versa?
#
# DEFINITION:
#   on-transect  = is_survey == TRUE  (standardized survey effort; both methods --
#                  every specimen is survey-collected, plus survey iNaturalist)
#   off-transect = is_survey == FALSE (casual park-wide records, BOTH methods --
#                  mostly non-lethal iNaturalist plus a few off-date specimens)
#
# OUTPUTS: species/genus split into on-only / shared / off-only, and the list of
# off-only taxa (candidate park residents the standardized surveys are missing).
# Descriptive -- no hypothesis test, so no p-value.
#
# Run from the repo root:  source("scripts/analysis/coverage/coverage_offtransect.R")
# Depends on: dplyr, stringr, ggplot2 (+ config.R).
# =============================================================

# Dependencies are CHECKED here, not installed: see beescabr_require() in config.R.
if (!exists("beescabr_require")) source("scripts/config.R")
beescabr_require()
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_SET")) source("scripts/analysis/shared/theme_beescabr.R")   # shared house style
OUT_DIR       <- file.path(DIR_REPORT, "coverage/off_transect")
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"
# scope_cap(): use the SHARED helper from theme_beescabr.R -- adds Source + data-as-of, one canonical order (no local override).

# ---- 1. pool records, flag on/off-transect + taxonomy keys ------------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
prep <- function(df, method) data.frame(
  method   = method,
  location = ifelse(is_true(df$is_survey), "on-transect", "off-transect"),
  taxon_rank = df$taxon_rank, genus = df$genus, species = df$species,
  stringsAsFactors = FALSE) %>%
  mutate(species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                                !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
         genus_key   = ifelse(taxon_rank %in% GENUS_RANKS & !is.na(genus) & genus != "", genus, NA))
rec <- bind_rows(prep(spec, "lethal"), prep(inat, "non-lethal"))
message(sprintf("Records: on-transect %d, off-transect %d",
                sum(rec$location == "on-transect"), sum(rec$location == "off-transect")))

# ---- 2. on/off partition per taxon (species and genus rank) ------------------
partition <- function(key_col, rank_label) {
  d <- rec[!is.na(rec[[key_col]]), ]
  by_taxon <- d %>% distinct(location, key = .data[[key_col]]) %>%
    group_by(key) %>%
    summarise(on = any(location == "on-transect"), off = any(location == "off-transect"),
              .groups = "drop") %>%
    mutate(region = case_when(on & off ~ "both", on ~ "on-only", TRUE ~ "off-only"),
           rank = rank_label)
  by_taxon
}
sp_part <- partition("species_key", "species")
gn_part <- partition("genus_key",   "genus")

summ <- bind_rows(sp_part, gn_part) %>% count(rank, region)
write.csv(summ, file.path(OUT_DIR, "bees_found_off_transect_summary.csv"), row.names = FALSE)

# full taxon table + the actionable off-only list
taxa_tbl <- bind_rows(sp_part, gn_part) %>% arrange(rank, region, key) %>%
  select(rank, taxon = key, region, on_transect = on, off_transect = off)
write.csv(taxa_tbl, file.path(OUT_DIR, "bees_found_off_transect_taxa.csv"), row.names = FALSE)
offonly_sp <- taxa_tbl %>% filter(rank == "species", region == "off-only") %>% pull(taxon)

message("Species: ",
        paste(sprintf("%s=%d", summ$region[summ$rank=="species"], summ$n[summ$rank=="species"]), collapse="  "))
message("Genera:  ",
        paste(sprintf("%s=%d", summ$region[summ$rank=="genus"], summ$n[summ$rank=="genus"]), collapse="  "))
message("Off-only species (surveys miss these): ", length(offonly_sp))

# ---- 3. figure: on-only / shared / off-only, species + genus -----------------
# set-overlap colors (two-family scheme): on-only = dark teal (focus), both = light teal (shared core /
# background), off-only = RED (BEE_SET b_only) -- the taxa the surveys MISS get the urgent-red pop. "both"
# takes the calm light teal rather than a teal+red blend (near-complementary hues muddy to taupe).
plot_df <- summ %>%
  mutate(region = factor(region, levels = c("on-only", "both", "off-only")),
         rank = factor(rank, levels = c("species", "genus")))
# horizontal layout ("on its side"): rank on the y-axis, counts run left->right.
# rev() keeps species on top so the panel reads top-down species -> genus.
g <- ggplot(plot_df, aes(x = n, y = rank, fill = region)) +
  # house rule: genus series hatched, species solid (both ranks share this chart)
  ggpattern::geom_col_pattern(aes(pattern = rank), position = position_dodge(0.8), width = 0.7,
    pattern_fill = "white", pattern_colour = NA, pattern_angle = 45,
    pattern_density = 0.08, pattern_spacing = 0.025, pattern_key_scale_factor = 0.4) +
  ggpattern::scale_pattern_manual(values = c(species = "none", genus = "stripe"), guide = "none") +
  geom_text(aes(label = n), position = position_dodge(0.8), hjust = -0.35, size = 3.4, show.legend = FALSE) +
  scale_fill_manual(values = c("on-only" = unname(BEE_SET["a_only"]),
                               "both"     = unname(BEE_SET["shared"]),
                               "off-only" = unname(BEE_SET["b_only"])), name = NULL) +
  guides(fill = guide_legend(override.aes = list(pattern = "none"))) +   # region legend swatches solid (hatch = rank, not region)
  scale_y_discrete(limits = rev) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Do the fixed transects catch every bee?",
       subtitle = sprintf("Off-transect recording turns up %d bee species the fixed transects never caught.", length(offonly_sp)),
       caption = scope_cap(scope = "all records; on-transect (standardized survey effort) vs off-transect (casual park-wide records)",
                           method = "lethal + non-lethal pooled", rank = "species + genus"),
       x = "distinct taxa of bees", y = "taxon rank") +
  theme_beescabr(11) +
  theme(panel.grid.major.y = element_blank(), plot.title = element_text(hjust = 0.5))
bee_ggsave(file.path(OUT_DIR, "bees_found_off_transect.png"), g, width = 8, height = 5.5, bg = "white")
message("Wrote bees_found_off_transect.{png,_summary.csv,_taxa.csv} to ", OUT_DIR)
