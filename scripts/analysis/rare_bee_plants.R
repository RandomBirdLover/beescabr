# =============================================================
# analysis/rare_bee_plants.R
# beescabr -- plants used by the park's RARE / at-risk native bees  (for management)
#
# THE QUESTION: which plants do the park's rare bees rely on, so management knows
# what to protect and plant? "Rare" here is two things:
#   (a) NAMED conservation-priority bumble bees -- Crotch's (Bombus crotchii, a
#       California Endangered Species Act candidate) and Sonoran (Bombus sonorus,
#       the at-risk American-bumble-bee lineage); and
#   (b) every bee species we have FEWER THAN `RARE_CUT` records of (rarely seen here).
#
# A "plant visit" = a bee record (specimen net OR iNaturalist photo) that carries a
# plant_genus, pooled across methods. Counts are small (these bees are rarely seen) --
# read them as "where the few sightings concentrate," not as visit rates. No p-value.
#
# TWO figures:
#   A. HUBS  -- plant genera ranked by HOW MANY different rare (< RARE_CUT record) bee
#      species use them. A plant feeding many rare bees is a conservation hub.
#   B. NAMED -- the two flagship bees' own plant lists (per species).
#
# Run from the repo root:  Rscript scripts/analysis/rare_bee_plants.R
# Depends on: dplyr, stringr, ggplot2 (+ config.R).
# =============================================================

for (pkg in c("ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_SEQ")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_DIR <- "data/analysis/conservation"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
# scope_cap() now provided by theme_beescabr.R (adds n / sig / source + data date)

RARE_CUT      <- 15                              # a bee is "rare" if we have < this many records
SPECIES_RANKS <- c("species", "subspecies")
NAMED <- data.frame(
  species_key = c("Bombus crotchii", "Bombus sonorus"),
  label = c("Crotch's bumble bee  (Bombus crotchii)",
            "Sonoran bumble bee  (Bombus sonorus)"),
  stringsAsFactors = FALSE)
has <- function(x) !is.na(x) & x != ""

# ---- 1. all bee records: species key + the plant they were on ----------------
grab <- function(df, method) data.frame(
  method        = method,
  taxon_rank    = tolower(str_squish(df$taxon_rank)),
  genus         = str_squish(df$genus),
  epithet       = tolower(word(str_squish(df$species), -1)),
  plant_genus   = str_squish(df$plant_genus),
  plant_species = str_squish(df$plant_species),
  stringsAsFactors = FALSE)
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
rec  <- bind_rows(grab(spec, "specimen (net)"), grab(inat, "photo (iNat)")) %>%
  mutate(species_key = ifelse(taxon_rank %in% SPECIES_RANKS & genus != "" & epithet != "",
                              paste(genus, epithet), NA_character_))

# ---- 2. rarity: species-level record counts -> the rare (< RARE_CUT) set ------
sp_counts <- rec %>% filter(!is.na(species_key)) %>% count(species_key, name = "n_records")
rare_keys <- sp_counts$species_key[sp_counts$n_records < RARE_CUT]
message(sprintf("Rare bees (< %d records): %d species", RARE_CUT, length(rare_keys)))

# ---- 3. FIGURE A: plant HUBS -- plants that feed the most different rare bees --
rare_vis <- rec %>% filter(species_key %in% rare_keys, has(plant_genus))
hub <- rare_vis %>% group_by(plant_genus) %>%
  summarise(rare_bee_species = n_distinct(species_key), visits = n(), .groups = "drop") %>%
  arrange(desc(rare_bee_species), desc(visits))
write.csv(hub, file.path(OUT_DIR, "rare_bee_plant_hubs.csv"), row.names = FALSE)
message(sprintf("  plant genera used by rare bees: %d (top: %s)",
                nrow(hub), paste(sprintf("%s[%dspp]", head(hub$plant_genus, 4), head(hub$rare_bee_species, 4)), collapse = " ")))

hub_fig <- hub %>% filter(rare_bee_species >= 2)               # the multi-rare-bee hubs
hub_fig$plant_genus <- factor(hub_fig$plant_genus, levels = rev(hub_fig$plant_genus))
gA <- ggplot(hub_fig, aes(x = rare_bee_species, y = plant_genus, fill = rare_bee_species)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = sprintf("%d spp  -  %d visits", rare_bee_species, visits)),
            hjust = -0.08, size = 3.0, colour = BEE_INK$secondary) +
  scale_fill_gradientn(colors = BEE_SEQ, guide = "none") +
  scale_x_continuous(breaks = scales::breaks_width(2), expand = expansion(mult = c(0, 0.32))) +
  labs(title = sprintf("Plant hubs for the park's rare bees (bees with < %d records each)", RARE_CUT),
       caption = str_wrap(paste0(scope_cap(sprintf("%d bee species with < %d records; records with a plant recorded",
                            length(rare_keys), RARE_CUT), "specimen net + iNat photo pooled", "plant genus"),
                            "  |  bar = number of different rare bee species that use each plant (hubs shown: used by 2+)"), 96),
       x = "number of different rare bee species", y = NULL) +
  theme_beescabr(11) +
  theme(legend.position = "none", panel.grid.major.y = element_blank())
ggsave(file.path(OUT_DIR, "rare_bee_plant_hubs.png"), gA, width = 9, height = 5.8, dpi = 200, bg = "white")

# ---- 4. FIGURE B: the two NAMED flagship bees' plants ------------------------
named    <- rec %>% inner_join(NAMED, by = "species_key")
tot      <- named %>% group_by(label) %>%
  summarise(n_records = n(), n_visits = sum(has(plant_genus)), .groups = "drop")
named_v  <- named %>% filter(has(plant_genus))
pg       <- named_v %>% count(label, plant_genus, name = "visits")
top_sp   <- named_v %>% filter(has(plant_species)) %>% count(label, plant_genus, plant_species, name = "n") %>%
  group_by(label, plant_genus) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>%
  transmute(label, plant_genus, top_plant_species = plant_species)
named_tbl <- pg %>% left_join(top_sp, by = c("label", "plant_genus")) %>%
  left_join(tot, by = "label") %>% arrange(label, desc(visits), plant_genus)
write.csv(named_tbl, file.path(OUT_DIR, "rare_named_bee_plants.csv"), row.names = FALSE)

panel_of <- setNames(sprintf("%s\n%d records  -  %d plant visits", tot$label, tot$n_records, tot$n_visits), tot$label)
plot_df  <- pg %>% mutate(panel = panel_of[label], row_key = paste(label, plant_genus, sep = "@@"))
lev <- plot_df %>% arrange(visits, plant_genus) %>% pull(row_key)
plot_df$row_key <- factor(plot_df$row_key, levels = unique(lev))
gB <- ggplot(plot_df, aes(x = visits, y = row_key, fill = visits)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = visits), hjust = -0.5, size = 3.1, colour = BEE_INK$secondary) +
  facet_wrap(~ panel, ncol = 1, scales = "free") +
  scale_y_discrete(labels = function(x) sub("^.*@@", "", x)) +
  scale_fill_gradientn(colors = BEE_SEQ, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Named priority bees: the plants they use",
       caption = str_wrap(paste0(scope_cap("Bombus crotchii & B. sonorus, records with a plant recorded",
                            "specimen net + iNat photo pooled", "plant genus"),
                            "  |  Crotch's = CA Endangered Species Act candidate; Sonoran = at-risk American bumble bee. Low n: where the few sightings concentrate"), 92),
       x = "plant visits (records with this plant recorded)", y = NULL) +
  theme_beescabr(11) +
  theme(legend.position = "none", panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 10, lineheight = 1.05))
ggsave(file.path(OUT_DIR, "rare_named_bee_plants.png"), gB, width = 8.5, height = 7.5, dpi = 200, bg = "white")

message("Wrote rare_bee_plant_hubs.{png,csv} + rare_named_bee_plants.{png,csv} to ", OUT_DIR)
