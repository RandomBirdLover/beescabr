# =============================================================
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
# Run from the repo root:  Rscript scripts/analysis/coverage_offtransect.R
# Depends on: dplyr, stringr, ggplot2 (+ config.R).
# =============================================================

for (pkg in c("ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_SET")) source("scripts/analysis/theme_beescabr.R")   # shared house style
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
write.csv(summ, file.path(OUT_DIR, "coverage_offtransect_summary.csv"), row.names = FALSE)

# full taxon table + the actionable off-only list
taxa_tbl <- bind_rows(sp_part, gn_part) %>% arrange(rank, region, key) %>%
  select(rank, taxon = key, region, on_transect = on, off_transect = off)
write.csv(taxa_tbl, file.path(OUT_DIR, "coverage_offtransect_taxa.csv"), row.names = FALSE)
offonly_sp <- taxa_tbl %>% filter(rank == "species", region == "off-only") %>% pull(taxon)

message("Species: ",
        paste(sprintf("%s=%d", summ$region[summ$rank=="species"], summ$n[summ$rank=="species"]), collapse="  "))
message("Genera:  ",
        paste(sprintf("%s=%d", summ$region[summ$rank=="genus"], summ$n[summ$rank=="genus"]), collapse="  "))
message("Off-only species (surveys miss these): ", length(offonly_sp))

# ---- 3. figure: on-only / shared / off-only, species + genus -----------------
# set-overlap colours: on-only = ink (focal set), off-only = sienna (the other set),
# and "both" = the two MIXED -- the Lab (perceptual) midpoint of the on-only + off-only
# hues (#3C3B36 + #B0632B -> #764F32), so the shared bar literally looks like the two blended.
BOTH_BLEND <- "#764F32"
plot_df <- summ %>%
  mutate(region = factor(region, levels = c("on-only", "both", "off-only")),
         rank = factor(rank, levels = c("species", "genus")))
g <- ggplot(plot_df, aes(x = rank, y = n, fill = region)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = n), position = position_dodge(0.8), vjust = -0.3, size = 3.4) +
  scale_fill_manual(values = c("on-only" = unname(BEE_SET["a_only"]),
                               "both"     = BOTH_BLEND,
                               "off-only" = unname(BEE_SET["b_only"])), name = NULL) +
  labs(title = "On-Transect vs. Off-Transect Bee Coverage",
       caption = scope_cap(scope = "all records; on-transect (standardized survey effort) vs off-transect (casual park-wide records)",
                           method = "lethal + non-lethal pooled", rank = "species + genus"),
       x = NULL, y = "distinct taxa") +
  theme_beescabr(11) +
  theme(panel.grid.major.x = element_blank(), plot.title = element_text(hjust = 0.5))
ggsave(file.path(OUT_DIR, "coverage_offtransect.png"), g, width = 8, height = 5.5, dpi = 200, bg = "white")
message("Wrote coverage_offtransect.{png,_summary.csv,_taxa.csv} to ", OUT_DIR)
