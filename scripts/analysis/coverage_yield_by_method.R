# =============================================================
# Q11 -- Yield by METHOD: which method finds the most, on a FAIR footing?
# beescabr / Cabrillo National Monument (CABR) native bees
#
# Restricted to a fair comparison window so we measure the METHOD, not when/how it ran:
#   SCOPE  = survey records only (is_survey = TRUE)
#   SEASON = March-October (the months lethal netting ran)
#   YEARS  = 2021-2023 (the only years lethal netting ran; non-lethal photography continued
#            2024-2026 with no lethal counterpart, so those years are clipped out).
# Untagged on-transect iNat records (no surveyor) are dropped, so non-lethal = attributed
# survey photos. NOTE: in this fair window every intern record is a specimen (lethal) and
# every beeple record is a photo (non-lethal), so "yield by method" and "yield by surveyor
# group" would be the SAME two bars -- the group figure was dropped as redundant.
#
# TWO figures (species + genus rank), 4 panels each: n_records (effort), taxa recorded,
#   taxa_per_100_records (efficiency), method-exclusive taxa (found ONLY by that method).
#   * SPECIES-level  = detection + ID resolution (a record must be keyed to species)
#   * GENUS-level    = detection alone (photos not penalised for stalling at genus)
# Descriptive counts -- no hypothesis test (the coverage-standardized richness comparison
# lives in rarefaction_inext.R / rarefaction_vegan.R; method x ID resolution in Q2).
#
# Run from the repo root:  Rscript scripts/analysis/coverage_yield_by_method.R
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
OUT_DIR       <- "data/analysis/method_comparison/yield"
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
WINDOW_MONTHS <- 3:10                                   # Mar-Oct: the lethal-netting season
WINDOW_YEARS  <- 2021:2023                              # the only years lethal netting ran (fair vs non-lethal)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"

# ---- 1. pool records with method + taxonomy keys + month/year ---------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

prep <- function(df, method) {
  st <- str_squish(tolower(as.character(df$surveyor_type)))
  st[is.na(st) | st == ""] <- "unattributed"
  data.frame(
    method     = method,
    surveyor   = st,
    is_survey  = is_true(df$is_survey),
    month      = suppressWarnings(as.integer(substr(df$observed_on, 6, 7))),
    year       = suppressWarnings(as.integer(substr(df$observed_on, 1, 4))),
    taxon_rank = df$taxon_rank, genus = df$genus, species = df$species,
    stringsAsFactors = FALSE) %>%
    mutate(
      species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                             !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
      genus_key   = ifelse(taxon_rank %in% GENUS_RANKS & !is.na(genus) & genus != "", genus, NA))
}
# FAIR window: survey records only, Mar-Oct, 2021-2023, untagged non-lethal dropped.
rec <- bind_rows(prep(spec, "lethal"), prep(inat, "nonlethal")) %>%
  filter(is_survey, !is.na(month), month %in% WINDOW_MONTHS,
         !is.na(year), year %in% WINDOW_YEARS, surveyor != "unattributed")
message(sprintf("Survey records in the fair window (Mar-Oct, 2021-2023): %d (lethal %d, non-lethal %d)",
                nrow(rec), sum(rec$method == "lethal"), sum(rec$method == "nonlethal")))

# ---- 2. yield metrics (both species- and genus-level) -----------------------
# species_key needs a species/subspecies ID; genus_key counts anything that pins a genus
# (species/subgenus/complex/genus), so the genus metrics DON'T penalise photos ID'd only to
# genus -- a cleaner read on DETECTION, separate from ID resolution.
yield_tbl <- function(d) {
  excl_sp <- d %>% filter(!is.na(species_key)) %>% distinct(method, species_key) %>%
    count(species_key) %>% filter(n == 1) %>% pull(species_key)
  excl_gn <- d %>% filter(!is.na(genus_key)) %>% distinct(method, genus_key) %>%
    count(genus_key) %>% filter(n == 1) %>% pull(genus_key)
  d %>% group_by(method) %>%
    summarise(
      n_records         = n(),
      species           = n_distinct(species_key[!is.na(species_key)]),
      genera            = n_distinct(genus_key[!is.na(genus_key)]),
      exclusive_species = n_distinct(species_key[!is.na(species_key) & species_key %in% excl_sp]),
      exclusive_genera  = n_distinct(genus_key[!is.na(genus_key) & genus_key %in% excl_gn]),
      .groups = "drop") %>%
    mutate(species_per_100_records = round(100 * species / n_records, 1),
           genera_per_100_records  = round(100 * genera  / n_records, 1))
}
tbl <- yield_tbl(rec)
tbl <- tbl[match(c("lethal", "nonlethal"), tbl$method), ]; tbl <- tbl[!is.na(tbl$method), ]
write.csv(tbl, file.path(OUT_DIR, "coverage_yield_by_method.csv"), row.names = FALSE)
message("\nYield by METHOD (survey-only, Mar-Oct 2021-2023):"); print(as.data.frame(tbl), row.names = FALSE)

# ---- 3. one 4-panel yield figure (rank = "species" or "genus") --------------

message("\nWrote coverage_yield_by_method.csv (table only -- yield is now shown in the method Venn) to ", OUT_DIR)
