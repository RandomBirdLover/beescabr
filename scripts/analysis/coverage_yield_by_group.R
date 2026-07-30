# =============================================================
# Q11 -- Yield by group: who finds what?
# beescabr / Cabrillo National Monument (CABR) native bees
#
# THE QUESTION: how much does each surveyor group x method contribute, and what
# does each group uniquely add? Groups present in the data:
#     * intern  x lethal      (specimens -- every specimen is an intern collection)
#     * intern  x non-lethal  (iNaturalist photos logged by interns)
#     * beeple  x non-lethal  (iNaturalist photos by the beeple community group)
#     * unattributed x non-lethal (on-transect iNat records with no surveyor tag)
#   (there is no beeple-lethal: only interns collect specimens.)
#
# METRICS per group, reported for TWO scopes side by side:
#     * n_records                -- raw effort/output
#     * species / genera          -- distinct taxa recorded
#     * species_per_100_records   -- efficiency (unique species yield per effort)
#     * exclusive_species/genera  -- taxa found ONLY by that group in that scope
#                                    (what would be missed without them)
#
# SCOPE is stated on every output:
#     * survey-only (is_survey = TRUE)  -- fair comparison of standardized effort
#     * all-records                     -- full credit incl. casual/off-transect iNat
#   Descriptive counts -- no hypothesis test, so no p-value (method's effect on
#   ID resolution is the tested question and lives in coverage_method_venn.R / Q2).
#
# Run from the repo root:  Rscript scripts/analysis/coverage_yield_by_group.R
# Depends on: dplyr, stringr, ggplot2 (+ config.R).
# =============================================================

for (pkg in c("ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_SCOPE")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_DIR       <- "data/analysis/coverage"
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"
scope_cap <- function(scope, method, rank) sprintf("Scope: %s  |  Method: %s  |  Rank: %s",
                                                   scope, method, rank)

# ---- 1. pool records with group + method + taxonomy keys --------------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

prep <- function(df, method) {
  st <- str_squish(tolower(as.character(df$surveyor_type)))
  st[is.na(st) | st == ""] <- "unattributed"
  data.frame(
    method     = method,
    surveyor   = st,
    is_survey  = is_true(df$is_survey),
    taxon_rank = df$taxon_rank, genus = df$genus, species = df$species,
    stringsAsFactors = FALSE) %>%
    mutate(
      group = paste(surveyor, method),
      species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                             !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
      genus_key   = ifelse(taxon_rank %in% GENUS_RANKS & !is.na(genus) & genus != "", genus, NA))
}
rec <- bind_rows(prep(spec, "lethal"), prep(inat, "non-lethal"))

# ---- 2. yield metrics for one scope -----------------------------------------
yield_for_scope <- function(d, scope_label) {
  # exclusive taxa: recorded by exactly one group within this scope
  excl_sp <- d %>% filter(!is.na(species_key)) %>% distinct(group, species_key) %>%
    count(species_key) %>% filter(n == 1) %>% pull(species_key)
  excl_gn <- d %>% filter(!is.na(genus_key)) %>% distinct(group, genus_key) %>%
    count(genus_key) %>% filter(n == 1) %>% pull(genus_key)

  d %>% group_by(group) %>%
    summarise(
      n_records        = n(),
      species          = n_distinct(species_key[!is.na(species_key)]),
      genera           = n_distinct(genus_key[!is.na(genus_key)]),
      exclusive_species = n_distinct(species_key[!is.na(species_key) & species_key %in% excl_sp]),
      exclusive_genera  = n_distinct(genus_key[!is.na(genus_key) & genus_key %in% excl_gn]),
      .groups = "drop") %>%
    mutate(species_per_100_records = round(100 * species / n_records, 1),
           scope = scope_label) %>%
    arrange(desc(n_records))
}
all_tbl <- yield_for_scope(rec, "all records")
srv_tbl <- yield_for_scope(filter(rec, is_survey), "survey-only")

out <- bind_rows(srv_tbl, all_tbl) %>%
  select(scope, group, n_records, species, genera,
         species_per_100_records, exclusive_species, exclusive_genera)
write.csv(out, file.path(OUT_DIR, "coverage_yield_by_group.csv"), row.names = FALSE)
message("Q11 group-yield table:"); print(as.data.frame(out), row.names = FALSE)

# ---- 3. exclusive-taxa lists (what each group alone contributes) -------------
excl_list <- function(d, scope_label) {
  sp <- d %>% filter(!is.na(species_key)) %>% distinct(group, species_key) %>%
    count(species_key) %>% filter(n == 1) %>% pull(species_key)
  d %>% filter(!is.na(species_key), species_key %in% sp) %>%
    distinct(group, species_key) %>% mutate(scope = scope_label) %>%
    arrange(group, species_key)
}
excl_out <- bind_rows(excl_list(filter(rec, is_survey), "survey-only"),
                      excl_list(rec, "all records"))
write.csv(excl_out, file.path(OUT_DIR, "coverage_yield_by_group_exclusive_species.csv"), row.names = FALSE)

# ---- 4. figure: 4 metric panels, group on x, scope as dodged fill ------------
metrics <- c(n_records = "Records", species = "Species recorded",
             species_per_100_records = "Species / 100 records",
             exclusive_species = "Group-exclusive species")
long <- do.call(rbind, lapply(names(metrics), function(m)
  data.frame(scope = out$scope, group = out$group, metric = metrics[[m]],
             value = out[[m]], stringsAsFactors = FALSE)))
long$metric <- factor(long$metric, levels = unname(metrics))
long$group  <- factor(long$group, levels = out$group[out$scope == "all records"])

g <- ggplot(long, aes(x = group, y = value, fill = scope)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_fill_manual(values = BEE_SCOPE, name = "scope") +   # house scope: survey-only accent vs all-records grey
  labs(title = "Q11 - Yield by surveyor group x method (CABR bees)",
       subtitle = scope_cap("survey-only vs all records (both shown)",
                            "lethal (specimens) + non-lethal (iNaturalist)", "species"),
       x = NULL, y = NULL) +
  theme_beescabr(11) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        panel.grid.major.x = element_blank())
ggsave(file.path(OUT_DIR, "coverage_yield_by_group.png"), g, width = 10, height = 6.5, dpi = 200, bg = "white")
message("Wrote coverage_yield_by_group.{csv,png} + coverage_yield_by_group_exclusive_species.csv to ", OUT_DIR)
