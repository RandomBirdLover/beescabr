# =============================================================
# analysis/method_comparison/coverage_yield_by_method.R
# Q11 -- Yield of RECORDS: what each method / contributor turned up. SPLIT figure.
# beescabr / Cabrillo National Monument (CABR) native bees
#
# SPLIT -- feeds BOTH papers with different scopes:
#
#   * JOURNAL (method comparison) -> the FAIR WINDOW: survey records only,
#     Mar-Oct, 2021-2023, attributed (untagged/casual dropped; interns' 2024 photos
#     excluded). Two methods on equal footing: lethal (net) vs non-lethal (photo).
#     Written as a backing TABLE (_journal.csv) -- the journal *figure* for yield is
#     the method Venn (coverage_method_venn.R). Yield = records + taxa + group-exclusive;
#     the per-100 rate (and the FAIR rarefied per-100) is the efficiency figure's job
#     (coverage_efficiency_by_method.R), NOT here.
#
#   * REPORT (what the park holds) -> ALL RECORDS, no window, everyone included.
#     ONE figure with two stacked views of the SAME pile of records:
#       - by CONTRIBUTOR: general public | beeple | interns
#           (interns = all their records: lethal specimens + their 2024 photos)
#       - by METHOD:      lethal | non-lethal   (every record collapsed to method)
#     Because interns' bar holds all their records, the two views reconcile to the
#     same grand total. Three metrics per view: records, taxa recorded, group-exclusive.
#     Run at BOTH species and genus rank.
#
# Run from the repo root:  Rscript scripts/analysis/method_comparison/coverage_yield_by_method.R
# Depends on: dplyr, stringr, ggplot2 (+ config.R, theme_beescabr.R).
# =============================================================

# Dependencies are CHECKED here, not installed: see beescabr_require() in config.R.
if (!exists("beescabr_require")) source("scripts/config.R")
beescabr_require()
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/shared/theme_beescabr.R")   # shared house style
OUT_JOURNAL   <- file.path(DIR_JOURNAL, "method_comparison/yield/fair_method_2021_2023")   # fair-window backing table
OUT_REPORT    <- file.path(DIR_REPORT,  "method_comparison/yield")   # all-records contributor/method figures
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
dir.create(OUT_JOURNAL, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_REPORT,  recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"

# ---- 1. pool records with method + surveyor + taxonomy keys + month/year -----
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
rec_all <- bind_rows(prep(spec, "lethal"), prep(inat, "nonlethal"))

# ---- 2. yield-by-group engine (grouping column is a parameter) --------------
# group-exclusive = a taxon recorded by ONLY that group within the chosen grouping.
yield_by <- function(d, gcol) {
  d <- d %>% mutate(grp = .data[[gcol]])
  excl_sp <- d %>% filter(!is.na(species_key)) %>% distinct(grp, species_key) %>%
    count(species_key) %>% filter(n == 1) %>% pull(species_key)
  excl_gn <- d %>% filter(!is.na(genus_key)) %>% distinct(grp, genus_key) %>%
    count(genus_key) %>% filter(n == 1) %>% pull(genus_key)
  # NOTE: no per-100 rate here -- taxa/100 (and the fair RAREFIED per-100) is the
  # efficiency figure's job (coverage_efficiency_by_method.R); duplicating a raw
  # rate in the yield table just weakens that story. Yield = records + taxa + exclusive.
  d %>% group_by(grp) %>%
    summarise(
      n_records         = n(),
      species           = n_distinct(species_key[!is.na(species_key)]),
      genera            = n_distinct(genus_key[!is.na(genus_key)]),
      exclusive_species = n_distinct(species_key[!is.na(species_key) & species_key %in% excl_sp]),
      exclusive_genera  = n_distinct(genus_key[!is.na(genus_key) & genus_key %in% excl_gn]),
      .groups = "drop")
}

# =============================================================================
# JOURNAL -- fair-window method table (backs the journal Venn / efficiency figs)
# =============================================================================
rec_fair <- rec_all %>%
  filter(is_survey, !is.na(month), month %in% FAIR_MONTHS,
         !is.na(year), year %in% FAIR_YEARS, surveyor != "unattributed")
tbl_j <- yield_by(rec_fair, "method")
tbl_j <- tbl_j[match(c("lethal", "nonlethal"), tbl_j$grp), ]; tbl_j <- tbl_j[!is.na(tbl_j$grp), ]
names(tbl_j)[1] <- "method"
write.csv(tbl_j, file.path(OUT_JOURNAL, "coverage_yield_by_method_journal.csv"), row.names = FALSE)
message(sprintf("JOURNAL (fair window): lethal %d recs / %d sp / %d gen ; non-lethal %d recs / %d sp / %d gen",
                tbl_j$n_records[1], tbl_j$species[1], tbl_j$genera[1],
                tbl_j$n_records[2], tbl_j$species[2], tbl_j$genera[2]))

# =============================================================================
# REPORT -- ALL records, two reconciling views (by contributor + by method)
# =============================================================================
rep_rec <- rec_all %>%
  mutate(
    contributor = case_when(
      method == "lethal"        ~ "interns",                # all specimens are intern-collected
      surveyor == "beeple"      ~ "beeple",
      surveyor == "intern"      ~ "interns",                # interns' 2024 iNaturalist photos
      TRUE                      ~ "general public"),        # blank/unattributed casual photos
    method_lbl = ifelse(method == "lethal", "lethal", "non-lethal"))

CONTRIB <- c("general public", "beeple", "interns")
METHODL <- c("lethal", "non-lethal")
tbl_c <- yield_by(rep_rec, "contributor"); tbl_c <- tbl_c[match(CONTRIB, tbl_c$grp), ]
tbl_m <- yield_by(rep_rec, "method_lbl");  tbl_m <- tbl_m[match(METHODL, tbl_m$grp), ]
write.csv(tbl_c, file.path(OUT_REPORT, "bee_yield_by_contributor.csv"), row.names = FALSE)
write.csv(tbl_m, file.path(OUT_REPORT, "bee_yield_by_method.csv"),      row.names = FALSE)
message(sprintf("REPORT by contributor: %s",
                paste(sprintf("%s=%d recs", tbl_c$grp, tbl_c$n_records), collapse = "  ")))
message(sprintf("REPORT by method:      %s   (grand totals reconcile: contrib=%d, method=%d)",
                paste(sprintf("%s=%d recs", tbl_m$grp, tbl_m$n_records), collapse = "  "),
                sum(tbl_c$n_records), sum(tbl_m$n_records)))

# palette: purple = net/lethal/interns, vermillion = photo/non-lethal/beeple, gray = public
GRP_COL <- c(
  "general public" = unname(BEE_INK$muted),
  "beeple"         = unname(BEE_METHOD_COL["nonlethal"]),
  "interns"        = unname(BEE_METHOD_COL["lethal"]),
  "lethal"         = unname(BEE_METHOD_COL["lethal"]),
  "non-lethal"     = unname(BEE_METHOD_COL["nonlethal"]))
GRP_LEVELS <- c(CONTRIB, METHODL)

# melt one yield table (chosen rank) into (section, group, metric, value) for 3 metrics
report_long <- function(tbl, section, rank) {
  m <- if (rank == "species")
    c(n_records = "Records", species = "Species Recorded", exclusive_species = "Group-Exclusive Species")
  else
    c(n_records = "Records", genera = "Genera Recorded", exclusive_genera = "Group-Exclusive Genera")
  do.call(rbind, lapply(names(m), function(k)
    data.frame(section = section, group = tbl$grp, metric = m[[k]],
               value = tbl[[k]], stringsAsFactors = FALSE)))
}

plot_report <- function(rank, file) {
  metrics <- if (rank == "species")
    c("Records", "Species Recorded", "Group-Exclusive Species")
  else
    c("Records", "Genera Recorded", "Group-Exclusive Genera")
  long <- rbind(report_long(tbl_c, "By Contributor", rank),
                report_long(tbl_m, "By Method",      rank))
  long$group <- factor(long$group, levels = GRP_LEVELS)
  long$panel <- factor(paste0(long$section, ": ", long$metric),
                       levels = c(paste0("By Contributor: ", metrics),
                                  paste0("By Method: ",      metrics)))
  ttl <- sprintf("Did any one method or contributor find every %s?", rank)
  g <- ggplot(long, aes(x = group, y = value, fill = group)) +
    ggpattern::geom_col_pattern(width = 0.66,   # house rule: genus figure hatched, species solid
      pattern = if (rank == "genus") "stripe" else "none", pattern_fill = "white", pattern_colour = NA,
      pattern_angle = 45, pattern_density = 0.08, pattern_spacing = 0.03, pattern_key_scale_factor = 0.4) +
    geom_text(aes(label = value), vjust = -0.35, size = 2.7, colour = BEE_INK$secondary) +
    facet_wrap(~ panel, scales = "free", ncol = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
    scale_fill_manual(values = GRP_COL, guide = "none") +
    labs(title = ttl,
         subtitle = "Interns, beeple, and the public each turn up taxa the others miss -- no single group or method sees it all.",
         caption = paste0(
           scope_cap(scope  = "all records, no window; every specimen + every iNaturalist photo (survey or not)",
                     method = "lethal vs non-lethal",
                     rank   = rank),
           "\n",
           str_wrap(paste0(
             "Interns' bar = their specimens + 2024 photos, so the contributor and method views reconcile to the ",
             "same total. Group-exclusive = a taxon only that group recorded."), 108)),
         x = "contributor group or method", y = NULL) +
    theme_beescabr(11) +
    theme(axis.text.x = element_text(size = 8.5),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(size = 8.7, hjust = 0.5))
  bee_ggsave(file, g, width = 9.5, height = 6.4, bg = "white")
}
# ---- BOTH RANKS ON ONE FIGURE ----------------------------------------------
# The per-rank versions printed the Records column TWICE: records by contributor
# and by method do not depend on rank, so those four bars were identical in both
# files. Here Records appears once and the two rank-dependent columns carry both
# ranks -- species solid, genera hatched, the house rule.
plot_report_both <- function(file) {
  L <- rbind(transform(rbind(report_long(tbl_c, "By Contributor", "species"),
                             report_long(tbl_m, "By Method",      "species")), rank = "Species"),
             transform(rbind(report_long(tbl_c, "By Contributor", "genus"),
                             report_long(tbl_m, "By Method",      "genus")),   rank = "Genus"))
  L$group <- factor(L$group, levels = GRP_LEVELS)
  L$rank  <- factor(L$rank,  levels = c("Species", "Genus"))
  # Records is rank-independent: keep one copy, and label the other columns generically
  L$metric2 <- ifelse(grepl("^Records$", L$metric), "Records",
                ifelse(grepl("Group-Exclusive", L$metric), "Taxa only that group found", "Taxa recorded"))
  L <- L[!(L$metric2 == "Records" & L$rank == "Genus"), , drop = FALSE]
  L$panel <- factor(paste0(L$section, ": ", L$metric2),
                    levels = c(paste0("By Contributor: ", c("Records", "Taxa recorded", "Taxa only that group found")),
                               paste0("By Method: ",      c("Records", "Taxa recorded", "Taxa only that group found"))))
  g <- ggplot(L, aes(x = group, y = value, fill = group)) +
    ggpattern::geom_col_pattern(aes(pattern = rank), position = position_dodge(0.78), width = 0.7,
      pattern_fill = "white", pattern_colour = NA, pattern_angle = 45,
      pattern_density = 0.08, pattern_spacing = 0.03, pattern_key_scale_factor = 0.4) +
    ggpattern::scale_pattern_manual(values = c(Species = "none", Genus = "stripe"), name = NULL) +
    geom_text(aes(label = value, group = rank), position = position_dodge(0.78),
              vjust = -0.35, size = 2.5, colour = BEE_INK$secondary) +
    facet_wrap(~ panel, scales = "free", ncol = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
    scale_fill_manual(values = GRP_COL, guide = "none") +
    labs(title = "Did any one method or contributor find every bee?",
         subtitle = "Interns, beeple, and the public each turn up taxa the others miss. No single group or method sees it all.\nSpecies solid, genera hatched. Records are the same at either rank, so they are shown once.",
         caption = paste0(
           scope_cap(scope  = "all records, no window; every specimen + every iNaturalist photo (survey or not)",
                     method = "lethal vs non-lethal", rank = "species + genus"),
           "\n",
           str_wrap(paste0(
             "Interns' bar = their specimens + 2024 photos, so the contributor and method views reconcile to the ",
             "same total. Group-exclusive = a taxon only that group recorded."), 108)),
         x = "contributor group or method", y = NULL) +
    theme_beescabr(11) +
    theme(axis.text.x = element_text(size = 8.5), panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0.5), legend.position = "top",
          plot.subtitle = element_text(size = 8.7, hjust = 0.5))
  bee_ggsave(file, g, width = 10.5, height = 6.8, bg = "white")
}
plot_report_both(file.path(OUT_REPORT, "bee_yield_by_contributor_and_method.png"))

message("\nWrote JOURNAL table (_journal.csv) + REPORT figure (bee_yield_by_contributor_and_method.png, both ranks) + report CSVs to journal_paper_2026/ + nps_report_2026/")
