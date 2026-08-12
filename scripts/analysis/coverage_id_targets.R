# =============================================================
# Q7 -- Target-ID list: what needs identifying most?
# beescabr / Cabrillo National Monument (CABR) native bees
#
# THE QUESTION: many records stall above species level. Which taxa carry the most
# unresolved records, so ID effort can be pointed where it pays off -- and, crucially,
# which of those are SPECIMENS (keyable in hand) vs PHOTOS (often not resolvable)?
#
# WHAT COUNTS AS "needs ID": any record whose taxon_rank is coarser than species
# (genus, subgenus, complex, tribe, ... -- anything not species/subspecies).
# Grouped to the finest resolved label available (genus when known, else the coarse
# rank name), ranked by number of unresolved records, split by method.
#
# OUTPUTS (this figure family is split by paper):
#   * coverage_id_targets_report.png  -> REPORT: ALL records; the actionable worklist
#       view -- one dark "done" bar (both methods pooled) + keyable (lethal) + stuck
#       (non-lethal). The manager's "what's left to key" picture.
#   * coverage_id_targets_journal.png -> JOURNAL: FAIR WINDOW; the SAME bars but the
#       dark "done" bar is SPLIT by method (lethal done vs non-lethal done), so it
#       shows the different sets of already-resolved records each method contributes.
#   * coverage_id_targets_specimen.png / _photo.png -> JOURNAL only, FAIR WINDOW:
#       per-method ID progress by genus.
#   * coverage_id_completeness.png -> JOURNAL only, FAIR WINDOW: resolution funnel.
#   * coverage_id_targets.csv -> the actionable worklist (ALL records -- any specimen
#       can be keyed regardless of window).
#
# Descriptive counts -- no hypothesis test, so no p-value.
#
# Run from the repo root:  Rscript scripts/analysis/coverage_id_targets.R
# Depends on: dplyr, stringr, ggplot2 (+ config.R).
# =============================================================

for (pkg in c("ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_IDSTATUS")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_JOURNAL   <- file.path(DIR_JOURNAL, "coverage/id_resolution")   # fair-window: completeness, targets(journal), specimen, photo
OUT_REPORT    <- file.path(DIR_REPORT,  "coverage/id_resolution")   # all-records: targets(report) + the keyable worklist CSV
SPECIES_RANKS <- c("species", "subspecies")
dir.create(OUT_JOURNAL, recursive = TRUE, showWarnings = FALSE); dir.create(OUT_REPORT, recursive = TRUE, showWarnings = FALSE)
is_true   <- function(x) toupper(str_squish(as.character(x))) == "TRUE"
# scope_cap(): use the SHARED helper from theme_beescabr.R -- adds Source + data-as-of, one canonical order (no local override).
# method colours (net = rose-red / photo = periwinkle) + lighter tints for the "still to do" side
COL_L  <- unname(BEE_METHOD_COL["lethal"]);       COL_NL    <- unname(BEE_METHOD_COL["nonlethal"])
COL_L_LT <- unname(BEE_METHOD_COL_LT["lethal"]);  COL_NL_LT <- unname(BEE_METHOD_COL_LT["nonlethal"])   # lighter tints straight from the theme

## NOTE (#7 -- method scope is intentional): the REPORT view POOLS lethal + non-lethal;
## the JOURNAL views restrict to the fair window (survey-only, FAIR_MONTHS/FAIR_YEARS,
## attributed) so any lethal-vs-non-lethal contrast is on equal footing.
# ---- 1. pool records, keep the fields needed for scope + resolution ----------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
prep <- function(df, method) {
  st <- str_squish(tolower(as.character(df$surveyor_type))); st[is.na(st) | st == ""] <- "unattributed"
  data.frame(
    method    = method, taxon_rank = str_squish(tolower(df$taxon_rank)),
    genus     = str_squish(df$genus),
    is_survey = is_true(df$is_survey), surveyor = st,
    month = suppressWarnings(as.integer(substr(df$observed_on, 6, 7))),
    year  = suppressWarnings(as.integer(substr(df$observed_on, 1, 4))),
    stringsAsFactors = FALSE)
}
rec <- bind_rows(prep(spec, "specimen"), prep(inat, "photo")) %>%
  mutate(resolved = taxon_rank %in% SPECIES_RANKS,
         level = ifelse(resolved, "species",
                 ifelse(!is.na(genus) & genus != "", "genus", "coarser")),
         target = ifelse(!is.na(genus) & genus != "", genus, paste0("(", taxon_rank, ")")))

# fair window: survey-only, Mar-Oct 2021-2023, attributed (drops casual public + 2024 intern photos)
in_fair  <- rec$is_survey & rec$month %in% FAIR_MONTHS & rec$year %in% FAIR_YEARS & rec$surveyor != "unattributed"
rec_fair <- rec[in_fair, ]
message(sprintf("All records: %d | fair-window records: %d", nrow(rec), nrow(rec_fair)))

# ---- 2. worklist CSV (ALL records -- the actionable keyable list) ------------
resolved_by_genus_all <- rec %>% filter(resolved, !is.na(genus), genus != "") %>%
  count(genus, name = "resolved_species")
work <- rec %>% filter(!resolved) %>%
  group_by(target) %>%
  summarise(unresolved_total = n(),
            specimen_unresolved = sum(method == "specimen"),
            photo_unresolved    = sum(method == "photo"),
            ranks = paste(sort(unique(taxon_rank)), collapse = ", "),
            .groups = "drop") %>%
  left_join(resolved_by_genus_all, by = c("target" = "genus")) %>%
  mutate(resolved_species = coalesce(resolved_species, 0L)) %>%
  arrange(desc(specimen_unresolved), desc(unresolved_total))
write.csv(work, file.path(OUT_REPORT, "coverage_id_targets.csv"), row.names = FALSE)
keyable <- work %>% filter(specimen_unresolved > 0)
message(sprintf("Targets with specimen-backed (keyable) records: %d genera, %d specimens",
                nrow(keyable), sum(keyable$specimen_unresolved)))

# ---- 3. id-targets figure (report = pooled done; journal = done split by method) ----
draw_targets <- function(dat, scope, file, split_done) {
  n_total <- nrow(dat); n_resolved <- sum(dat$resolved); pct_resolved <- 100 * n_resolved / n_total
  # per-target counts
  un <- dat %>% filter(!resolved) %>% group_by(target) %>%
    summarise(specimen_unresolved = sum(method == "specimen"),
              photo_unresolved    = sum(method == "photo"),
              unresolved_total = n(), .groups = "drop") %>%
    filter(!grepl("^\\(", target))   # drop the coarse-rank buckets (epifamily..tribe): genus-level rows only
  rs_all <- dat %>% filter(resolved, !is.na(genus), genus != "") %>% count(genus, name = "resolved_species")
  rs_l   <- dat %>% filter(resolved, method == "specimen", !is.na(genus), genus != "") %>% count(genus, name = "resolved_lethal")
  rs_n   <- dat %>% filter(resolved, method == "photo",    !is.na(genus), genus != "") %>% count(genus, name = "resolved_nonlethal")
  tt <- un %>%
    left_join(rs_all, by = c("target" = "genus")) %>%
    left_join(rs_l,   by = c("target" = "genus")) %>%
    left_join(rs_n,   by = c("target" = "genus")) %>%
    mutate(across(c(resolved_species, resolved_lethal, resolved_nonlethal), ~ coalesce(.x, 0L))) %>%
    arrange(desc(unresolved_total))

  if (split_done) {
    long <- bind_rows(
      data.frame(target = tt$target, cat = "lethal done",       n = tt$resolved_lethal),
      data.frame(target = tt$target, cat = "non-lethal done",   n = tt$resolved_nonlethal),
      data.frame(target = tt$target, cat = "lethal keyable",    n = tt$specimen_unresolved),
      data.frame(target = tt$target, cat = "non-lethal stuck",  n = tt$photo_unresolved)) %>% filter(n > 0)
    lv   <- c("lethal done", "non-lethal done", "lethal keyable", "non-lethal stuck")
    fill <- c("lethal done" = COL_L, "non-lethal done" = COL_NL,
              "lethal keyable" = COL_L_LT, "non-lethal stuck" = COL_NL_LT)
    subttl <- "Dark = already identified to species (split by method); light = still unresolved. Fair window."
  } else {
    long <- bind_rows(
      data.frame(target = tt$target, cat = "resolved to species",       n = tt$resolved_species),
      data.frame(target = tt$target, cat = "lethal stuck at genus",      n = tt$specimen_unresolved),
      data.frame(target = tt$target, cat = "non-lethal stuck at genus",  n = tt$photo_unresolved)) %>% filter(n > 0)
    lv   <- c("resolved to species", "lethal stuck at genus", "non-lethal stuck at genus")
    fill <- c("resolved to species"        = BEE_METHOD_BOTH,   # both methods, DONE -> red+blue blend (purple)
              "lethal stuck at genus"       = COL_L_LT,          # unresolved lethal specimen -> LIGHT red (matches panels)
              "non-lethal stuck at genus"   = COL_NL_LT)         # unresolved non-lethal photo -> LIGHT blue (matches panels)
    subttl <- "Dark = already identified to species (both methods); light = still unresolved. All records."
  }
  long$cat <- factor(long$cat, levels = lv)
  # genera A->Z at top; parenthesised coarse-rank buckets sink to the bottom
  tg <- unique(as.character(long$target))
  long$target <- factor(long$target,
    levels = rev(c(sort(tg[!grepl("^\\(", tg)]), sort(tg[grepl("^\\(", tg)]))))
  lab <- tt %>% transmute(target, total = resolved_species + unresolved_total,
                          pct_id = ifelse(total > 0, 100 * resolved_species / total, 0)) %>% filter(total > 0)
  lab$target <- factor(lab$target, levels = levels(long$target))
  g <- ggplot(long, aes(x = n, y = target, fill = cat)) +
    geom_col(width = 0.72) +
    geom_text(data = lab, aes(x = total, y = target, label = sprintf("%.0f%% ID'd", pct_id)),
              hjust = -0.12, size = 2.8, color = "grey25", inherit.aes = FALSE) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    # bee genera italic; the coarse-rank buckets like "(tribe)" stay upright
    scale_y_discrete(labels = function(x) as.expression(lapply(x, function(s)
      if (grepl("^\\(", s)) s else bquote(italic(.(s)))))) +
    scale_fill_manual(values = fill, name = NULL) +
    labs(title = "Latest Progress of ID Resolution by Genus",
         subtitle = sprintf("%.0f%% of records are identified to species; the light bars are the genera with the most left to key.", pct_resolved),
         caption = scope_cap(scope, "lethal + non-lethal pooled", "genus / coarse rank"),
         x = "records", y = NULL) +
    theme_beescabr(11) +
    theme(legend.position = "top", panel.grid.major.y = element_blank(),
          plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, size = 9))
  bee_ggsave(file, g, width = 9.5, height = max(7, 0.30 * nrow(tt) + 2), bg = "white")
}
draw_targets(rec,      "all records",                                  file.path(OUT_REPORT, "coverage_id_targets_report.png"),  split_done = FALSE)
draw_targets(rec_fair, "fair window (survey-only, Mar-Oct 2021-2023)", file.path(OUT_JOURNAL, "coverage_id_targets_journal.png"), split_done = TRUE)

# ---- 4. per-method species-ID progress (JOURNAL only, FAIR WINDOW) -----------
# Shared genus set: EVERY named genus in the fair window (no top-N cap), ALPHABETICAL,
# so the specimen and photo panels line up genus-for-genus.
method_genera <- rec_fair %>% filter(!is.na(genus), genus != "") %>% distinct(genus) %>% pull(genus) %>% sort()
method_genus_fig <- function(m, file, method_label) {
  d <- rec_fair %>% filter(method == m, genus %in% method_genera) %>%
    group_by(genus) %>%
    summarise(species = sum(resolved), genus_only = sum(!resolved), total = n(), .groups = "drop")
  d <- data.frame(genus = method_genera, stringsAsFactors = FALSE) %>%
    left_join(d, by = "genus") %>%
    mutate(species = coalesce(species, 0L), genus_only = coalesce(genus_only, 0L), total = coalesce(total, 0L))
  long <- bind_rows(
    data.frame(genus = d$genus, cat = "identified to species",   n = d$species),
    data.frame(genus = d$genus, cat = "genus-only (unresolved)", n = d$genus_only)) %>% filter(n > 0)
  long$cat   <- factor(long$cat, levels = c("identified to species", "genus-only (unresolved)"))
  long$genus <- factor(long$genus, levels = rev(method_genera))
  pct <- 100 * sum(d$species) / max(1, sum(d$total))
  lab2 <- d %>% filter(total > 0) %>% transmute(genus, total, pct_id = 100 * species / total)
  lab2$genus <- factor(lab2$genus, levels = rev(method_genera))
  g2 <- ggplot(long, aes(x = n, y = genus, fill = cat)) +
    geom_col(width = 0.72) +
    geom_text(data = lab2, aes(x = total, y = genus, label = sprintf("%.0f%%", pct_id)),
              hjust = -0.15, size = 2.7, color = "grey25", inherit.aes = FALSE) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    scale_y_discrete(drop = FALSE) +
    scale_fill_manual(values = if (m == "specimen")   # colour by METHOD: lethal = red, non-lethal = blue; light tint = unresolved
        c("identified to species" = COL_L,  "genus-only (unresolved)" = COL_L_LT)
      else
        c("identified to species" = COL_NL, "genus-only (unresolved)" = COL_NL_LT), name = NULL) +
    labs(title = sprintf("%s Method: Progress of ID by Genus", method_label),
         subtitle = sprintf("%s: %.0f%% of records reach species; the rest are ID targets, by genus.", method_label, pct),
         caption = scope_cap("fair window (survey-only, Mar-Oct 2021-2023)", tolower(method_label), "genus"),
         x = "records", y = NULL) +
    theme_beescabr(11) +
    theme(legend.position = "top", panel.grid.major.y = element_blank(), plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(face = "italic"))   # bee genus names = scientific -> italic
  bee_ggsave(file, g2, width = 9.5, height = max(7, 0.30 * length(method_genera) + 2), bg = "white")
}
method_genus_fig("specimen", file.path(OUT_JOURNAL, "coverage_id_targets_specimen.png"), "Lethal")
method_genus_fig("photo",    file.path(OUT_JOURNAL, "coverage_id_targets_photo.png"),    "Non-Lethal")

# ---- 5. ID-completeness funnel (JOURNAL only, FAIR WINDOW) --------------------
n_total_f <- nrow(rec_fair); n_res_f <- sum(rec_fair$resolved); pct_res_f <- 100 * n_res_f / n_total_f
grid   <- expand.grid(method = c("specimen", "photo"),
                      level  = c("species", "genus", "coarser"), stringsAsFactors = FALSE)
funnel <- merge(grid, dplyr::count(rec_fair, method, level, name = "n"), all.x = TRUE)
funnel$n[is.na(funnel$n)] <- 0L
funnel$level  <- factor(funnel$level,  levels = c("species", "genus", "coarser"))
funnel$method <- factor(funnel$method, levels = c("specimen", "photo"))
gf <- ggplot(funnel, aes(x = level, y = n, fill = method)) +
  geom_col(position = position_dodge(0.72), width = 0.68) +
  geom_text(aes(label = format(n, big.mark = ",")), position = position_dodge(0.72), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(specimen = COL_L, photo = COL_NL), name = NULL,
                    labels = c(specimen = "lethal", photo = "non-lethal")) +
  scale_x_discrete(labels = c(species = "to species", genus = "genus-only", coarser = "coarser than genus")) +
  labs(title = "Counts of ID Resolution between Methods",
       subtitle = sprintf("Specimens key to species; photos often stall at genus -- %.0f%% of records reach species overall.", pct_res_f),
       caption = scope_cap("fair window (survey-only, Mar-Oct 2021-2023)", "lethal vs non-lethal", "resolution level"),
       x = NULL, y = "records") +
  theme_beescabr(11) +
  theme(legend.position = "top", panel.grid.major.x = element_blank(), plot.title = element_text(hjust = 0.5))
bee_ggsave(file.path(OUT_JOURNAL, "coverage_id_completeness.png"), gf, width = 8.5, height = 5.6, bg = "white")

message("Wrote coverage_id_targets.csv + _report.png/_journal.png + _specimen.png/_photo.png + coverage_id_completeness.png to journal_paper_2026/ + nps_report_2026/")
