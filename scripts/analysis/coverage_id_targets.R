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
# The actionable worklist is the SPECIMEN column: physical specimens stuck at genus
# can be keyed to species now. Photo-only stalls flag detection limits, not tasks.
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
OUT_DIR       <- "data/analysis/coverage"
SPECIES_RANKS <- c("species", "subspecies")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
# scope_cap() now provided by theme_beescabr.R (adds n / sig / source + data date)

## NOTE (#7 -- method scope is intentional; do NOT "fix" it down to one method):
## This POOLS lethal (specimen) + non-lethal (photo) records on purpose -- question #7 asks what
## needs ID in "lethal AND non-lethal specimens." That it spans BOTH methods (while #12 plant
## phenology is non-lethal only) is NOT an inconsistency: each analysis matches its own question's
## wording. Keep both methods here.
# ---- 1. pool records, keep only those NOT resolved to species ----------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
prep <- function(df, method) data.frame(
  method = method, taxon_rank = str_squish(tolower(df$taxon_rank)),
  genus = str_squish(df$genus), stringsAsFactors = FALSE)
rec <- bind_rows(prep(spec, "specimen"), prep(inat, "photo")) %>%
  mutate(resolved = taxon_rank %in% SPECIES_RANKS,
         # how far each record got: species / genus-only / coarser-than-genus
         level = ifelse(resolved, "species",
                 ifelse(!is.na(genus) & genus != "", "genus", "coarser")),
         # finest label we can name the unresolved cluster by: genus if known, else the rank
         target = ifelse(!is.na(genus) & genus != "", genus, paste0("(", taxon_rank, ")")))
unresolved <- rec %>% filter(!resolved)
message(sprintf("Unresolved records (coarser than species): %d of %d (%.1f%%)",
                nrow(unresolved), nrow(rec), 100 * nrow(unresolved) / nrow(rec)))

# resolved (identified to species) -- the "work already done" side
resolved_by_genus <- rec %>% filter(resolved, !is.na(genus), genus != "") %>%
  count(genus, name = "resolved_species")
n_total <- nrow(rec); n_resolved <- sum(rec$resolved); pct_resolved <- 100 * n_resolved / n_total
message(sprintf("Resolved to species: %d of %d records (%.1f%%)", n_resolved, n_total, pct_resolved))

# ---- 2. worklist: unresolved records per target, split by method -------------
work <- unresolved %>%
  group_by(target) %>%
  summarise(unresolved_total = n(),
            specimen_unresolved = sum(method == "specimen"),
            photo_unresolved    = sum(method == "photo"),
            ranks = paste(sort(unique(taxon_rank)), collapse = ", "),
            .groups = "drop") %>%
  left_join(resolved_by_genus, by = c("target" = "genus")) %>%
  mutate(resolved_species = coalesce(resolved_species, 0L)) %>%
  arrange(desc(specimen_unresolved), desc(unresolved_total))
write.csv(work, file.path(OUT_DIR, "coverage_id_targets.csv"), row.names = FALSE)

keyable <- work %>% filter(specimen_unresolved > 0)
message(sprintf("Targets with specimen-backed (keyable) records: %d genera, %d specimens",
                nrow(keyable), sum(keyable$specimen_unresolved)))
message("Top keyable genera: ",
        paste(sprintf("%s(%d)", head(keyable$target, 6), head(keyable$specimen_unresolved, 6)), collapse = "  "))

# ---- 3. figure: every remaining target -- work already done (resolved) + what remains ----
# No top-N cap: show EVERY target that still has unresolved records. A fully-resolved
# genus isn't a "remaining" task, so its absence here just means its ID work is done.
top <- work %>% arrange(desc(unresolved_total))
long <- bind_rows(
  data.frame(target = top$target, cat = "resolved (to species)", n = top$resolved_species),
  data.frame(target = top$target, cat = "specimen (keyable)",    n = top$specimen_unresolved),
  data.frame(target = top$target, cat = "photo (needs ID)",      n = top$photo_unresolved)) %>%
  filter(n > 0)
long$cat    <- factor(long$cat, levels = c("resolved (to species)", "specimen (keyable)", "photo (needs ID)"))
# genera A->Z at the top; the parenthesised coarse-rank buckets ((tribe),
# (subfamily), (epifamily)) sink to the bottom so they don't split the A-Z genus list.
tg <- unique(as.character(long$target))
long$target <- factor(long$target,
  levels = rev(c(sort(tg[!grepl("^\\(", tg)]), sort(tg[grepl("^\\(", tg)]))))
# per-target % identified to species, labelled at the end of each bar
lab <- top %>% transmute(target, total = resolved_species + unresolved_total,
                         pct_id = ifelse(total > 0, 100 * resolved_species / total, 0)) %>% filter(total > 0)
lab$target <- factor(lab$target, levels = levels(long$target))
g <- ggplot(long, aes(x = n, y = target, fill = cat)) +
  geom_col(width = 0.72) +
  geom_text(data = lab, aes(x = total, y = target, label = sprintf("%.0f%% ID'd", pct_id)),
            hjust = -0.12, size = 2.8, color = "grey25", inherit.aes = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.13))) +
  # lightness = resolution status, hue = method. resolved (BOTH methods, to species) = the DARK red/blue
  # blend (a purple). specimen-keyable & photo-needs-ID are UNRESOLVED -> the LIGHT method colours -- the
  # same faded red/blue the per-method panels use for "genus-only", so a pale bar always = "still needs ID".
  scale_fill_manual(values = c(
    "resolved (to species)" = grDevices::colorRampPalette(c(unname(BEE_METHOD_COL["lethal"]), unname(BEE_METHOD_COL["nonlethal"])))(3)[2],
    "specimen (keyable)"    = grDevices::colorRampPalette(c(unname(BEE_METHOD_COL["lethal"]),    "white"))(3)[2],
    "photo (needs ID)"      = grDevices::colorRampPalette(c(unname(BEE_METHOD_COL["nonlethal"]), "white"))(3)[2]),
    name = NULL) +
  labs(title = sprintf("Q7 - Species-level ID: work done vs all %d remaining targets", nrow(top)),
       caption = str_wrap(sprintf("%s of %s bee records (%.0f%%) already identified to species.  %s",
                            format(n_resolved, big.mark = ","), format(n_total, big.mark = ","), pct_resolved,
                            scope_cap("all records", "resolved vs specimen-keyable vs photo", "genus / coarse rank")), 82),
       x = "records", y = NULL) +
  theme_beescabr(11) +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
ggsave(file.path(OUT_DIR, "coverage_id_targets.png"), g,
       width = 9.5, height = max(7, 0.30 * nrow(top) + 2), dpi = 200, bg = "white")  # taller for all targets

# ---- 4. per-method species-ID progress: to species vs genus-only --------------
# Shared genus set: EVERY named genus (no top-N cap), ALPHABETICAL -- so the specimen
# and photo panels line up genus-for-genus and nothing is hidden by an arbitrary cutoff.
# Genera with 0 records for a method still hold their row (see drop = FALSE below).
method_genera <- rec %>% filter(!is.na(genus), genus != "") %>%
  distinct(genus) %>% pull(genus) %>% sort()
method_genus_fig <- function(m, file, method_label) {
  d <- rec %>% filter(method == m, genus %in% method_genera) %>%
    group_by(genus) %>%
    summarise(species = sum(resolved), genus_only = sum(!resolved), total = n(), .groups = "drop")
  d <- data.frame(genus = method_genera, stringsAsFactors = FALSE) %>%
    left_join(d, by = "genus") %>%
    mutate(species = coalesce(species, 0L), genus_only = coalesce(genus_only, 0L), total = coalesce(total, 0L))
  long <- bind_rows(
    data.frame(genus = d$genus, cat = "identified to species",   n = d$species),
    data.frame(genus = d$genus, cat = "genus-only (unresolved)", n = d$genus_only)) %>% filter(n > 0)
  long$cat   <- factor(long$cat, levels = c("identified to species", "genus-only (unresolved)"))
  long$genus <- factor(long$genus, levels = rev(method_genera))   # alphabetical (A at top)
  pct <- 100 * sum(d$species) / sum(d$total)
  # this panel is a SINGLE method, so colour it in that method's hue (red = specimen, blue = photo):
  # solid = identified (done), lightened = genus-only (not yet).
  mcol       <- unname(BEE_METHOD_COL[if (m == "specimen") "lethal" else "nonlethal"])
  mcol_faded <- grDevices::colorRampPalette(c(mcol, "white"))(3)[2]
  # per-genus % identified to species, labelled at the end of each bar (0-record genera get no label)
  lab2 <- d %>% filter(total > 0) %>% transmute(genus, total, pct_id = 100 * species / total)
  lab2$genus <- factor(lab2$genus, levels = rev(method_genera))
  g2 <- ggplot(long, aes(x = n, y = genus, fill = cat)) +
    geom_col(width = 0.72) +
    geom_text(data = lab2, aes(x = total, y = genus, label = sprintf("%.0f%%", pct_id)),
              hjust = -0.15, size = 2.7, color = "grey25", inherit.aes = FALSE) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    scale_y_discrete(drop = FALSE) +   # keep EVERY shared genus, even 0-record ones (e.g. photo-only Xenoglossa has 0 specimens) so specimen & photo align row-for-row
    scale_fill_manual(values = c("identified to species"   = mcol,         # solid method colour = done
                                 "genus-only (unresolved)" = mcol_faded),   # faded method colour = not yet
                      name = NULL) +
    labs(title = sprintf("Q7 - %s: species-level ID progress (all %d genera)", method_label, length(method_genera)),
         caption = str_wrap(sprintf("%s: %s of %s records (%.0f%%) identified to species in these genera.  %s",
                              method_label, format(sum(d$species), big.mark = ","),
                              format(sum(d$total), big.mark = ","), pct,
                              scope_cap("records with a genus", method_label, "genus")), 82),
         x = "records", y = NULL) +
    theme_beescabr(11) +
    theme(legend.position = "top", panel.grid.major.y = element_blank())
  ggsave(file, g2, width = 9.5, height = max(7, 0.30 * length(method_genera) + 2), dpi = 200, bg = "white")  # taller for all genera
}
method_genus_fig("specimen", file.path(OUT_DIR, "coverage_id_targets_specimen.png"), "Specimen (net)")
method_genus_fig("photo",    file.path(OUT_DIR, "coverage_id_targets_photo.png"),    "Photo (iNaturalist)")

# ---- 5. ID-completeness funnel: how far each record got, by method ------------
grid   <- expand.grid(method = c("specimen", "photo"),
                      level  = c("species", "genus", "coarser"), stringsAsFactors = FALSE)
funnel <- merge(grid, dplyr::count(rec, method, level, name = "n"), all.x = TRUE)
funnel$n[is.na(funnel$n)] <- 0L
funnel$level  <- factor(funnel$level,  levels = c("species", "genus", "coarser"))
funnel$method <- factor(funnel$method, levels = c("specimen", "photo"))
gf <- ggplot(funnel, aes(x = level, y = n, fill = method)) +
  geom_col(position = position_dodge(0.72), width = 0.68) +
  geom_text(aes(label = format(n, big.mark = ",")), position = position_dodge(0.72), vjust = -0.3, size = 3) +
  # method now has its own colours (red net / blue photo), kept off the transect palette
  scale_fill_manual(values = c(specimen = unname(BEE_METHOD_COL["lethal"]),
                               photo    = unname(BEE_METHOD_COL["nonlethal"])), name = NULL,
                    labels = c(specimen = "specimen (net)", photo = "photo (iNat)")) +
  scale_x_discrete(labels = c(species = "to species", genus = "genus-only", coarser = "coarser than genus")) +
  labs(title = "Q7 - Bee ID completeness: how far each record got, by method",
       caption = str_wrap(sprintf("%s of %s records (%.0f%%) identified to species.  %s",
                            format(n_resolved, big.mark = ","), format(n_total, big.mark = ","), pct_resolved,
                            scope_cap("all records", "specimen (net) vs photo (iNat)", "resolution level")), 82),
       x = NULL, y = "records") +
  theme_beescabr(11) +
  theme(legend.position = "top", panel.grid.major.x = element_blank())
ggsave(file.path(OUT_DIR, "coverage_id_completeness.png"), gf, width = 8.5, height = 5.6, dpi = 200, bg = "white")

message("Wrote coverage_id_targets.{csv,png}, _specimen.png, _photo.png, and coverage_id_completeness.png to ", OUT_DIR)
