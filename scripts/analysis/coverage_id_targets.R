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
OUT_DIR       <- "data/analysis/coverage"
SPECIES_RANKS <- c("species", "subspecies")
TOP_N         <- 15
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
scope_cap <- function(scope, method, rank) sprintf("Scope: %s  |  Method: %s  |  Rank: %s",
                                                   scope, method, rank)

# ---- 1. pool records, keep only those NOT resolved to species ----------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
prep <- function(df, method) data.frame(
  method = method, taxon_rank = str_squish(tolower(df$taxon_rank)),
  genus = str_squish(df$genus), stringsAsFactors = FALSE)
rec <- bind_rows(prep(spec, "specimen"), prep(inat, "photo")) %>%
  mutate(resolved = taxon_rank %in% SPECIES_RANKS,
         # finest label we can name the unresolved cluster by: genus if known, else the rank
         target = ifelse(!is.na(genus) & genus != "", genus, paste0("(", taxon_rank, ")")))
unresolved <- rec %>% filter(!resolved)
message(sprintf("Unresolved records (coarser than species): %d of %d (%.1f%%)",
                nrow(unresolved), nrow(rec), 100 * nrow(unresolved) / nrow(rec)))

# ---- 2. worklist: unresolved records per target, split by method -------------
work <- unresolved %>%
  group_by(target) %>%
  summarise(unresolved_total = n(),
            specimen_unresolved = sum(method == "specimen"),
            photo_unresolved    = sum(method == "photo"),
            ranks = paste(sort(unique(taxon_rank)), collapse = ", "),
            .groups = "drop") %>%
  arrange(desc(specimen_unresolved), desc(unresolved_total))
write.csv(work, file.path(OUT_DIR, "coverage_id_targets.csv"), row.names = FALSE)

keyable <- work %>% filter(specimen_unresolved > 0)
message(sprintf("Targets with specimen-backed (keyable) records: %d genera, %d specimens",
                nrow(keyable), sum(keyable$specimen_unresolved)))
message("Top keyable genera: ",
        paste(sprintf("%s(%d)", head(keyable$target, 6), head(keyable$specimen_unresolved, 6)), collapse = "  "))

# ---- 3. figure: top targets by unresolved records, stacked by method ---------
top <- work %>% slice_max(unresolved_total, n = TOP_N, with_ties = FALSE)
long <- bind_rows(
  data.frame(target = top$target, method = "specimen (keyable)", n = top$specimen_unresolved),
  data.frame(target = top$target, method = "photo",              n = top$photo_unresolved)) %>%
  filter(n > 0)
long$target <- factor(long$target, levels = rev(top$target))
g <- ggplot(long, aes(x = n, y = target, fill = method)) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = c("specimen (keyable)" = "#1a9850", "photo" = "#b8b8b8"), name = NULL) +
  labs(title = sprintf("Q7 - Target-ID list: top %d bee taxa needing species-level ID", TOP_N),
       subtitle = str_wrap(scope_cap("all records not resolved to species",
                            "specimen (keyable) vs photo", "genus / coarse rank"), 78),
       x = "unresolved records", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "#b2182b", size = 9),
        legend.position = "top", panel.grid.major.y = element_blank())
ggsave(file.path(OUT_DIR, "coverage_id_targets.png"), g, width = 9, height = 6.2, dpi = 200, bg = "white")
message("Wrote coverage_id_targets.{csv,png} to ", OUT_DIR)
