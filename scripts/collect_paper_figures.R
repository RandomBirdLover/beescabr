# =============================================================
# collect_paper_figures.R -- sort analysis outputs into the two paper folders
# beescabr / Cabrillo National Monument (CABR) native bees
#
# The pipeline writes every output into data/analysis/<theme>/... . This step COPIES
# each output into the paper it belongs to, so the two manuscripts have a clean, ready
# folder of exactly their material (originals stay put -- copies, not moves):
#   * data/analysis/journal_paper_2026/  -- the lethal-vs-non-lethal METHOD paper (fair window)
#   * data/analysis/nps_report_2026/     -- the NPS park report (all records)
# The source sub-folder structure is MIRRORED under each paper folder, so same-named
# files (e.g. species_vegan_bars.png in by_method/ vs by_transect/) never collide.
#
# SCOPE OF WHAT'S COPIED:
#   * REPORT (all-records)  -- figures AND tables/HTML/txt (it's a data report).
#   * JOURNAL (fair window) -- figures (PNG) only for now; its data tables are split later.
#
# Re-runnable: CLEARS both paper folders first, then re-copies, so retired/renamed
# outputs never linger. Run AFTER the analysis pipeline (or append to run_all_analysis.R):
#   Rscript scripts/collect_paper_figures.R
# =============================================================

BASE <- "data/analysis"
PAPERS <- list(journal = file.path(BASE, "journal_paper_2026"),
               report  = file.path(BASE, "nps_report_2026"))
ALL_EXT <- "\\.(png|csv|html|txt)$"
PNG_EXT <- "\\.png$"

list_dir <- function(reldir, ext) {
  p <- file.path(BASE, reldir); if (!dir.exists(p)) return(character(0))
  sub(paste0("^", BASE, "/"), "", list.files(p, pattern = ext, full.names = TRUE, recursive = TRUE))
}

# ---- JOURNAL (figures only for now) -----------------------------------------
JOURNAL_DIRS  <- c("method_comparison/efficiency", "method_comparison/effort",
                   "richness/rarefaction/by_method", "richness/rarefaction/by_observer")
JOURNAL_FILES <- c(
  "coverage/id_resolution/coverage_id_completeness.png",
  "coverage/id_resolution/coverage_id_targets_journal.png",
  "coverage/id_resolution/coverage_id_targets_photo.png",
  "coverage/id_resolution/coverage_id_targets_specimen.png",
  "coverage/records_by_evidence/records_per_genus_by_evidence_journal.png",
  "coverage/records_by_evidence/records_per_species_by_evidence_journal.png",
  "method_comparison/yield/yield_by_method_journal.png",
  "phenology/effort_by_month_journal.png",
  "phenology/effort_year_month_grid_journal.png",
  "richness/accumulation/accumulation_genera_journal.png",
  "richness/accumulation/accumulation_species_journal.png",
  "richness/diversity/diversity_rank_abundance_journal.png",
  "richness/diversity/transect_effort_journal.png")

# ---- REPORT (all-records: figures + tables + HTML) --------------------------
# whole folders that are ENTIRELY report -> copy every file type
REPORT_DIRS  <- c("coverage/bee_bounties", "coverage/checklist_gaps", "coverage/footprint",
                  "coverage/least_sampled", "coverage/off_transect",
                  "interactions/networks", "interactions/top_plants",
                  "reference/conservation", "reference/field_guide", "reference/nps_summary",
                  "richness/rarefaction/by_transect", "richness/rarefaction/by_year")
# individual report items from the SPLIT folders (figures + their all-records data)
REPORT_FILES <- c(
  # figures
  "coverage/id_resolution/coverage_id_targets_report.png",
  "coverage/records_by_evidence/records_per_genus_by_evidence_report.png",
  "coverage/records_by_evidence/records_per_species_by_evidence_report.png",
  "method_comparison/yield/coverage_yield_by_method_report_genus.png",
  "method_comparison/yield/coverage_yield_by_method_report_species.png",
  "method_comparison/yield/yield_by_method_report.png",
  "phenology/effort_by_month_report.png",
  "phenology/effort_year_month_grid_report.png",
  "phenology/phenology_bee_genus.png",
  "phenology/phenology_bee_species.png",
  "phenology/phenology_plant_genus_bloom_evidence.png",
  "richness/accumulation/accumulation_genera_report.png",
  "richness/accumulation/accumulation_species_report.png",
  "richness/diversity/diversity_evenness_by_transect.png",
  "richness/diversity/diversity_evenness_by_year.png",
  "richness/diversity/diversity_nmds_composition.png",
  "richness/diversity/diversity_rank_abundance_report.png",
  "richness/diversity/transect_effort_report.png",
  # tables / stats (all-records)
  "coverage/id_resolution/coverage_id_targets.csv",
  "coverage/records_by_evidence/records_per_genus_by_evidence_report.csv",
  "coverage/records_by_evidence/records_per_species_by_evidence_report.csv",
  "method_comparison/yield/coverage_method_resolution_chisq_report.txt",
  "method_comparison/yield/coverage_method_resolution_report.csv",
  "method_comparison/yield/coverage_yield_by_method_report_contributor.csv",
  "method_comparison/yield/coverage_yield_by_method_report_method.csv",
  "method_comparison/yield/yield_by_method_taxa_report.csv",
  "phenology/effort_by_month_report.csv",
  "phenology/phenology_bee_genus.csv", "phenology/phenology_bee_genus_rayleigh.csv",
  "phenology/phenology_bee_species.csv", "phenology/phenology_bee_species_rayleigh.csv",
  "phenology/phenology_plant_genus_bloom_evidence.csv", "phenology/phenology_plant_genus_bloom_evidence_rayleigh.csv",
  "richness/accumulation/transect_accumulation_summary.csv",
  "richness/diversity/diversity_by_transect.csv", "richness/diversity/diversity_by_year.csv",
  "richness/diversity/diversity_permanova.txt", "richness/diversity/spatial_richness_grid.csv",
  "richness/diversity/transect_richness.csv")

# ---- collect ----------------------------------------------------------------
copy_paper <- function(paper, rel_items) {
  dest_root <- PAPERS[[paper]]
  if (dir.exists(dest_root)) unlink(dest_root, recursive = TRUE)   # clear so nothing stale lingers
  dir.create(dest_root, recursive = TRUE, showWarnings = FALSE)
  n_ok <- 0L; miss <- character(0)
  for (rel in unique(rel_items)) {
    src <- file.path(BASE, rel)
    if (!file.exists(src)) { miss <- c(miss, rel); next }
    dst <- file.path(dest_root, rel); dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    if (file.copy(src, dst, overwrite = TRUE)) n_ok <- n_ok + 1L
  }
  message(sprintf("  %-8s <- %d files%s", paper, n_ok,
                  if (length(miss)) sprintf("  (MISSING %d: %s)", length(miss), paste(miss, collapse = ", ")) else ""))
}

message("Collecting outputs into the two paper folders:")
copy_paper("journal", c(unlist(lapply(JOURNAL_DIRS, list_dir, ext = PNG_EXT)), JOURNAL_FILES))
copy_paper("report",  c(unlist(lapply(REPORT_DIRS,  list_dir, ext = ALL_EXT)), REPORT_FILES))
message("Done. journal_paper_2026/ (figures) and nps_report_2026/ (figures + tables) under ", normalizePath(BASE))
