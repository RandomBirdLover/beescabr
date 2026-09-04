# =============================================================
# analysis/findings_summaries.R   (runs LAST -- excluded from the auto-discovered loop)
#
# "Every analysis has a summary." For each analysis in the pipeline this writes a small,
# plain-language FINDINGS summary -- a field/value table (question, method, what it controls
# for, the headline finding, caveats, outputs) -- into data/analysis/findings_generated/, and then a
# master data/analysis/findings_generated/findings_index.csv with one row per analysis (name, type,
# one-line key finding). The point: the write-up lives WITH the data, not only in the README.
#
# Where a headline number is cheap to read live (from an analysis's own output CSV) it is
# pulled in so the summary stays current; everything else is the analysis's documented finding.
# Missing outputs are handled gracefully (the qualitative summary still writes).
#
# Sourced explicitly at the END of run_all_analysis_pipeline.R (after all analyses have produced their
# outputs). Depends on: base R only.
# =============================================================

if (!exists("DIR_REPORT")) source("scripts/config.R")   # DIR_REPORT / DIR_JOURNAL paper roots
# Each analysis's findings file is written NEXT TO the outputs it describes, so the
# summary is found by whoever opens that folder. Only the cross-cutting index sits
# at the top of the season folder, as the way in.
FIND_DIR <- DIR_REPORT
dir.create(FIND_DIR, recursive = TRUE, showWarnings = FALSE)

# where does this analysis actually write? Find its first named output in the tree.
.all_outputs <- NULL
.find_home <- function(outputs) {
  txt <- as.character(outputs)
  # many entries name their folder outright ("method_comparison/yield/x.png")
  dirs <- regmatches(txt, gregexpr("[a-z_]+/[a-z_]+(/[a-z_]+)*", txt))[[1]]
  dirs <- sub("/(website|fair_[a-z0-9_]+)(/.*)?$", "", dirs)
  for (d in dirs) {                                # the match may swallow the filename too
    while (grepl("/", d)) {
      if (dir.exists(file.path(DIR_REPORT, d))) return(file.path(DIR_REPORT, d))
      d <- sub("/[^/]*$", "", d)
    }
  }
  fs <- trimws(strsplit(txt, ";")[[1]])
  fs <- fs[!is.na(fs) & grepl("[.](csv|png|html)$", fs)]
  if (!length(fs)) return(FIND_DIR)
  if (is.null(.all_outputs))
    .all_outputs <<- list.files(DIR_REPORT, recursive = TRUE, full.names = TRUE)
  for (f in fs) {                                  # some names are braced globs or prose
    hit <- .all_outputs[basename(.all_outputs) == f]
    if (!length(hit)) {                            # "x.csv/png" shorthand -> match the stem
      stem <- sub("[.](csv|png|html).*$", "", f)
      if (nzchar(stem)) hit <- .all_outputs[startsWith(basename(.all_outputs), stem)]
    }
    if (length(hit)) {
      d <- dirname(hit[[1]])
      # a findings file belongs with the DATA, not inside website/ or fair_window/
      d <- sub("/(website|fair_[a-z0-9_]+)$", "", d)
      return(d)
    }
  }
  FIND_DIR
}
.today <- as.character(Sys.Date())

# best-effort read of an output CSV (NULL if absent) + a couple of tiny extractors
.rd  <- function(p) if (file.exists(p)) tryCatch(read.csv(p, stringsAsFactors = FALSE, check.names = FALSE),
                                                 error = function(e) NULL) else NULL
.n   <- function(df) if (is.null(df)) NA_integer_ else nrow(df)
.chr <- function(x) if (length(x) == 0 || is.na(x)) "-" else as.character(x)

.index <- list()
# fw(): write one analysis's findings table + register a master-index row.
fw <- function(name, title, type, key_finding, details = character(0), outputs = "") {
  df <- data.frame(
    field = c("analysis name", "type", "key_finding", names(details), "outputs", "data_as_of"),
    value = c(title, type, key_finding, unname(details), outputs, .today),
    stringsAsFactors = FALSE)
  home <- .find_home(outputs)
  dir.create(home, recursive = TRUE, showWarnings = FALSE)
  write.csv(df, file.path(home, paste0(name, "_findings.csv")), row.names = FALSE)
  .index[[length(.index) + 1]] <<- data.frame(analysis = title, type = type,
                                              key_finding = key_finding,
                                              findings_file = paste0(name, "_findings.csv"),
                                              stringsAsFactors = FALSE)
}

# ---- live numbers (best-effort) ---------------------------------------------
fs   <- .rd(file.path(DIR_REPORT, "interactions/networks/forage_selectivity_summary.csv"))
h2   <- .rd(file.path(DIR_REPORT, "interactions/networks/bee_genus_specialization_h2.csv"))
acc  <- .rd(file.path(DIR_REPORT, "richness/accumulation/transect_accumulation_summary.csv"))
holw <- .rd(file.path(DIR_REPORT, "coverage/checklist_gaps/cabr_bees_not_on_county_checklist.csv"))
yld_m <- .rd(file.path(DIR_JOURNAL, "method_comparison/yield/fair_method_2021_2023/coverage_yield_by_method_journal.csv"))
lsb   <- .rd(file.path(DIR_REPORT, "coverage/least_sampled/least_sampled_bees.csv"))
.pick <- function(df, g, col) if (is.null(df)) "-" else {
  key <- if (!is.null(df$grp)) df$grp else df$method   # yield_by_method keys on method
  v <- df[[col]][as.character(key) == g]; if (length(v)) .chr(v[1]) else "-" }

fs_sel <- if (!is.null(fs)) sum(fs$forage_pattern == "Selective") else NA
fs_tot <- .n(fs)
fs_top <- if (!is.null(fs)) { s <- fs[fs$forage_pattern == "Selective", ]
  s <- s[order(-suppressWarnings(as.numeric(s$preferred_vs_available_x))), ]
  paste(utils::head(sprintf("%s -> %s", s$genus, s$preferred_plant), 3), collapse = "; ") } else "-"
h2_sig <- if (!is.null(h2)) sum(suppressWarnings(as.numeric(h2$H2prime_p)) < 0.05, na.rm = TRUE) else NA

# ============================ INFERENTIAL ====================================
fw("forage_selectivity",
   "Forage selectivity -- does a bee genus favor plants beyond availability?",
   "inferential",
   sprintf("%s of %s bee genera show a real plant preference; the set is stable across abundance/month/year/method controls.",
           .chr(fs_sel), .chr(fs_tot)),
   c(question    = "For each bee genus, does it visit plants differently from what was actually available to it?",
     "analysis"      = "Matched Monte-Carlo chi-square; availability matched to each genus's own (month, year, survey-method) cells, leave-one-out",
     controls_for   = "overall abundance; month (phenology); year (climate); survey method (net vs photo)",
     not_controlled = "observer (averages out over 10-48 observers/genus); plant detectability (no independent bloom census)",
     top_specialists = fs_top,
     generalists     = "Megachile, Nomada",
     caveats     = "availability = realized community use per cell, not a bloom census; verdicts near p=0.05 borderline"),
   "forage_selectivity_summary.csv; bee_plant_network_genus.png; bee_plant_network_species.png; field-guide Forage-preference column")

fw("interactions_genus_species_webs",
   "Within-genus niche partitioning (H2')",
   "inferential",
   sprintf("%s bee genera' species partition plant genera more than their flight-season/method differences explain (Melissodes, Habropoda drop once controlled).",
           .chr(h2_sig)),
   c(question    = "Within a genus, do the species divide up plant genera (niche partitioning) beyond chance?",
     "analysis"      = "H2' specialization vs a null that permutes species labels within (month x method) strata",
     controls_for = "flight season (month) and survey method; NOT year (species overlap in years -> would kill power)",
     effect_of_control = "Melissodes and Habropoda drop to non-significant -- their apparent partitioning was seasonal/method timing",
     caveats     = "borderline p-values can jitter; only significant genera are shown in the figures"),
   "bee_genus_specialization_h2.csv; bee_genus_specialization_overview.png; genus_species_webs/*.png")

# ============================ ESTIMATORS =====================================
acc_line <- if (!is.null(acc)) paste(sprintf("%s %s%% complete", acc$transect, acc$species_pct_complete), collapse = "; ") else "-"
fw("genera_and_species_accumulation",
   "Species / genera accumulation & completeness (Chao2)",
   "estimator",
   sprintf("Per-transect sampling completeness (Chao2). %s.", if (is.null(acc)) "run to populate" else "TP/UPMON near-complete; OT least sampled"),
   c("analysis" = "Chao2 asymptotic richness vs observed, per transect + park",
     per_transect_completeness = acc_line,
     assumption = "assumes roughly even sampling; effort is 2024-heavy + seasonal, so treat as approximate"),
   "richness/accumulation/: transect_accumulation_summary.csv; REPORT bee_taxa_accumulation_by_transect_{species,genus}.png (transect completeness); JOURNAL accumulation_by_effort_journal_{species,genus}.png (lethal vs non-lethal, fair window)")

fw("richness_at_equal_effort",
   "Rarefaction/extrapolation (iNEXT, Hill numbers) -- JOURNAL only",
   "estimator",
   "iNEXT is an R package that answers: at EQUAL sampling effort, do lethal or non-lethal surveys find more kinds of bee (fair_method_2021_2023), and do beeple or interns (fair_observer_2024)? Raw counts cannot answer it because the groups were not sampled equally. iNEXT levels them by coverage (how completely each group was sampled) and reports three views: q0 = how many kinds, q1 = weighted toward common kinds, q2 = weighted hard toward the most common.",
   c("analysis" = "iNEXT size- and coverage-based rarefaction/extrapolation (journal only; the report uses the vegan curves/bars)",
     assumption = "standardizes by sample size/coverage but still sensitive to uneven effort; read CIs as approximate. CONFOUND in fair_method_2021_2023: in Mar-Oct 2021-2023 only interns netted and only beeple photographed, so lethal-vs-non-lethal is also beeple-vs-interns on the same records, and a difference there cannot be attributed to the method rather than to who was surveying. fair_observer_2024 is the control: May-Sep 2024, both groups photographing, so method is held constant and the observer effect is measured cleanly. Read the two together, never either alone."),
   "richness/rarefaction/fair_method_2021_2023/bee_richness_lethal_vs_nonlethal_both_ranks_rarefaction.png and richness/rarefaction/fair_observer_2024/bee_richness_beeple_vs_interns_both_ranks_rarefaction.png -- ONE figure per comparison, both ranks and all three Hill orders on it, iNEXT curves with vegan points as the cross-check (+ the matching .csv, which also keeps the coverage-standardised curves the figure omits). The _effort_standardized_estimates.csv beside each holds both ranks and all three standardizations (asymptotic / equal_size / equal_coverage) in one table, keyed by a `basis` column")

fw("richness_rarefied_to_smallest_group",
   "Rarefaction (vegan) -- curves + rarefied-richness bars",
   "estimator",
   "vegan is an R package doing the older, simpler version of the same levelling: cut every group down to the smallest group's record count and see what is left. It is the report's rarefaction figures, and in the journal it is the cross-check on iNEXT. The two agreeing is what makes the result trustworthy.",
   c("analysis" = "vegan rarefaction to the lowest group's record total",
     assumption = "assumes even sampling within a group"),
   "richness/rarefaction/: report bee_richness_by_{transect,year}_rarefaction.png (+ _{species,genus}.csv). The method/observer comparison is drawn by rarefaction_combined.R into fair_method_2021_2023/, where vegan appears as the cross-check on the iNEXT curves; its rarefied numbers are in each window folder's _rarefied_to_smallest_group.csv, both ranks in one table")

fw("diversity_indices",
   "Community diversity (Shannon / Simpson / NMDS)",
   "estimator",
   "Diversity indices and community composition (NMDS) by transect and year.",
   c("analysis" = "Shannon/Simpson/evenness + rank-abundance + NMDS ordination",
     assumption = "diversity indices are effort-sensitive; the 2024-heavy skew inflates apparent year differences"),
   "diversity_by_transect.csv; diversity_by_year.csv; REPORT: bee_community_composition_nmds.png, bee_species_commonness_rank_abundance.png (pooled + per-transect combined); JOURNAL: diversity_rank_abundance_journal.png")

fw("phenology_activity",
   "Seasonal activity phenology (+ Rayleigh test)",
   "estimator",
   "When bees (per genus/species) and flowering plants are active across the year; Rayleigh tests seasonal concentration.",
   c("analysis" = "circular-mean activity ridgelines + Rayleigh test of seasonal concentration",
     confound = "seasonal survey effort (interns ~Mar-Oct) can drive apparent bee seasonality -- read timing, not intensity"),
   "bee_genus_activity_by_month.png; bee_species_activity_by_month.png; plant_bloom_timing_for_bees.png; *_rayleigh.csv")

# ============================ DESCRIPTIVE ====================================
fw("interactions_network",
   "Plant-bee visitation network (webs + heatmaps)",
   "descriptive",
   "Who visits what: full plant-genus x bee network as webs + heatmaps (raw co-occurrence; read descriptively).",
   c(note = "raw visitation counts -- NOT preference (see forage_selectivity for the inferential preference test)",
     scope = "specimen net + iNaturalist photo pooled, CABR only"),
   "bee_plant_network_genus.png; bee_plant_network_species.png; bee_plant_interaction_heatmap_*.png; interactions_*_matrix.csv; bee_genus_specialization_overview.png; bee_specialist_network.png")

fw("interactions_top_plants",
   "Top plants bees visit",
   "descriptive",
   "Ranked most-visited plant genera (whole-park / survey-only / by method + a per-month breakdown).",
   c(note = "counts reflect sampling effort + detectability, not preference; a common easy-to-photograph plant tops the list"),
   "interactions_top_plants.csv; interactions_top_plants.png; interactions_top_plants_by_month.csv/png")

fw("bee_field_guide",
   "Bee field guide -- by species",
   "descriptive (+ inferential Forage-preference column)",
   "Per-species reference: peak day, active months, most-recorded flowers, diet breadth, status, IUCN, and an availability-corrected Forage-preference column (species-level, same matched month/year/method test as the genus guide; ~19 species selective, shown only where a species has >=50 plant-visit records, so it fills in as sampling grows).",
   c(note = "'Most-recorded flowers' = where it was seen most; 'Forage preference' is the matched-test result (Selective -> plant / Generalist / too few records)",
     threshold = "forage preference gated at >=50 plant-visit records -- most species read 'too few records to judge' today"),
   "website/bee_field_guide_species.html; bee_field_guide_species.csv")

fw("bee_field_guide_genus",
   "Bee field guide -- by genus",
   "descriptive (+ inferential Forage-preference column)",
   "Per-genus companion guide; carries the inferential Forage-preference column from the selectivity test.",
   c(note = "Most-recorded flowers/Most-used plant are descriptive; Forage preference is the matched-test result"),
   "website/bee_field_guide_genus.html; bee_field_guide_genus.csv")

fw("rare_bee_plants",
   "Plants the park's rare / at-risk bees were recorded on",
   "descriptive",
   "Which plants the rare (< threshold records) and IUCN-threatened bees were RECORDED on -- management-facing. Bars = where sightings fall (NOT a preference: too few records to correct for availability). Threatened bees with >=20 records also get an availability-corrected PREFERRED plant, which can differ from the most-recorded one (e.g. Bombus californicus recorded most on milkvetch but prefers paintbrush; B. sonorus prefers stinkweed).",
   c(recorded_vs_preferred = "bars = recorded-on (availability-blended); PREFERRED (starred) = availability-corrected, shown only where n>=20 -- same matched test as the genus webs",
     note = "low counts: read as 'where the few sightings concentrate', not visit rates or preference",
     threatened_source = "IUCN threatened set read live from the IUCN cache"),
   "reference/conservation/plants_anchoring_rare_bees.csv/png; rare_bee_forage_preference.csv/png")

fw("bee_bounties",
   "Collecting / photo bounties (method gaps)",
   "descriptive",
   "Taxa recorded by one method but not the other -- worklist for what to net (voucher) or photograph next.",
   c(note = "gap = present in one method, absent in the other; directs future effort"),
   "coverage/bee_bounties/specimen_bee_bounty.csv/png; inaturalist_bee_bounty.csv/png; *_bounty_map.png")

fw("coverage_cabr_vs_holway",
   "CABR bees not on the Holway county checklist",
   "descriptive",
   sprintf("%s CABR taxa are not on the Holway San Diego County checklist (candidate county additions / to verify).",
           .chr(.n(holw))),
   c(note = "iNaturalist records flagged for verification before treating as county additions"),
   "cabr_bees_not_on_county_checklist.csv/png; cabr_bees_not_on_county_checklist_inat_records.csv")

fw("yield_by_method",
   "Yield by method (lethal vs non-lethal Venn)",
   "descriptive",
   "How the lethal and non-lethal methods overlap in the taxa they detect -- each method's unique + shared species and genera (the yield tier of the effort/yield/efficiency comparison).",
   c(note   = "shows each method's unique + shared taxa contribution",
     scope  = "ALL records (survey filter off), since 'what each method detects' wants every record",
     tier   = "YIELD -- see method_comparison/effort for sampling work and method_comparison/efficiency for richness at equal effort"),
   "method_comparison/yield/yield_by_method_report.png; bee_yield_by_contributor_and_method.png; coverage_method_resolution_report.csv; yield_by_method_taxa_report.csv")

fw("effort_by_method",
   "Effort by method (survey trips)",
   "descriptive",
   "Sampling WORK per method: survey trips, lethal vs non-lethal. The honest denominator for reading yield -- non-lethal logged far more trips than lethal, so raw yield can't be compared directly (see efficiency tier).",
   c(tier = "EFFORT -- first of the three method-comparison tiers (effort -> yield -> efficiency)"),
   "method_comparison/effort/coverage_effort_by_method.png; coverage_effort_by_method.csv")

fw("efficiency_by_method",
   "Efficiency by method (rarefied to equal effort)",
   "descriptive",
   "Richness at EQUAL sampling effort (rarefaction): both methods sub-sampled to the smaller method's record total, at species and genus rank. At species level lethal stays ahead (47 vs 33); at genus level the two are even (23 vs 23) -- non-lethal's raw genus lead was an effort artifact.",
   c(tier   = "EFFICIENCY -- third tier; removes the effort imbalance that makes raw yield unfair to compare",
     "analysis" = "Hurlbert rarefaction to the smaller method's total records, survey records only"),
   "method_comparison/efficiency/efficiency_by_method_both_ranks.png (both ranks on one figure: species solid, genera hatched)")

fw("bees_found_off_transect",
   "Off-transect coverage",
   "descriptive",
   "What the off-transect (opportunistic) records add beyond the fixed transects.",
   character(0),
   "bees_found_off_transect_summary.csv; bees_found_off_transect_taxa.csv; bees_found_off_transect.png")

fw("coverage_id_targets",
   "Identification targets",
   "descriptive",
   "Genus-level records that could still be pushed to species (ID targets), by method.",
   character(0),
   "bees_needing_id_by_genus.csv; coverage_id_targets_*.png; coverage_id_completeness.png")

fw("coverage_yield_by_method",
   "Yield by method (lethal vs non-lethal)",
   "descriptive",
   sprintf("Fair footing (survey-only, Mar-Oct, 2021-2023): at SPECIES level lethal netting records more species (%s vs %s) and far more method-exclusive species (%s vs %s) despite fewer records; but at GENUS level the two are even -- non-lethal edges ahead (%s vs %s genera; exclusive %s vs %s). So lethal's species advantage is mostly ID RESOLUTION (specimens key to species, photos stall at genus), not detection.",
           .pick(yld_m, "lethal", "species"),           .pick(yld_m, "nonlethal", "species"),
           .pick(yld_m, "lethal", "exclusive_species"), .pick(yld_m, "nonlethal", "exclusive_species"),
           .pick(yld_m, "lethal", "genera"),            .pick(yld_m, "nonlethal", "genera"),
           .pick(yld_m, "lethal", "exclusive_genera"),  .pick(yld_m, "nonlethal", "exclusive_genera")),
   c(scope    = "SURVEY-ONLY, March-October, 2021-2023 (year-clipped so both methods share the window)",
     groups   = "in this window lethal = intern net specimens; non-lethal = beeple survey photos (general public and intern iNat photos are NOT included -- see the REPORT figure bee_yield_by_contributor_and_method_* for the all-records contributor view that adds them)",
     controls_for = "season (Mar-Oct) + year (2021-2023) + survey scope -- removes non-lethal's 3 extra years",
     species_vs_genus = "species-level = detection + ID resolution; genus-level = detection alone (photos not penalised for stalling at genus)",
     takeaway = "specimens win on species-level yield/efficiency; detection (genus) is about even -- the gap is ID resolution, not who finds more bees"),
   "method_comparison/yield/coverage_yield_by_method_journal.csv (table only; the journal figure is the method Venn, method_comparison/yield/yield_by_method_journal.png). REPORT all-records contributor/method view: bee_yield_by_contributor_and_method_{species,genus}.png")

fw("coverage_cabr_share_of_county",
   "CABR share of county diversity",
   "descriptive",
   "How much of San Diego County's native-bee diversity the tiny CABR site captures.",
   character(0),
   "cabr_share_of_county.csv; cabr_county_map.png (the lollipop figure was retired: every number it showed is on the map, which also shows where Cabrillo is)")

fw("coverage_cabr_county_map",
   "CABR county locator map -- speck of area, big share of bees",
   "descriptive",
   "A locator map placing tiny Cabrillo National Monument on the San Diego County map: CABR is a fraction of a percent of the county by AREA yet carries a disproportionately large share of its native-bee species and genera -- the visual companion to the share-of-county figures.",
   c(measures    = "area % straight from the NPS official CABR polygon vs the San Diego County polygon (same CRS); species/genus shares from the CABR official checklist vs the Holway county checklist",
     the_numbers = "the actual area % and diversity shares are computed and tabulated by coverage_cabr_share_of_county (cabr_share_of_county.csv); this script is the map that dramatizes them",
     scope       = "checklist-level (all records feeding each checklist), not survey-only"),
   "cabr_county_map.png")

fw("records_per_genus_by_evidence",
   "Evidence backing each genus",
   "descriptive",
   "How much (and what kind of) evidence backs each bee genus -- lethal specimen vs non-lethal iNat photo, per genus.",
   character(0),
   "coverage/records_by_evidence/: records_by_evidence_{report,journal}_genus.{csv,png}")

fw("records_per_species_by_evidence",
   "Evidence backing each species",
   "descriptive",
   "How much (and what kind of) evidence backs each bee SPECIES -- lethal specimen vs non-lethal iNat photo, one row per species (genus-only records excluded); species with <10 records flagged as thin.",
   character(0),
   "coverage/records_by_evidence/: records_by_evidence_{report,journal}_species.{csv,png}")

fw("least_sampled_bees",
   "Least-sampled bees -- go-find-it sheet",
   "descriptive",
   sprintf("The %s bee species with <50 records TOTAL across both methods -- under-detected by netting AND iNaturalist -- each with its per-method split and when/where/on-what-flower context. Coverage split: %s.",
           .chr(.n(lsb)),
           if (is.null(lsb)) "run to populate" else paste(sprintf("%d %s", as.integer(table(lsb$coverage)), names(table(lsb$coverage))), collapse = ", ")),
   c(threshold   = "least sampled = < 50 records total (both methods pooled), the report's low-record floor",
     coverage    = "both (thin) = a few of each method; photo-only = never netted (also a specimen bounty); specimen-only = never photographed (also an iNat bounty)",
     context     = "when (peak months + active span), where (top transect), flower (top plant genera) pooled across both methods, + an example iNat photo URL",
     vs_bounties = "bee_bounties lists taxa MISSING from one method; this keeps the under-sampled species and adds the find-it context in one sheet"),
   "least_sampled_bees.csv; website/least_sampled_bees.html (the table PNG was retired: an image of a table blurs when zoomed)")

fw("transect_effort",
   "Per-transect sampling effort",
   "descriptive",
   "How many bee records each transect has produced, split by method (lethal vs non-lethal) and as a total; specimens summarised by transect (their reliable spatial unit).",
   c(note = "raw per-transect richness is NOT charted -- unequal effort biases it; the effort-standardized version is rarefaction by_transect"),
   "transect_effort_{report,journal}.png; transect_effort_total_{report,journal}.png; survey_effort_by_transect_richness.csv; transect_effort_journal.csv")

fw("phenology_effort",
   "Survey effort by month",
   "descriptive",
   "Survey effort across months and years -- the context for every seasonal/annual pattern in the other analyses.",
   c(note = "documents the effort skew (interns ~Mar-Oct, beeple year-round; 2024-heavy) that the inferential tests control for"),
   "survey_effort_by_month.csv; survey_effort_by_month.png; fair_method_2021_2023/effort_by_month_journal.png; fair_method_2021_2023/effort_year_month_grid_journal.png")

fw("nps_summary_tables",
   "NPS descriptive summary tables -- plain counts, no interpretation",
   "descriptive",
   "The bare descriptive tables for the data-focused NPS report: participation (deduped surveyors + public contributors, trips, span), bee totals (genera/species/records), plant totals, methods, and full genus/species checklists -- deliberately counts only, no tests or interpretation.",
   c(scope    = "ALL records (not survey-only); the report's factual backbone that every other analysis interprets",
     contents = "participation (dedicated surveyors + public contributors, deduped from the roster, plus surveys by method + year span), bee + plant genus/species counts, method breakdown, and bee/plant checklists",
     no_stats = "descriptive by design -- no p-values or estimators here; the inferential findings live in the other summaries"),
   "nps_participation.csv; nps_bees_summary.csv; nps_bee_checklist_species.csv; nps_bee_checklist_genus.csv; nps_methods.csv; nps_plants_summary.csv; nps_plant_checklist_genus.csv; nps_summary_tables.{html,png}")

# ---- master index -----------------------------------------------------------
idx <- do.call(rbind, .index)
idx <- idx[order(match(idx$type, c("inferential", "estimator", "descriptive")), idx$analysis), ]
write.csv(idx, file.path(FIND_DIR, "findings_index.csv"), row.names = FALSE)
message(sprintf("Wrote %d per-analysis findings summaries + findings_index.csv to %s",
                nrow(idx), FIND_DIR))

# ---- a note in the folders that hold several different things ----------------
# Runs here because this is the last analysis step, so every findings file it reads
# is already on disk. FOLDER_NOTES is the "which folders get one" decision: a folder
# holding a single analysis already explains itself through its findings CSV.
if (!exists("write_folder_readmes")) source("scripts/analysis/folder_readmes.R")
message(sprintf("Wrote %d folder notes (WHAT_THESE_FILES_ARE.txt)", write_folder_readmes(FIND_DIR)))
