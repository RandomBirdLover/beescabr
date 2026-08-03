# =============================================================
# analysis/findings_summaries.R   (runs LAST -- excluded from the auto-discovered loop)
#
# "Every analysis has a summary." For each analysis in the pipeline this writes a small,
# plain-language FINDINGS summary -- a field/value table (question, method, what it controls
# for, the headline finding, caveats, outputs) -- into data/analysis/findings/, and then a
# master data/analysis/findings/findings_index.csv with one row per analysis (name, type,
# one-line key finding). The point: the write-up lives WITH the data, not only in the README.
#
# Where a headline number is cheap to read live (from an analysis's own output CSV) it is
# pulled in so the summary stays current; everything else is the analysis's documented finding.
# Missing outputs are handled gracefully (the qualitative summary still writes).
#
# Sourced explicitly at the END of run_all_analysis.R (after all analyses have produced their
# outputs). Depends on: base R only.
# =============================================================

FIND_DIR <- "data/analysis/findings"
dir.create(FIND_DIR, recursive = TRUE, showWarnings = FALSE)
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
    field = c("analysis", "type", "key_finding", names(details), "outputs", "data_as_of"),
    value = c(title, type, key_finding, unname(details), outputs, .today),
    stringsAsFactors = FALSE)
  write.csv(df, file.path(FIND_DIR, paste0(name, "_findings.csv")), row.names = FALSE)
  .index[[length(.index) + 1]] <<- data.frame(analysis = title, type = type,
                                              key_finding = key_finding,
                                              findings_file = paste0(name, "_findings.csv"),
                                              stringsAsFactors = FALSE)
}

# ---- live numbers (best-effort) ---------------------------------------------
fs   <- .rd("data/analysis/interactions/forage_selectivity_summary.csv")
h2   <- .rd("data/analysis/interactions/interactions_genus_h2.csv")
acc  <- .rd("data/analysis/accumulation/transect_accumulation_summary.csv")
holw <- .rd("data/analysis/coverage/coverage_cabr_not_on_holway.csv")
yld_m <- .rd("data/analysis/coverage/coverage_yield_by_method.csv")
yld_g <- .rd("data/analysis/coverage/coverage_yield_by_group.csv")
lsb   <- .rd("data/analysis/least_sampled/least_sampled_bees.csv")
.pick <- function(df, g, col) if (is.null(df)) "-" else {
  v <- df[[col]][as.character(df$grp) == g]; if (length(v)) .chr(v[1]) else "-" }

fs_sel <- if (!is.null(fs)) sum(fs$forage_pattern == "Selective") else NA
fs_tot <- .n(fs)
fs_top <- if (!is.null(fs)) { s <- fs[fs$forage_pattern == "Selective", ]
  s <- s[order(-suppressWarnings(as.numeric(s$preferred_vs_available_x))), ]
  paste(utils::head(sprintf("%s -> %s", s$genus, s$preferred_plant), 3), collapse = "; ") } else "-"
h2_sig <- if (!is.null(h2)) sum(suppressWarnings(as.numeric(h2$H2prime_p)) < 0.05, na.rm = TRUE) else NA

# ============================ INFERENTIAL ====================================
fw("forage_selectivity",
   "Forage selectivity -- does a bee genus favour plants beyond availability?",
   "inferential",
   sprintf("%s of %s bee genera show a real plant preference; the set is stable across abundance/month/year/method controls.",
           .chr(fs_sel), .chr(fs_tot)),
   c(question    = "For each bee genus, does it visit plants differently from what was actually available to it?",
     method      = "Matched Monte-Carlo chi-square; availability matched to each genus's own (month, year, survey-method) cells, leave-one-out",
     controls_for   = "overall abundance; month (phenology); year (climate); survey method (net vs photo)",
     not_controlled = "observer (averages out over 10-48 observers/genus); plant detectability (no independent bloom census)",
     top_specialists = fs_top,
     generalists     = "Megachile, Nomada",
     caveats     = "availability = realized community use per cell, not a bloom census; verdicts near p=0.05 borderline"),
   "forage_selectivity_summary.csv; interactions_web_genus.png; interactions_web_species.png; field-guide Forage-preference column")

fw("interactions_genus_species_webs",
   "Within-genus niche partitioning (H2')",
   "inferential",
   sprintf("%s bee genera' species partition plant genera more than their flight-season/method differences explain (Melissodes, Habropoda drop once controlled).",
           .chr(h2_sig)),
   c(question    = "Within a genus, do the species divide up plant genera (niche partitioning) beyond chance?",
     method      = "H2' specialization vs a null that permutes species labels within (month x method) strata",
     controls_for = "flight season (month) and survey method; NOT year (species overlap in years -> would kill power)",
     effect_of_control = "Melissodes and Habropoda drop to non-significant -- their apparent partitioning was seasonal/method timing",
     caveats     = "borderline p-values can jitter; only significant genera are shown in the figures"),
   "interactions_genus_h2.csv; interactions_genus_h2_overview.png; genus_species_webs/*.png")

# ============================ ESTIMATORS =====================================
acc_line <- if (!is.null(acc)) paste(sprintf("%s %s%% complete", acc$transect, acc$species_pct_complete), collapse = "; ") else "-"
fw("genera_and_species_accumulation",
   "Species / genera accumulation & completeness (Chao2)",
   "estimator",
   sprintf("Per-transect sampling completeness (Chao2). %s.", if (is.null(acc)) "run to populate" else "TP/UPMON near-complete; OT least sampled"),
   c(method = "Chao2 asymptotic richness vs observed, per transect + park",
     per_transect_completeness = acc_line,
     assumption = "assumes roughly even sampling; effort is 2024-heavy + seasonal, so treat as approximate"),
   "transect_accumulation_summary.csv; accumulation_species.png; accumulation_genera.png")

fw("rarefaction_inext",
   "Coverage-based rarefaction/extrapolation (iNEXT)",
   "estimator",
   "Sample-coverage-standardized richness comparison (Hill numbers) across groups.",
   c(method = "iNEXT size- and coverage-based rarefaction/extrapolation",
     assumption = "standardizes by coverage but still sensitive to the uneven effort; read CIs as approximate"),
   "*_inext_by_coverage.csv; *_inext_by_size.csv; *_inext_coverage.png; *_inext_size.png")

fw("rarefaction_vegan",
   "Rarefaction (vegan)",
   "estimator",
   "Classic individual-based rarefaction curves as a cross-check on iNEXT.",
   c(method = "vegan rarefaction",
     assumption = "assumes even sampling within a group"),
   "*_vegan.csv; *_vegan_curves.png; *_vegan_bars.png")

fw("diversity_indices",
   "Community diversity (Shannon / Simpson / NMDS)",
   "estimator",
   "Diversity indices and community composition (NMDS) by transect and year.",
   c(method = "Shannon/Simpson/evenness + rank-abundance + NMDS ordination",
     assumption = "diversity indices are effort-sensitive; the 2024-heavy skew inflates apparent year differences"),
   "diversity_by_transect.csv; diversity_by_year.csv; diversity_nmds_composition.png; diversity_rank_abundance.png")

fw("phenology_activity",
   "Seasonal activity phenology (+ Rayleigh test)",
   "estimator",
   "When bees (per genus/species) and flowering plants are active across the year; Rayleigh tests seasonal concentration.",
   c(method = "circular-mean activity ridgelines + Rayleigh test of seasonal concentration",
     confound = "seasonal survey effort (interns ~Mar-Oct) can drive apparent bee seasonality -- read timing, not intensity"),
   "phenology_bee_genus.png; phenology_bee_species.png; phenology_plant_genus.png; *_rayleigh.csv")

# ============================ DESCRIPTIVE ====================================
fw("interactions_network",
   "Plant-bee visitation network (webs + heatmaps)",
   "descriptive",
   "Who visits what: full plant-genus x bee network as webs + heatmaps (raw co-occurrence; read descriptively).",
   c(note = "raw visitation counts -- NOT preference (see forage_selectivity for the inferential preference test)",
     scope = "specimen net + iNaturalist photo pooled, CABR only"),
   "interactions_web_genus.png; interactions_web_species.png; interactions_heatmap_*.png; interactions_*_matrix.csv; interactions_bee_genus_network.png")

fw("interactions_top_plants",
   "Top plants bees visit",
   "descriptive",
   "Ranked most-visited plant genera (whole-park / survey-only / by method + a per-month breakdown).",
   c(note = "counts reflect sampling effort + detectability, not preference; a common easy-to-photograph plant tops the list"),
   "interactions_top_plants.csv; interactions_top_plants.png; interactions_top_plants_by_month.csv/png")

fw("bee_field_guide",
   "Bee field guide -- by species",
   "descriptive",
   "Per-species reference: peak day, active months, most-recorded flowers, diet breadth, status, IUCN.",
   c(note = "'Most-recorded flowers' = where it was seen most, NOT proven preference"),
   "bee_field_guide.html; bee_field_guide.csv; bee_field_guide.png")

fw("bee_field_guide_genus",
   "Bee field guide -- by genus",
   "descriptive (+ inferential Forage-preference column)",
   "Per-genus companion guide; carries the inferential Forage-preference column from the selectivity test.",
   c(note = "Most-recorded flowers/Most-used plant are descriptive; Forage preference is the matched-test result"),
   "bee_field_guide_genus.html; bee_field_guide_genus.csv; bee_field_guide_genus.png")

fw("rare_bee_plants",
   "Plants the park's rare / at-risk bees were recorded on",
   "descriptive",
   "Which plants the rare (< threshold records) and IUCN-threatened bees were RECORDED on -- management-facing. Bars = where sightings fall (NOT a preference: too few records to correct for availability). Threatened bees with >=20 records also get an availability-corrected PREFERRED plant, which can differ from the most-recorded one (e.g. Bombus californicus recorded most on milkvetch but prefers paintbrush; B. sonorus prefers stinkweed).",
   c(recorded_vs_preferred = "bars = recorded-on (availability-blended); PREFERRED (starred) = availability-corrected, shown only where n>=20 -- same matched test as the genus webs",
     note = "low counts: read as 'where the few sightings concentrate', not visit rates or preference",
     threatened_source = "IUCN threatened set read live from the IUCN cache"),
   "rare_bee_plant_hubs.csv/png; rare_named_bee_plants.csv/png")

fw("bee_bounties",
   "Collecting / photo bounties (method gaps)",
   "descriptive",
   "Taxa recorded by one method but not the other -- worklist for what to net (voucher) or photograph next.",
   c(note = "gap = present in one method, absent in the other; directs future effort"),
   "specimen_bee_bounty.csv/png; inaturalist_bee_bounty.csv/png; *_bounty_map.png")

fw("coverage_cabr_vs_holway",
   "CABR bees not on the Holway county checklist",
   "descriptive",
   sprintf("%s CABR taxa are not on the Holway San Diego County checklist (candidate county additions / to verify).",
           .chr(.n(holw))),
   c(note = "iNaturalist records flagged for verification before treating as county additions"),
   "coverage_cabr_not_on_holway.csv/png; coverage_cabr_not_on_holway_inat_records.csv")

fw("coverage_method_venn",
   "Method overlap (net vs photo)",
   "descriptive",
   "How the lethal (net) and non-lethal (photo) methods overlap in the taxa they detect.",
   c(note = "shows each method's unique + shared taxa contribution"),
   "coverage_method_venn.png; coverage_method_resolution.csv; coverage_method_venn_taxa.csv")

fw("coverage_offtransect",
   "Off-transect coverage",
   "descriptive",
   "What the off-transect (opportunistic) records add beyond the fixed transects.",
   character(0),
   "coverage_offtransect_summary.csv; coverage_offtransect_taxa.csv; coverage_offtransect.png")

fw("coverage_id_targets",
   "Identification targets",
   "descriptive",
   "Genus-level records that could still be pushed to species (ID targets), by method.",
   character(0),
   "coverage_id_targets.csv; coverage_id_targets_*.png; coverage_id_completeness.png")

fw("coverage_yield_by_method",
   "Yield by method (lethal vs non-lethal)",
   "descriptive",
   sprintf("Fair footing (survey-only, Mar-Oct, 2021-2023): at SPECIES level lethal netting records more species (%s vs %s) and far more method-exclusive species (%s vs %s) despite fewer records; but at GENUS level the two are even -- non-lethal edges ahead (%s vs %s genera; exclusive %s vs %s). So lethal's species advantage is mostly ID RESOLUTION (specimens key to species, photos stall at genus), not detection.",
           .pick(yld_m, "lethal", "species"),           .pick(yld_m, "nonlethal", "species"),
           .pick(yld_m, "lethal", "exclusive_species"), .pick(yld_m, "nonlethal", "exclusive_species"),
           .pick(yld_m, "lethal", "genera"),            .pick(yld_m, "nonlethal", "genera"),
           .pick(yld_m, "lethal", "exclusive_genera"),  .pick(yld_m, "nonlethal", "exclusive_genera")),
   c(scope    = "SURVEY-ONLY, March-October, 2021-2023 (year-clipped so both methods share the window)",
     groups   = "in this window lethal = intern net specimens; non-lethal = beeple survey photos (strangers and intern iNat photos are NOT included -- see coverage_yield_by_group for the contribution view that adds them)",
     controls_for = "season (Mar-Oct) + year (2021-2023) + survey scope -- removes non-lethal's 3 extra years",
     species_vs_genus = "species-level = detection + ID resolution; genus-level = detection alone (photos not penalised for stalling at genus)",
     takeaway = "specimens win on species-level yield/efficiency; detection (genus) is about even -- the gap is ID resolution, not who finds more bees"),
   "coverage_yield_by_method.csv; coverage_yield_by_method.png; coverage_yield_by_method_genus.png")

fw("coverage_yield_by_group",
   "Yield by surveyor group (interns / beeple / strangers)",
   "descriptive",
   sprintf("Who logged CABR's bees, Mar-Oct 2021-2023 (ALL records incl. casual public): intern net specimens find the most species (%s) and by far the most group-exclusive species (%s), vs beeple survey photos (%s species, %s excl) and casual 'stranger' photos (%s species, %s excl). Netted specimens are the biggest unique contributor.",
           .pick(yld_g, "interns (lethal)", "species"),        .pick(yld_g, "interns (lethal)", "exclusive_species"),
           .pick(yld_g, "beeple (non-lethal)", "species"),     .pick(yld_g, "beeple (non-lethal)", "exclusive_species"),
           .pick(yld_g, "strangers (non-lethal)", "species"),  .pick(yld_g, "strangers (non-lethal)", "exclusive_species")),
   c(scope   = "ALL records incl. casual public (NOT survey-only), Mar-Oct 2021-2023",
     groups  = "interns = lethal net specimens (survey); beeple = non-lethal survey photos; strangers = casual iNaturalist photos by non-surveyor public (not a survey)",
     excludes = "intern iNaturalist photos (none exist in 2021-2023; interns only photographed from 2024)",
     vs_method_figure = "coverage_yield_by_method is SURVEY-ONLY (structured lethal vs non-lethal) and so excludes strangers"),
   "coverage_yield_by_group.csv; coverage_yield_by_group.png; coverage_yield_by_group_genus.png")

fw("coverage_cabr_share_of_county",
   "CABR share of county diversity",
   "descriptive",
   "How much of San Diego County's native-bee diversity the tiny CABR site captures.",
   character(0),
   "cabr_share_of_county.csv; cabr_share_of_county.png; cabr_county_map.png")

fw("records_per_genus_by_evidence",
   "Evidence backing each genus",
   "descriptive",
   "How much (and what kind of) evidence backs each bee genus -- lethal specimen vs non-lethal iNat photo, per genus.",
   character(0),
   "records_per_genus_by_evidence.csv; records_per_genus_by_evidence.png")

fw("records_per_species_by_evidence",
   "Evidence backing each species",
   "descriptive",
   "How much (and what kind of) evidence backs each bee SPECIES -- lethal specimen vs non-lethal iNat photo, one row per species (genus-only records excluded); species with <10 records flagged as thin.",
   character(0),
   "records_per_species_by_evidence.csv; records_per_species_by_evidence.png")

fw("least_sampled_bees",
   "Least-sampled bees -- go-find-it sheet",
   "descriptive",
   sprintf("The %s bee species with <10 records TOTAL across both methods -- under-detected by netting AND iNaturalist -- each with its per-method split and when/where/on-what-flower context. Coverage split: %s.",
           .chr(.n(lsb)),
           if (is.null(lsb)) "run to populate" else paste(sprintf("%d %s", as.integer(table(lsb$coverage)), names(table(lsb$coverage))), collapse = ", ")),
   c(threshold   = "least sampled = < 10 records total (both methods pooled), the project's thin-evidence line",
     coverage    = "both (thin) = a few of each method; photo-only = never netted (also a specimen bounty); specimen-only = never photographed (also an iNat bounty)",
     context     = "when (peak months + active span), where (top transect), flower (top plant genera) pooled across both methods, + an example iNat photo URL",
     vs_bounties = "bee_bounties lists taxa MISSING from one method; this keeps the under-sampled species and adds the find-it context in one sheet"),
   "least_sampled_bees.csv; least_sampled_bees.html; least_sampled_bees.png")

fw("spatial_richness_map",
   "Spatial richness maps",
   "descriptive",
   "Where richness (species/genus, observed + rarefied) and sampling effort concentrate across the park.",
   c(note = "rarefied richness map controls for uneven per-cell effort; raw maps do not"),
   "map_species_richness.png; map_genus_richness.png; map_rarefied_richness.png; map_sampling_effort.png; spatial_richness_grid.csv; transect_richness.csv/png")

fw("phenology_effort",
   "Survey effort by month",
   "descriptive",
   "Survey effort across months and years -- the context for every seasonal/annual pattern in the other analyses.",
   c(note = "documents the effort skew (interns ~Mar-Oct, beeple year-round; 2024-heavy) that the inferential tests control for"),
   "effort_by_month.csv; effort_by_month.png; effort_year_month_grid.png")

# ---- master index -----------------------------------------------------------
idx <- do.call(rbind, .index)
idx <- idx[order(match(idx$type, c("inferential", "estimator", "descriptive")), idx$analysis), ]
write.csv(idx, file.path(FIND_DIR, "findings_index.csv"), row.names = FALSE)
message(sprintf("Wrote %d per-analysis findings summaries + findings_index.csv to %s",
                nrow(idx), FIND_DIR))
