# =============================================================
# Rarefaction & extrapolation (iNEXT) -- Hill numbers, size- and coverage-based
# beescabr / Cabrillo National Monument (CABR) native bees
#
# Same three comparisons as rarefaction_vegan.R, but with iNEXT instead of vegan.
# iNEXT is the more complete tool: it rarefies AND extrapolates, reports the whole
# Hill-number family with confidence intervals, and can standardize either to a
# common SAMPLE SIZE or to a common COVERAGE (completeness). See the folder README
# for which of the three approaches to trust.
#
# HILL NUMBERS (q): q0 = species richness (counts every species equally),
#   q1 = exp(Shannon) = "effective # of common species", q2 = inverse Simpson =
#   "effective # of dominant species". Higher q downweights rare species, so it is
#   less sensitive to sampling effort -- useful alongside raw richness.
#
# FOUR COMPARISONS, each at BOTH genus and species rank (survey records only):
#   1. per TRANSECT   2. per YEAR (Mar-Sep)   3. beeple vs intern
#   4. observations vs specimens (non-lethal iNaturalist vs lethal specimens)
#
# NOTE: iNEXT could not be installed in the build sandbox (no CRAN access there),
# so this script was NOT executed here -- it is written to run on your machine,
# where iNEXT is installed. Run scripts/utils/install_requirements.R once per machine.
#
# Run from the repo root:  Rscript scripts/analysis/rarefaction_inext.R
# Depends on: dplyr, stringr, iNEXT, ggplot2 (+ config.R).
# =============================================================

# Dependencies are CHECKED here, not installed: see beescabr_require() in config.R.
if (!exists("beescabr_require")) source("scripts/config.R")
beescabr_require()
if (!requireNamespace("iNEXT", quietly = TRUE))
  stop("iNEXT is not installed. Run: Rscript scripts/utils/install_requirements.R", call. = FALSE)
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(iNEXT); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_TRANSECT")) source("scripts/analysis/theme_beescabr.R")   # shared house style
if (!exists("inext_estimates_tidy")) source("scripts/analysis/inext_estimates.R")
if (!exists("rare_out_name")) source("scripts/analysis/rarefaction_names.R")

# One tidy estimates table per DIMENSION, both ranks stacked, instead of three raw
# iNEXT dumps per rank. run_inext() fills this; section 4 writes it out.
EST_ACC <- new.env(parent = emptyenv())
# iNEXT is JOURNAL-only. Each comparison gets the folder its window declares.
OUT_JOURNAL   <- function(dim) file.path(DIR_JOURNAL, "richness/rarefaction", rare_window_dir(dim))
OUT_REPORT    <- file.path(DIR_REPORT,  "richness/rarefaction")  # (report iNEXT is skipped -- report uses vegan rarefaction)
SPECIES_RANKS <- c("species", "subspecies")
TRANSECTS     <- c("BST", "UPMON", "TP", "OT")
WINDOW_MONTHS <- 3:9
QVALS         <- c(0, 1, 2)     # Hill orders
NBOOT         <- 50             # bootstrap reps for CIs (raise to 100+ for final)
# which paper each rarefaction dimension belongs to
JOURNAL_DIMS  <- names(RARE_WINDOWS)
rare_base <- function(dimdir) if (dimdir %in% JOURNAL_DIMS) OUT_JOURNAL(dimdir) else OUT_REPORT
for (.d in JOURNAL_DIMS) dir.create(OUT_JOURNAL(.d), recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_REPORT,  recursive = TRUE, showWarnings = FALSE)
set.seed(1)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"
# scope_cap(): use the SHARED helper from theme_beescabr.R -- adds Source + data-as-of, one canonical order (no local override).

# ---- 1. survey-only bee records (same prep as the vegan script) --------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
prep <- function(df, method) {
  df %>% filter(is_true(is_survey)) %>%
    transmute(method = method,
      surveyor = ifelse(is.na(surveyor_type) | str_squish(surveyor_type) == "",
                        "unattributed", str_squish(tolower(surveyor_type))),
      transect = toupper(str_squish(transect)),
      year  = suppressWarnings(as.integer(ifelse(!is.na(survey_year) & survey_year != "",
                                                 survey_year, substr(observed_on, 1, 4)))),
      month = suppressWarnings(as.integer(substr(observed_on, 6, 7))),
      taxon_rank, genus, species) %>%
    mutate(obs_type = ifelse(method == "lethal", "specimen", "observation"),
           species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                                  !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
           genus_key   = ifelse(!is.na(genus) & genus != "", genus, NA))
}
rec <- bind_rows(prep(spec, "lethal"), prep(inat, "nonlethal"))

# abundance list (one named integer vector of taxon counts per group) for iNEXT,
# at the chosen rank (key_col = species_key or genus_key)
abun_list <- function(df, group_col, key_col, keep = NULL) {
  d <- df[!is.na(df[[group_col]]) & !is.na(df[[key_col]]), ]
  if (!is.null(keep)) d <- d[d[[group_col]] %in% keep, ]
  t <- table(d[[group_col]], d[[key_col]])
  gl <- setNames(lapply(rownames(t), function(g) as.integer(t[g, ])), rownames(t))
  gl <- gl[vapply(gl, sum, numeric(1)) > 0]
  if (!is.null(keep)) gl <- gl[intersect(keep, names(gl))]
  gl
}

# ---- 2. run iNEXT for one comparison: curves + standardized tables -----------
# house colors onto a ggiNEXT plot (sets both color + fill, keyed to the group/assemblage).
# ggiNEXT ships its OWN default color + fill scales; drop them first so ours REPLACES rather than
# stacks on top (that stacking is what emitted "Adding another scale for color ..." warnings).
add_cols <- function(p, cols) {
  if (is.null(cols)) return(p)
  p$scales$scales <- Filter(function(s) !any(c("colour", "fill") %in% s$aesthetics), p$scales$scales)
  p + ggplot2::scale_colour_manual(values = cols, name = NULL, aesthetics = c("colour", "fill"))
}

run_inext <- function(gl, key, title, rank, cols = NULL) {
  if (length(gl) < 2) { message("  ", key, ": <2 groups with data, skipped"); return(invisible()) }
  # iNEXT is JOURNAL-only: by_method / by_observer. The REPORT (by_transect / by_year) uses the
  # vegan rarefaction curves + bars as its rarefaction figures, so skip iNEXT for those dimensions.
  if (sub(paste0("_", rank, "$"), "", key) %in% c("by_transect", "by_year")) {
    message("  ", key, ": iNEXT skipped (report uses the vegan rarefaction figures)"); return(invisible())
  }
  out <- iNEXT::iNEXT(gl, q = QVALS, datatype = "abundance", nboot = NBOOT)
  # write straight into the accumulation folder (journal); dimension + method baked into the
  # filename, e.g. rarefaction_by_method_species_inext_size.png. No by_<dim>/ subfolders.
  dimdir <- sub(paste0("_", rank, "$"), "", key)   # "by_method_species" -> "by_method"
  outsub <- OUT_JOURNAL(dimdir); dir.create(outsub, recursive = TRUE, showWarnings = FALSE)
  pre    <- paste0("rarefaction_", dimdir, "_inext")   # rank appended LAST, e.g. rarefaction_by_method_inext_size_species
  sub <- scope_cap(scope = "survey records only", method = "lethal + non-lethal pooled", rank = rank)
  th  <- theme(plot.title = element_text(face = "bold", colour = BEE_INK$primary, hjust = 0.5),  # house ink, centred
               plot.subtitle = element_text(colour = BEE_INK$note, hjust = 0.5))
  # size-based rarefaction/extrapolation curves (type 1), faceted by Hill order q
  g1 <- add_cols(iNEXT::ggiNEXT(out, type = 1, facet.var = "Order.q") +
    labs(title = title, subtitle = "Effort-standardized richness (Hill numbers) -- a fair diversity comparison across methods/observers.",
         x = "records sampled (sample size)", y = "diversity (Hill numbers)",
         caption = scope_cap(scope = "survey records only", method = "lethal + non-lethal pooled", rank = rank,
                             sig = bee_test("iNEXT size-based rarefaction/extrapolation", "Hill q0/q1/q2"))) + th, cols)
  # SUPERSEDED by rarefaction_combined.R (one figure per comparison, both estimators): 
  # bee_ggsave(file.path(outsub, paste0(pre, "_size_", rank, ".png")), g1, width = 10, height = 4.2, bg = "white")
  # coverage-based curves (type 3): x-axis = sample completeness, the fair basis
  g3 <- add_cols(iNEXT::ggiNEXT(out, type = 3, facet.var = "Order.q") +
    labs(title = title, subtitle = "Effort-standardized richness (Hill numbers) -- a fair diversity comparison across methods/observers.",
         x = "sample completeness (coverage)", y = "diversity (Hill numbers)",
         caption = scope_cap(scope = "survey records only", method = "lethal + non-lethal pooled", rank = rank,
                             sig = bee_test("iNEXT coverage-based rarefaction/extrapolation", "Hill q0/q1/q2"))) + th, cols)
  # SUPERSEDED by rarefaction_combined.R (one figure per comparison, both estimators): 
  # bee_ggsave(file.path(outsub, paste0(pre, "_coverage_", rank, ".png")), g3, width = 10, height = 4.2, bg = "white")
  # the CURVES themselves, so rarefaction_combined.R can draw both estimators on one
  # pair of axes instead of leaving eight separate figures for one comparison.
  .cv <- out$iNextEst
  if (!is.null(.cv$size_based))     write.csv(.cv$size_based,     file.path(outsub, paste0(pre, "_curve_size_", rank, ".csv")),     row.names = FALSE)
  if (!is.null(.cv$coverage_based)) write.csv(.cv$coverage_based, file.path(outsub, paste0(pre, "_curve_coverage_", rank, ".csv")), row.names = FALSE)
  # Three standardizations of one comparison: the asymptotic ceiling, a common
  # COVERAGE (the fairest apples-to-apples), and a common SAMPLE SIZE (the direct
  # analogue of the vegan "rarefy to lowest" number). Same grain -- assemblage x q --
  # so they accumulate into ONE table per dimension rather than three files per rank.
  estC <- iNEXT::estimateD(gl, q = QVALS, datatype = "abundance", base = "coverage")
  minN <- min(vapply(gl, sum, numeric(1)))
  estS <- iNEXT::estimateD(gl, q = QVALS, datatype = "abundance", base = "size", level = minN)
  EST_ACC[[dimdir]] <- rbind(EST_ACC[[dimdir]], inext_estimates_tidy(out$AsyEst, estS, estC, rank))
  message(sprintf("  %-11s: iNEXT done (min n = %d). q0 by coverage:\n%s", key, minN,
                  paste(utils::capture.output(print(estC[estC$Order.q == 0,
                        c("Assemblage", "m", "SC", "qD")])), collapse = "\n")))
}

# ---- 3. every comparison at BOTH ranks (genus + species) ---------------------
RANKS   <- c(species = "species_key", genus = "genus_key")
rec_win  <- rec %>% filter(month %in% WINDOW_MONTHS, !is.na(year))
rec_fair <- rec %>% filter(month %in% 3:10, year %in% 2021:2023)   # FAIR WINDOW for the lethal-vs-non-lethal comparisons (matches yield_by_method/efficiency/Venn; drops 2024 intern photos)
message("iNEXT rarefaction/extrapolation:")
for (rk in names(RANKS)) {
  kc <- RANKS[[rk]]; message(sprintf(" %s rank:", rk))
  run_inext(abun_list(filter(rec, transect %in% TRANSECTS), "transect", kc, TRANSECTS),
            paste0("by_transect_", rk), "Do some transects have more richness of bees than others?", rk, cols = BEE_TRANSECT)   # transect palette
  gl_y <- abun_list(rec_win, "year", kc)
  run_inext(gl_y, paste0("by_year_", rk), "Did some years have more richness of bees than others?", rk,
            cols = setNames(grDevices::colorRampPalette(BEE_SEQ)(length(gl_y)), names(gl_y)))   # year -> blue sequential
  # by_observer runs in its OWN window (May-Sep 2024, non-lethal only), where both
  # groups photographed. Running it on rec_fair produced a duplicate of by_method:
  # in 2021-2023 only interns netted and only beeple photographed, so that was the
  # same split with different labels. See RARE_WINDOWS in rarefaction_names.R.
  w_obs <- rare_window_records(rec, "by_observer")
  run_inext(abun_list(w_obs, "surveyor", kc, rare_window("by_observer")$levels),
            paste0("by_observer_", rk), rare_window("by_observer")$title, rk,
            cols = BEE_OBSERVER_COL)   # declared in theme_beescabr.R: observer contrast, teal family, never the method colors
  w_met <- rare_window_records(rec, "by_method")
  run_inext(abun_list(w_met, rare_window("by_method")$group, kc, rare_window("by_method")$levels),
            paste0("by_method_", rk), rare_window("by_method")$title, rk,
            cols = BEE_METHOD_COL)   # keys are lethal / nonlethal, matching the window levels
}

# ---- 4. one estimates table per comparison (both ranks, all three bases) -----
for (dimdir in ls(EST_ACC)) {
  f <- file.path(OUT_JOURNAL(dimdir), rare_out_name(dimdir, kind = "estimates"))
  write.csv(EST_ACC[[dimdir]], f, row.names = FALSE)
  message(sprintf("  %-11s: %d estimate rows -> %s", dimdir, nrow(EST_ACC[[dimdir]]), basename(f)))
}
message("Wrote the effort-standardized estimates into each comparison's window folder under journal richness/rarefaction/ (iNEXT is journal-only)")
