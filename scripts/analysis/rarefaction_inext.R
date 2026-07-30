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
# where install.packages("iNEXT") works. It self-installs iNEXT on first run.
#
# Run from the repo root:  Rscript scripts/analysis/rarefaction_inext.R
# Depends on: dplyr, stringr, iNEXT, ggplot2 (+ config.R).
# =============================================================

for (pkg in c("iNEXT", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
if (!requireNamespace("iNEXT", quietly = TRUE))
  stop("iNEXT is not installed and could not be installed. Run install.packages('iNEXT') then re-run.")
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(iNEXT); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_TRANSECT")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_DIR       <- "data/analysis/rarefaction"
SPECIES_RANKS <- c("species", "subspecies")
TRANSECTS     <- c("BST", "UPMON", "TP", "OT")
WINDOW_MONTHS <- 3:9
QVALS         <- c(0, 1, 2)     # Hill orders
NBOOT         <- 50             # bootstrap reps for CIs (raise to 100+ for final)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
set.seed(1)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"
scope_cap <- function() "Scope: survey records only  |  Method: lethal + non-lethal pooled  |  Rank: species"

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
# house colours onto a ggiNEXT plot (sets both colour + fill, keyed to the group/assemblage)
add_cols <- function(p, cols) if (is.null(cols)) p else
  p + ggplot2::scale_colour_manual(values = cols, name = NULL, aesthetics = c("colour", "fill"))

run_inext <- function(gl, key, title, rank, cols = NULL) {
  if (length(gl) < 2) { message("  ", key, ": <2 groups with data, skipped"); return(invisible()) }
  out <- iNEXT::iNEXT(gl, q = QVALS, datatype = "abundance", nboot = NBOOT)
  sub <- sprintf("Scope: survey records only  |  Method: lethal + non-lethal pooled  |  Rank: %s", rank)
  th  <- theme(plot.title = element_text(face = "bold", colour = BEE_INK$primary),  # house ink on ggiNEXT text
               plot.subtitle = element_text(colour = BEE_INK$note))
  # size-based rarefaction/extrapolation curves (type 1), faceted by Hill order q
  g1 <- add_cols(iNEXT::ggiNEXT(out, type = 1, facet.var = "Order.q") +
    labs(title = sprintf("%s (%s) - iNEXT size-based (q0/q1/q2)", title, rank), subtitle = sub) + th, cols)
  ggsave(file.path(OUT_DIR, paste0(key, "_inext_size.png")), g1, width = 10, height = 4.2, dpi = 200, bg = "white")
  # coverage-based curves (type 3): x-axis = sample completeness, the fair basis
  g3 <- add_cols(iNEXT::ggiNEXT(out, type = 3, facet.var = "Order.q") +
    labs(title = sprintf("%s (%s) - iNEXT coverage-based (q0/q1/q2)", title, rank), subtitle = sub) + th, cols)
  ggsave(file.path(OUT_DIR, paste0(key, "_inext_coverage.png")), g3, width = 10, height = 4.2, dpi = 200, bg = "white")
  # asymptotic diversity estimates (the extrapolated ceiling) + observed
  write.csv(out$AsyEst, file.path(OUT_DIR, paste0(key, "_inext_asymptotic.csv")), row.names = FALSE)
  # standardized to a common COVERAGE (default: the lowest coverage among groups) --
  # this is the fairest apples-to-apples comparison
  estC <- iNEXT::estimateD(gl, q = QVALS, datatype = "abundance", base = "coverage")
  write.csv(estC, file.path(OUT_DIR, paste0(key, "_inext_by_coverage.csv")), row.names = FALSE)
  # ...and standardized to a common SAMPLE SIZE (the lowest group's n), the direct
  # analogue of the vegan "rarefy to lowest" number
  minN <- min(vapply(gl, sum, numeric(1)))
  estS <- iNEXT::estimateD(gl, q = QVALS, datatype = "abundance", base = "size", level = minN)
  write.csv(estS, file.path(OUT_DIR, paste0(key, "_inext_by_size.csv")), row.names = FALSE)
  message(sprintf("  %-11s: iNEXT done (min n = %d). q0 by coverage:\n%s", key, minN,
                  paste(utils::capture.output(print(estC[estC$Order.q == 0,
                        c("Assemblage", "m", "SC", "qD")])), collapse = "\n")))
}

# ---- 3. every comparison at BOTH ranks (genus + species) ---------------------
RANKS   <- c(species = "species_key", genus = "genus_key")
rec_win <- rec %>% filter(month %in% WINDOW_MONTHS, !is.na(year))
message("iNEXT rarefaction/extrapolation:")
for (rk in names(RANKS)) {
  kc <- RANKS[[rk]]; message(sprintf(" %s rank:", rk))
  run_inext(abun_list(filter(rec, transect %in% TRANSECTS), "transect", kc, TRANSECTS),
            paste0("by_transect_", rk), "Bees by transect", rk, cols = BEE_TRANSECT)   # transect palette
  gl_y <- abun_list(rec_win, "year", kc)
  run_inext(gl_y, paste0("by_year_", rk), "Bees by year (Mar-Sep)", rk,
            cols = setNames(grDevices::colorRampPalette(BEE_SEQ)(length(gl_y)), names(gl_y)))   # year -> blue sequential
  run_inext(abun_list(rec, "surveyor", kc, c("beeple", "intern")),
            paste0("by_observer_", rk), "Bees by observer (beeple vs intern)", rk,
            cols = c(intern = "#2166ac", beeple = "#b8b8b8"))   # intern focal blue / beeple grey
  run_inext(abun_list(rec, "obs_type", kc, c("observation", "specimen")),
            paste0("by_method_", rk), "Bees: observations vs specimens", rk,
            cols = c(observation = unname(BEE_METHOD_COL["nonlethal"]), specimen = unname(BEE_METHOD_COL["lethal"])))   # method colours
}

message("Wrote by_{transect,year,observer,method}_{species,genus}_inext_* to ", OUT_DIR)
