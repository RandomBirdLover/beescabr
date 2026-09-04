# =============================================================
# analysis/shared/rarefaction_names.R
# beescabr -- output names and fair windows for the rarefaction comparisons
# =============================================================

# Output names for the fair-window rarefaction files ----------------------------
#
# "iNEXT" and "vegan" are the names of two R packages. They say which library
# computed a number, which is the one thing a reader of the folder does not need
# to know, and nothing about what the file answers. So the journal filenames name
# the QUESTION (photos vs specimens, beeple vs interns) and the STANDARDIZATION
# (effort-standardized, rarefied to the smallest group) instead.
#
# One helper because three scripts write into that folder -- rarefaction_inext.R,
# rarefaction_vegan.R and rarefaction_combined.R -- and the last time the names
# lived in three places they drifted apart.
#
# Report dimensions (by_transect, by_year) are deliberately NOT renamed: their
# figures are already named to the report convention and are referenced elsewhere.
#
# Pure, no I/O -- tested in tests/testthat/test-rarefaction-names.R.

# grouping column -> what the comparison actually contrasts
# "lethal" / "non-lethal" is the project's word for the method, used by BEE_METHOD_COL,
# the scope captions and the efficiency figures. The rarefaction outputs match it.
RARE_COMPARISON <- c(method = "lethal_vs_nonlethal", observer = "beeple_vs_interns")

# accepts "by_method" or "method"; NULL for a report dimension we do not rename
rare_comparison <- function(dim) {
  key <- sub("^by_", "", dim)
  if (!key %in% names(RARE_COMPARISON)) return(NULL)
  unname(RARE_COMPARISON[[key]])
}

# kind: "figure" / "table" -> one per rank, sharing a stem so they sort together.
#       "estimates"        -> iNEXT's three standardizations, both ranks, one file.
#       "rarefied"         -> vegan's rarefy-to-the-smallest-group numbers, both ranks.
rare_out_name <- function(dim, rank = NULL, kind = c("figure", "table", "estimates", "rarefied")) {
  cmp <- rare_comparison(dim)
  if (is.null(cmp))
    stop("rare_out_name: '", dim, "' is a report dimension and keeps its existing name.", call. = FALSE)
  kinds <- c("figure", "table", "estimates", "rarefied")
  if (length(kind) > 1) kind <- kinds[1]
  if (!kind %in% kinds)
    stop("rare_out_name: kind must be one of ", paste(kinds, collapse = ", "),
         ", not '", kind, "'.", call. = FALSE)
  stem <- paste0("bee_richness_", cmp)
  switch(kind,
    figure    = paste0(stem, "_", rank, "_rarefaction.png"),
    table     = paste0(stem, "_", rank, "_rarefaction.csv"),
    estimates = paste0(stem, "_effort_standardized_estimates.csv"),
    rarefied  = paste0(stem, "_rarefied_to_smallest_group.csv"))
}


# The window each journal comparison runs in -----------------------------------
#
# The window is not a detail, it is what makes a comparison valid or worthless.
#
#   by_method    Mar-Oct 2021-2023, both methods. In those years only interns
#                netted and only beeple photographed, so this contrast is ALSO
#                beeple-vs-interns on the same records. It cannot separate the
#                method from the people. Kept because the method question is the
#                one the fair window was built for; the confound is stated in the
#                findings.
#   by_observer  May-Sep 2024, NON-LETHAL ONLY. Both groups photographed that
#                season, so method is held constant and a difference really is
#                about the observers. This is the clean half of the pair, and the
#                reason it exists is that the 2021-2023 window cannot answer it.
#
# Kept next to the naming so a comparison's folder, its records and its label are
# one edit rather than three. Tested in tests/testthat/test-rarefaction-windows.R.
RARE_WINDOWS <- list(
  by_method = list(
    dir = "fair_method_2021_2023", years = 2021:2023, months = 3:10, methods = NULL,
    # grouped on `method` itself, so the levels ARE BEE_METHOD_COL's keys and no
    # observation/specimen remapping is needed at the call sites
    group = "method", levels = c("nonlethal", "lethal"),
    title = "At equal effort, do lethal or non-lethal surveys find more bees?",
    scope = paste("fair window: survey records only, Mar-Oct 2021-2023, attributed.",
                  "Only interns netted and only beeple photographed in these years,",
                  "so this contrast cannot separate the method from the people."),
    method = "lethal vs non-lethal"),
  by_observer = list(
    dir = "fair_observer_2024", years = 2024, months = 5:9, methods = "nonlethal",
    group = "surveyor", levels = c("beeple", "intern"),
    title = "At equal effort, do beeple or interns find more bees?",
    scope = paste("fair window: survey records only, May-Sep 2024, attributed.",
                  "Both groups photographed this season, so the method is held",
                  "constant and the difference is about the observers."),
    method = "non-lethal only (method held constant)")
)

# NULL for a report dimension (by_transect / by_year), which has no fair window
rare_window <- function(dim) RARE_WINDOWS[[paste0("by_", sub("^by_", "", dim))]]

#' Output subfolder for a comparison's fair window
#'
#' @param dim Comparison dimension, `"method"` or `"observer"`.
#' @return The folder name (`fair_method_2021_2023`, `fair_observer_2024`), or
#'   `NULL` for a dimension that is not restricted to a window.
rare_window_dir <- function(dim) { w <- rare_window(dim); if (is.null(w)) NULL else w$dir }

# `methods = NULL` means keep both; naming a method is how the observer window
# holds it constant.
rare_window_records <- function(rec, dim) {
  w <- rare_window(dim)
  if (is.null(w))
    stop("rare_window_records: '", dim, "' has no window (report dimensions are not ",
         "restricted to a fair window).", call. = FALSE)
  keep <- rec$year %in% w$years & rec$month %in% w$months
  if (!is.null(w$methods)) keep <- keep & rec$method %in% w$methods
  keep <- keep & rec[[w$group]] %in% w$levels
  rec[keep & !is.na(keep), , drop = FALSE]
}


# The figure subtitle -----------------------------------------------------------
# House standard: the subtitle STATES THE RESULT, it does not describe the method.
# Computed from the same rarefied numbers the figure plots, so the sentence cannot
# drift away from the picture under it.
RARE_RANK_WORD <- c(species = "species", genus = "genera")

#' The subtitle sentence for a rarefaction figure
#'
#' Computed from the numbers actually plotted, so the sentence and the figure can
#' never disagree. Printed at one decimal place because `%.0f` rounds half to
#' even and would show a number that differs from the CSV.
#'
#' @param veg Rarefied-richness table for the figure, with `rank`, `group` and
#'   `rarefied_richness` columns.
#' @param dim Comparison dimension, `"method"` or `"observer"`.
#' @return One sentence naming the winning group and the gap, per rank.
rare_takeaway <- function(veg, dim) {
  w <- rare_window(dim)
  say <- function(rk) {
    v <- veg[veg$rank == rk, , drop = FALSE]
    if (nrow(v) < 2) return(NULL)
    v <- v[order(-v$rarefied_richness), ]
    hi <- v$rarefied_richness[1]; lo <- v$rarefied_richness[2]
    # "nonlethal" is a data key; BEE_METHOD_LABEL is how it is spelled for a reader.
    # Guarded because this file is pure and must load without the theme sourced.
    win <- v$group[1]
    if (exists("BEE_METHOD_LABEL") && !is.na(BEE_METHOD_LABEL[win]))
      win <- unname(BEE_METHOD_LABEL[win])
    word <- RARE_RANK_WORD[[rk]]
    if (isTRUE(all.equal(hi, lo)))
      return(sprintf("the two are tied on %s (%.1f each)", word, hi))
    # one decimal: rarefied richness is fractional, and "%.0f" would print 20.5 as 20
    # "lethal surveys find more" reads right; "beeple surveys find more" does not --
    # a method is a way of surveying, a group of people is not
    verb <- if (identical(w$group, "method")) " surveys find more " else " find more "
    sprintf("%s%s%s (%.1f vs %.1f)", win, verb, word, hi, lo)
  }
  parts <- Filter(Negate(is.null), lapply(c("species", "genus"), say))
  if (!length(parts)) return("")
  paste0("At equal sampling effort, ", paste(unlist(parts), collapse = ";\nand "),
         ".\nBoth groups rarefied to the smaller record total, so the comparison is fair.",
         "\nThe open circles are vegan's separate estimate of the same thing; sitting on the curve means the two agree.")
}
