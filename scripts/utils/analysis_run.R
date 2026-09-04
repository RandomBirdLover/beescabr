# =============================================================
# utils/analysis_run.R
# beescabr -- run one analysis script, attributing its warnings to it.
#
# WHY: run_all_analysis_pipeline.R runs every script best-effort, so one failure does
# not cost you the other 36. The gap that left: a script could keep running and compute
# the WRONG answer. R deferred its warnings, printed "There were 50 or more warnings" at
# the very end with no script name attached, and the tally still read "0 failed".
# That is exactly how a dropped column in least_sampled_bees.R put a wrong iNaturalist
# link on the public site while the run looked clean.
#
# Two things fix that:
#   * attribute warnings to the script that raised them, so the tally names it;
#   * promote the one warning class that is NEVER harmless to a failure.
# =============================================================

# "Unknown or uninitialised column: `x`" means the code asked a data frame for a column
# it does not have. `$` then returns NULL and every downstream test of it silently takes
# the FALSE branch. That is always a bug (a typo, or a select() that dropped the column),
# never a style nit, so it fails the script instead of scrolling past.
ANALYSIS_FATAL_WARNINGS <- c("Unknown or uninitialised column")

# run_analysis_script(): source one script, capturing what it warned about.
# source_fn is injectable so tests never have to write a real script to disk.
# Returns list(ok, warnings, error). Never throws: a failure is a value, not a stop.
run_analysis_script <- function(nm, dir = "scripts/analysis", source_fn = NULL,
                                fatal = ANALYSIS_FATAL_WARNINGS) {
  run <- if (is.null(source_fn)) function() source(file.path(dir, nm)) else function() source_fn(nm)
  warns <- character(0)
  err   <- NA_character_
  ok <- tryCatch({
    withCallingHandlers(
      run(),
      warning = function(w) {
        msg <- conditionMessage(w)
        if (any(vapply(fatal, function(p) grepl(p, msg, fixed = TRUE), logical(1))))
          stop(msg, call. = FALSE)          # promoted: becomes a script failure
        warns <<- c(warns, msg)
        invokeRestart("muffleWarning")      # counted here, so do not defer it to the end
      })
    TRUE
  }, error = function(e) { err <<- conditionMessage(e); FALSE })
  list(ok = ok, warnings = warns, error = err)
}

#' The warnings each script raised, named and quoted
#'
#' The tally names the scripts that warned. That is enough to know something is
#' wrong and not enough to do anything about it -- "6 with warnings" cannot tell a
#' harmless "NAs introduced by coercion" from a dropped column. This prints the
#' messages themselves, under the script that raised them.
#'
#' Identical messages are collapsed with a count: one script can raise the same
#' warning once per row, and 1,400 copies of one line buries the other five.
#'
#' @param names Script names, in the order they ran.
#' @param results The `run_analysis_script()` result for each.
#' @return Character vector of lines to `message()`, empty when nothing warned.
analysis_warning_report <- function(names, results) {
  out <- character(0)
  for (i in seq_along(results)) {
    w <- results[[i]]$warnings
    if (!length(w)) next
    tab <- sort(table(w), decreasing = TRUE)
    out <- c(out, sprintf("  %s", names[i]))
    for (j in seq_along(tab)) {
      n <- as.integer(tab[j])
      out <- c(out, sprintf("      %s%s", names(tab)[j],
                            if (n > 1) sprintf("   (x%d)", n) else ""))
    }
  }
  if (length(out)) c("", "  Warnings, by script:", out) else out
}

# analysis_tally(): the closing line of the run. Names the scripts that failed AND the
# ones that warned, because "somewhere in 37 scripts something warned" is not actionable.
analysis_tally <- function(names, results) {
  failed <- names[!vapply(results, function(r) isTRUE(r$ok), logical(1))]
  warned <- names[vapply(results, function(r) length(r$warnings) > 0, logical(1))]
  msg <- sprintf("Ran %d analysis scripts; %d failed", length(names), length(failed))
  if (length(failed)) msg <- paste0(msg, " (", paste(failed, collapse = ", "), ")")
  if (length(warned))
    msg <- paste0(msg, "; ", length(warned), " with warnings (",
                  paste(warned, collapse = ", "), ")")
  paste0(msg, ".")
}
