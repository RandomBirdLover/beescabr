# =============================================================
# utils/console.R -- the pipeline's console reporter (OUTPUT ONLY, no logic).
# One consistent look for run_data_cleaning_pipeline.R and every stage: phase banners, aligned
# key/value detail lines, calm notes, output arrows, "»" input markers, and an
# end-of-run "NEEDS YOU" rollup. UTF-8 glyphs (macOS / RStudio console).
#
# Sourced early by run_data_cleaning_pipeline.R; also safe to source standalone. Stage scripts
# call bx_kv / bx_cont / bx_out / bx_note for their detail lines; run_data_cleaning_pipeline.R
# owns the phase banners (bx_phase) and the closing rollup (bx_need_*).
# =============================================================
BX_WIDTH <- 62L                                   # banner rule width
BX_LABEL <- 12L                                   # key/value label column

# ---- structure ----
bx_rule  <- function() message(strrep("═", BX_WIDTH))                       # ══════
bx_title <- function(t) { bx_rule(); message("  ", t); bx_rule() }
bx_phase <- function(n, title) {                                                 # ━━ n · TITLE ━━
  head <- sprintf("━━ %s · %s ", n, title)
  message("\n", head, strrep("━", max(0L, BX_WIDTH - nchar(head))))
}

# ---- detail lines ----
bx_kv   <- function(label, ...) message(sprintf("  %-*s %s", BX_LABEL, label, paste0(...)))  # Label   detail
bx_cont <- function(...)        message(sprintf("  %-*s %s", BX_LABEL, "", paste0(...)))      # (aligned continuation)
bx_out  <- function(...)        message("  → ", paste0(...))                # → output file
bx_note <- function(...)        message("  note: ", paste0(...))                 # calm FYI
bx_act  <- function(...)        message("  » ", paste0(...))                # » wants your input

# ---- end-of-run "NEEDS YOU" rollup ----
# Stages don't touch this; run_data_cleaning_pipeline.R collects the items after the run (from
# the review artifacts on disk) and prints them once at the very end.
.BX_NEED <- new.env(parent = emptyenv()); .BX_NEED$items <- list()
#' Empty the "NEEDS YOU" queue
#'
#' Called at the start of a run so a second run in the same session does not
#' inherit the first run's items.
#'
#' @return Invisibly, nothing.
bx_need_reset <- function() .BX_NEED$items <- list()
#' Report a stage that FAILED, in a voice a note cannot be confused with
#'
#' bx_note() is for things that are fine ("CABR reaches past the County line --
#' expected"). It was also carrying real failures: the bee taxonomy lookup died
#' mid-build, said "note: taxonomy lookup failed: ...", and every later stage
#' joined against a lookup four days old while the run finished with a tick.
#'
#' @param stage What was being built, in words ("bee taxonomy lookup").
#' @param cause The error message.
#' @param stale The file that was therefore NOT rebuilt, if any.
#' @return Invisibly, nothing. Also queues a NEEDS YOU item.
bx_fail <- function(stage, cause, stale = NULL) {
  message("")
  message("  \u2717 FAILED: ", stage)
  message("      ", cause)
  if (!is.null(stale) && nzchar(stale)) {
    message("      This did NOT rebuild, so the rest of the run used the old file:")
    message("        ", stale)
    bx_need(sprintf("%s FAILED, re-run after fixing", stage), stale)
  } else {
    bx_need(sprintf("%s FAILED", stage), "")
  }
  invisible(NULL)
}

#' Queue an item for the end-of-run "NEEDS YOU" rollup
#'
#' Nothing here blocks the pipeline. It is how a stage says "a human should look
#' at this eventually" without interrupting a run that is otherwise fine.
#'
#' @param what What the person has to do.
#' @param where The file or place to do it in.
#' @return Invisibly, nothing.
bx_need <- function(what, where = "") {
  if (!is.null(what) && nzchar(what))
    .BX_NEED$items[[length(.BX_NEED$items) + 1L]] <- c(what = what, where = where)
}
#' The rollup line for specimens with no identification yet
#'
#' Kept apart from the iNaturalist-id item on purpose. These are physical specimens in
#' a drawer that nobody has put a name to; the other is a checklist name with no
#' iNaturalist number. Different job, different place, so one line each.
#'
#' It was missing from the rollup entirely: 186 specimens, the largest outstanding
#' task, mentioned once mid-run in a note that said "the raw .xlsx" (there are 19 of
#' them) and referred the reader to a TODO in a code comment.
#'
#' @param n How many specimens need identifying.
#' @return A `bx_need()` item, or NULL when there are none.
needs_specimen_ids <- function(n) {
  if (!length(n) || is.na(n) || n <= 0L) return(NULL)
  c(what  = sprintf("%d specimen%s need%s identifying", n,
                    if (n == 1L) "" else "s", if (n == 1L) "s" else ""),
    where = "data/specimens/specimens_clean/review/qc_review_specimen_cleanup_worklist_generated.csv")
}

#' Lay out the "NEEDS YOU" rollup
#'
#' Every item carries a full folder path, because a bare filename is not something
#' the operator can open. The longest is 78 characters, so padding them all into one
#' aligned column runs well past the banner and wraps wherever the terminal happens
#' to end. An item that does not fit puts its path on its own indented line instead.
#'
#' @param items List of `c(what=, where=)` vectors, as `bx_need()` stores them.
#' @param width Console width to fit within.
#' @return A character vector of lines, ready to print.
need_lines <- function(items, width = BX_WIDTH) {
  if (!length(items)) return(character(0))
  w <- max(vapply(items, function(x) nchar(x[["what"]]), 0L))
  unlist(lapply(items, function(x) {
    what <- x[["what"]]; where <- x[["where"]]
    if (!nzchar(where)) return(sprintf("    ▸ %s", what))
    one <- sprintf("    ▸ %-*s  %s", w, what, where)
    # 6 = the "    ▸ " prefix; a path that still overflows on its own line is
    # left whole rather than broken -- a half path cannot be pasted.
    if (nchar(one) <= width) one else c(sprintf("    ▸ %s", what), sprintf("        %s", where))
  }), use.names = FALSE)
}

#' Print the "NEEDS YOU" rollup at the end of a run
#'
#' @return Invisibly, nothing. Prints an all-clear line when nothing is queued.
bx_need_print <- function() {
  it <- .BX_NEED$items
  if (!length(it)) { message("  Nothing needs you right now — all clear ✓"); return(invisible()) }
  message("  NEEDS YOU  (all optional — nothing is blocked):")
  for (ln in need_lines(it)) message(ln)
}
