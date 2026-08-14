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
bx_need_reset <- function() .BX_NEED$items <- list()
bx_need <- function(what, where = "") {
  if (!is.null(what) && nzchar(what))
    .BX_NEED$items[[length(.BX_NEED$items) + 1L]] <- c(what = what, where = where)
}
bx_need_print <- function() {
  it <- .BX_NEED$items
  if (!length(it)) { message("  Nothing needs you right now — all clear ✓"); return(invisible()) }
  message("  NEEDS YOU  (all optional — nothing is blocked):")
  w <- max(vapply(it, function(x) nchar(x[["what"]]), 0L))
  for (x in it) message(sprintf("    ▸ %-*s  %s", w, x[["what"]], x[["where"]]))
}
