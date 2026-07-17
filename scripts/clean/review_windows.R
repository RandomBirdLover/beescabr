# =============================================================
# clean/review_windows.R
# beescabr -- interactive review of SURVEY-DATE windows
# Created 2026-07-16.
#
# The brain (finding_project_info) auto-confirms a beeple survey window when the
# assigned surveyor has a Cabrillo-tagged obs (or an in-CABR untagged obs) inside
# it. Windows it CAN'T auto-confirm -- empty / off-site / excluded / no-username --
# get written to survey_windows_to_review.csv for a human ruling. This walks them.
#
# Runs LAST in the review chain (after tags + fields), so it sees the fullest
# picture of membership. For each un-ruled window you say whether the survey
# actually happened:
#   y  survey     -- yes, it happened -> the brain promotes it into survey_dates.csv
#   n  no         -- it did not happen -> stays out
#   u  unsure     -- revisit next run
#   s  skip       -- leave blank, revisit next run       q  save & quit    ? help
#
# Decisions persist in survey_windows_to_review.csv (the `decision` column), so a
# window you've ruled y/n never comes back; only blank/unsure resurface.
#
# Run: source("scripts/clean/review_windows.R"); review_windows()
# =============================================================

library(dplyr); library(readr)

RW_PATH <- "data/project_info/survey_windows_to_review.csv"

.rw_blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
# a window still needs a ruling if its decision is blank OR "unsure"
.rw_todo  <- function(dec) .rw_blank(dec) | tolower(trimws(as.character(dec))) == "unsure"

.rw_help <- function() {
  bar <- strrep("-", 60)
  cat("\n", bar, "\n", sep = "")
  cat(" Ruling survey windows the brain couldn't auto-confirm:\n\n")
  cat("   y   survey  -- yes it happened (brain folds it into survey_dates.csv)\n")
  cat("   n   no      -- it did not happen\n")
  cat("   u   unsure  -- record as unsure, revisit next run\n")
  cat("   s   skip    -- leave blank, revisit next run\n")
  cat("   q   save & quit                                   ?  show this help\n")
  cat(" reason legend: empty=no obs near the window, off-site=obs but none in CABR,\n")
  cat("                too-few-obs=in-CABR obs but below the survey threshold, no-username=no iNat user\n")
  cat(bar, "\n", sep = "")
}

review_windows <- function(path = RW_PATH, prompt_fn = readline, write = TRUE, max_items = Inf) {
  if (!file.exists(path)) {
    message("No review file at ", path, " -- run finding_project_info() first.")
    return(invisible(NULL))
  }
  rv <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  if (!nrow(rv)) { message("No windows to review."); return(invisible(rv)) }
  if (!"decision" %in% names(rv))      rv$decision <- NA_character_
  if (!"decision_note" %in% names(rv)) rv$decision_note <- NA_character_

  todo <- which(.rw_todo(rv$decision))
  done <- nrow(rv) - length(todo)
  if (!length(todo)) { message(sprintf("All %d windows already ruled -- nothing to do.", nrow(rv))); return(invisible(rv)) }

  message(sprintf("%d window(s) to rule (%d already done).", length(todo), done))
  .rw_help()
  changed <- FALSE
  n_show <- min(length(todo), max_items)
  for (k in seq_len(n_show)) {
    i <- todo[k]
    cat(sprintf("\n[%d/%d] %s (%s)\n", k, n_show,
                rv$first_name[i], if (.rw_blank(rv$inat_username[i])) "no iNat user" else rv$inat_username[i]))
    cat(sprintf("   %s -> %s  |  transect %s  |  year %s\n",
                rv$window_start[i], rv$window_end[i], rv$transect[i], rv$year[i]))
    cat(sprintf("   reason: %s   (obs found in window: %s)\n",
                rv$review_reason[i], rv$n_obs_in_window[i]))
    cat("   y=survey  n=no  u=unsure  s=skip  q=save & quit  ?=help\n")

    repeat { ans <- tolower(trimws(prompt_fn("> "))); if (ans != "?") break; .rw_help() }
    if (ans == "q") break
    if (ans %in% c("s", "")) next
    dec <- switch(ans, y = "survey", n = "no", u = "unsure", NA_character_)
    if (is.na(dec)) { cat("   ? didn't understand -- skipped\n"); next }
    rv$decision[i] <- dec
    changed <- TRUE
  }

  if (changed && write) {
    write.csv(rv, path, row.names = FALSE, na = "")
    cat(sprintf("\nSaved rulings -> %s  (re-run the brain to fold 'survey' windows into survey_dates.csv)\n", path))
  } else cat("\nNo new rulings written.\n")
  invisible(rv)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  message('Sourced. Run: review_windows()')
