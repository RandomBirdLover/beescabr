# =============================================================
# project_info/qc_review_survey_windows.R
# beescabr -- interactive review of SURVEY-DATE windows
# Created 2026-07-16.
#
# The brain (finding_project_info) auto-confirms a beeple survey window when the
# assigned surveyor has a Cabrillo-tagged obs (or an in-CABR untagged obs) inside
# it. Windows it CAN'T auto-confirm -- empty / off-site / excluded / no-username --
# get written to qc_review_survey_beeple_date_windows.csv for a human ruling. This walks them.
#
# Runs LAST in the review chain (after tags + fields), so it sees the fullest
# picture of membership. For each un-ruled window you say whether the survey
# actually happened:
#   y  survey     -- yes, it happened (recorded for review ONLY; NOT added to survey_dates -- no tag = not a survey day)
#   n  no         -- it did not happen -> stays out
#   u  unsure     -- revisit next run
#   s  skip       -- leave blank, revisit next run       q  save & quit    ? help
#
# Decisions persist in qc_review_survey_beeple_date_windows.csv (the `decision` column), so a
# window you've ruled y/n never comes back; only blank/unsure resurface.
#
# This file ALSO holds review_transect_ties() -- for equal-split days where a beeple's
# obs are tagged evenly across two transects (looks like two transects in one day). The
# brain can't pick a majority, so it lists the tag counts and you rule which transect the
# whole day really was (or "both" to keep it a genuine two-transect day). Same persist model.
#
# Run: source("scripts/project_info/review/qc_review_survey_windows.R"); review_windows()   # missing/off-site windows
#      review_transect_ties()                                        # equal-split transect days
# =============================================================

library(dplyr); library(readr)

RW_PATH <- "data/project_info/review/qc_review_survey_beeple_date_windows.csv"

.rw_blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
# a window still needs a ruling if its decision is blank OR "unsure"
.rw_todo  <- function(dec) .rw_blank(dec) | tolower(trimws(as.character(dec))) == "unsure"

.rw_help <- function() {
  bar <- strrep("-", 60)
  cat("\n", bar, "\n", sep = "")
  cat(" Ruling survey windows the brain couldn't auto-confirm:\n\n")
  cat("   y   survey  -- yes it happened (recorded ONLY; NOT added to master_per_survey_info -- no tag = not a survey day)\n")
  cat("   n   no      -- it did not happen\n")
  cat("   u   unsure  -- record as unsure, revisit next run\n")
  cat("   s   skip    -- leave blank, revisit next run\n")
  cat("   l   list    -- show the observation URLs near this window (look before you rule)\n")
  cat("   q   save & quit                                   ?  show this help\n")
  cat(" Each window prints a SUGGESTED ruling: 'SUGGEST NO' (empty / off-site) can usually\n")
  cat(" just be answered n; 'LOOK' = a thin in-CABR cluster worth a glance -- press l first.\n")
  cat(bar, "\n", sep = "")
}

# print the obs URLs stored on a review row (in-CABR ones first), on demand ("l").
.rw_list_urls <- function(rv, i) {
  urls <- if ("obs_urls" %in% names(rv)) rv$obs_urls[i] else NA_character_
  if (.rw_blank(urls)) { cat("   (no observations near this window)\n"); return(invisible()) }
  parts <- trimws(strsplit(as.character(urls), ";\\s*")[[1]]); parts <- parts[nzchar(parts)]
  cat(sprintf("   %d observation(s) near this window (in-CABR first):\n", length(parts)))
  for (u in parts) cat("     ", u, "\n")
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
    if ("suggestion" %in% names(rv) && !.rw_blank(rv$suggestion[i]))
      cat(sprintf("   >> %s\n", rv$suggestion[i]))
    else
      cat(sprintf("   reason: %s   (obs near window: %s)\n", rv$review_reason[i], rv$n_obs_in_window[i]))
    cat("   y=survey  n=no  u=unsure  s=skip  l=list obs URLs  q=save & quit  ?=help\n")

    repeat {
      ans <- tolower(trimws(prompt_fn("> ")))
      if (ans == "?") { .rw_help(); next }
      if (ans == "l") { .rw_list_urls(rv, i); next }
      break
    }
    if (ans == "q") break
    if (ans %in% c("s", "")) next
    dec <- switch(ans, y = "survey", n = "no", u = "unsure", NA_character_)
    if (is.na(dec)) { cat("   ? didn't understand -- skipped\n"); next }
    rv$decision[i] <- dec
    changed <- TRUE
  }

  if (changed && write) {
    write.csv(rv, path, row.names = FALSE, na = "")
    cat(sprintf("\nSaved rulings -> %s  (a review record ONLY -- rulings are NOT added to master_per_survey_info; no tag = not a survey day)\n", path))
  } else cat("\nNo new rulings written.\n")
  invisible(rv)
}

# =============================================================
# review_transect_ties() -- rule equal-split transect days
# -------------------------------------------------------------
# resolve_beeple_transects_per_survey.R (called by the brain) resolves each beeple survey day to the
# transect the MAJORITY of that day's obs are tagged with. When it's an exact tie
# (e.g. TP:3 | UPMON:3 -- looks like two transects walked in one day) it does NOT
# guess; it writes the day to qc_review_survey_transect_overlap.csv with the per-transect
# tag counts. This walks those ties so you can rule each one:
#   <TP|UPMON|...>  the whole day was really this ONE transect -> stamped on every obs,
#                   the other tag's obs go to qc_review_inat_mistagged_transects.csv
#   b  both         a genuine two-transect day -> obs keep their own tags (stays split)
# Your ruling persists in the file's `decision` column and is applied on the next brain
# run; blank/unsure ties resurface, ruled ones don't.
# =============================================================
RTT_PATH <- "data/project_info/review/qc_review_survey_transect_overlap.csv"

# a tie still needs a ruling if its decision is blank OR "unsure"
.rtt_todo <- function(dec) .rw_blank(dec) | tolower(trimws(as.character(dec))) == "unsure"

# "TP:3 | UPMON:3" -> c("TP","UPMON") : the transect codes the surveyor actually tagged
.rtt_options <- function(tag_counts) {
  parts <- trimws(strsplit(as.character(tag_counts), "\\|")[[1]])
  opts  <- toupper(trimws(sub(":.*$", "", parts)))
  opts[nzchar(opts)]
}

.rtt_help <- function() {
  bar <- strrep("-", 60)
  cat("\n", bar, "\n", sep = "")
  cat(" Ruling equal-split transect days (no clear majority tag):\n\n")
  cat("   A beeple's obs for one day are tagged EVENLY across two transects, as if they\n")
  cat("   walked both. The counts show exactly what they tagged. Decide what it really was:\n\n")
  cat("   TP / UPMON / ...  pick the ONE real transect -> the WHOLE day is stamped that;\n")
  cat("                     the other tag's obs -> qc_review_inat_mistagged_transects.csv\n")
  cat("   b  both     -- a genuine two-transect day; keep every obs on its own tag\n")
  cat("   u  unsure   -- revisit next run\n")
  cat("   s  skip     -- leave blank, revisit next run\n")
  cat("   l  list     -- show this day's observation URLs (look before you rule)\n")
  cat("   q  save & quit                                    ?  show this help\n")
  cat(bar, "\n", sep = "")
}

# print the obs URLs stored on a tie row, on demand ("l").
.rtt_list_urls <- function(tv, i) {
  urls <- if ("obs_urls" %in% names(tv)) tv$obs_urls[i] else NA_character_
  if (.rw_blank(urls)) { cat("   (no observation URLs recorded for this day)\n"); return(invisible()) }
  parts <- trimws(strsplit(as.character(urls), ";\\s*")[[1]]); parts <- parts[nzchar(parts)]
  cat(sprintf("   %d observation(s) on this tie day:\n", length(parts)))
  for (u in parts) cat("     ", u, "\n")
}

review_transect_ties <- function(path = RTT_PATH, prompt_fn = readline, write = TRUE, max_items = Inf) {
  if (!file.exists(path)) {
    message("No tie file at ", path, " -- run finding_project_info() first (0 ties = nothing to rule).")
    return(invisible(NULL))
  }
  tv <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  if (!nrow(tv)) { message("No transect ties to review -- every survey day had a clear majority."); return(invisible(tv)) }
  if (!"decision" %in% names(tv))      tv$decision <- NA_character_
  if (!"decision_note" %in% names(tv)) tv$decision_note <- NA_character_

  todo <- which(.rtt_todo(tv$decision))
  done <- nrow(tv) - length(todo)
  if (!length(todo)) { message(sprintf("All %d tie day(s) already ruled -- nothing to do.", nrow(tv))); return(invisible(tv)) }

  message(sprintf("%d equal-split transect day(s) to rule (%d already done).", length(todo), done))
  .rtt_help()
  changed <- FALSE
  n_show <- min(length(todo), max_items)
  for (k in seq_len(n_show)) {
    i <- todo[k]
    opts <- .rtt_options(tv$tag_counts[i])
    cat(sprintf("\n[%d/%d] %s  on %s\n", k, n_show, tv$inat_username[i], tv$date[i]))
    cat("   equal split -- looks like two transects in one day.\n")
    cat(sprintf("   the surveyor tagged their obs:  %s\n", tv$tag_counts[i]))
    cat(sprintf("   pick the ONE real transect (%s), or 'b'=both to keep it a two-transect day\n",
                paste(opts, collapse = "/")))
    cat("   b=both  u=unsure  s=skip  l=list obs URLs  q=save & quit  ?=help\n")

    action <- NULL   # resolves to: __quit__ / __skip__ / unsure / both / a transect code
    repeat {
      ans <- trimws(prompt_fn("> ")); low <- tolower(ans)
      if (low == "?") { .rtt_help(); next }
      if (low == "l") { .rtt_list_urls(tv, i); next }
      if (low == "q")            { action <- "__quit__"; break }
      if (low %in% c("s", ""))   { action <- "__skip__"; break }
      if (low == "u")            { action <- "unsure";   break }
      if (low %in% c("b", "both")) { action <- "both";   break }
      pick <- toupper(ans)
      if (pick %in% opts)        { action <- pick;       break }
      cat(sprintf("   ? '%s' isn't one of %s (or b/u/s/l/q) -- try again\n", ans, paste(opts, collapse = "/")))
    }
    if (identical(action, "__quit__")) break
    if (identical(action, "__skip__")) next
    if (identical(action, "both")) {
      tv$decision[i] <- "both"; tv$decision_note[i] <- "kept as a genuine two-transect day"
    } else if (identical(action, "unsure")) {
      tv$decision[i] <- "unsure"
    } else {
      tv$decision[i] <- action; tv$decision_note[i] <- sprintf("whole day ruled %s", action)
    }
    changed <- TRUE
  }

  if (changed && write) {
    write.csv(tv, path, row.names = FALSE, na = "")
    cat(sprintf("\nSaved rulings -> %s  (re-run the brain to stamp each day with your chosen transect)\n", path))
  } else cat("\nNo new rulings written.\n")
  invisible(tv)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  message('Sourced. Run: review_windows()  (missing windows)  |  review_transect_ties()  (equal-split days)')
