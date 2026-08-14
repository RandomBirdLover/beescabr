# =============================================================
# project_info/review_notes.R
# beescabr -- interactive review of free-text observation NOTES
# Created 2026-07-16.
#
# OPTIONAL (opt-in) -- reviewing free-text notes is offered as a y/N prompt in
# run_data_cleaning_pipeline.R stage 3b: answer 'y' and it sources this file and calls review_notes();
# otherwise notes are left untouched (see scripts/project_info/free_text_notes_status.md).
# Triaging free text is a human job.
#
# Why notes get their OWN reviewer (not review_crosswalk.R):
#   Tags and obs-FIELDS are reusable -- once you file "windspeed" as metadata,
#   every future obs carrying that field is known. NOTES are free text, unique
#   per observation, so a decision can't be filed as a reusable "variant".
#   Instead each decision is remembered PER obs_id in notes_reviewed.csv, so a
#   note you've already judged never comes back.
#
# It shows each un-reviewed note one at a time, PRE-FILLED with a keyword guess:
#   1 survey      the note describes / confirms survey activity (a real survey obs)
#   2 metadata    survey conditions logged on the obs (weather / wind / time / sky)
#   3 not-survey  incidental or personal (garden, "on the walk to...", off-site)
#   <Enter>  accept the highlighted (*) guess      u  unsure -> park as "ambiguous"
#   s        skip (stays un-reviewed, returns)      q  save & quit      ?  help
#
# Input:   data/project_info/review/review_inat_unknown_notes.csv   (brain output)
# Output:  data/project_info/review/notes_reviewed.csv                (persistent, per obs_id)
#   Non-interactive preview only:
#          data/project_info/review/notes_auto_suggestions.csv        (guesses, un-reviewed)
#
# Run: source("scripts/project_info/review_notes.R"); review_notes()
#      BEESCABR_NONINTERACTIVE=1 Rscript -e 'source(...); review_notes()'  # preview guesses
# =============================================================

library(dplyr); library(readr); library(stringr)

NOTES_IN       <- "data/project_info/review/review_inat_unknown_notes.csv"
NOTES_REVIEWED <- "data/project_info/review/notes_reviewed.csv"
NOTES_SUGGEST  <- "data/project_info/review/notes_auto_suggestions.csv"

# The categories a note can be filed under. 1/2/3 are what you type; a note the
# guesser can't place (or you mark 'u') lands in "ambiguous".
NOTE_CATEGORIES <- c("survey", "metadata", "not-survey")

# ------------------------------------------------------------
# Auto-suggest: keyword buckets ported from the notes analysis. IMPERFECT ON
# PURPOSE -- these only pre-fill a guess you confirm. The "metadata" bucket in
# particular over-fires on weather words ("wind") that also appear in personal
# notes, which is exactly why a human confirms each one.
# ------------------------------------------------------------
note_suggest <- function(note) {
  low <- tolower(ifelse(is.na(note), "", note))
  survey <- str_detect(low, "cabrillobee10minutesurvey|10[\\s-]?min(?:ute)?s?\\s*(?:focused |native bee )*survey|(?:native )?bee survey|cabrillo\\w*\\s*survey")
  meta   <- str_detect(low, "meta ?data|start[: ].*\\d|end[: ].*\\d|\\bwind\\b|cloud cover|\\bsky\\b|\\btemp\\b|\\d{1,3} ?%|\\d{2,4} ?(?:am|pm)|\\d{2,3} ?f\\b")
  incid  <- str_detect(low, "watched.*\\bfor\\b.*\\d+ ?min|perched.*\\d+ ?min|for (?:about )?\\d+ ?min|native garden|my (?:garden|yard|backyard)|on (?:the )?(?:road|way|walk|trail) to")
  case_when(survey ~ "survey", meta ~ "metadata", incid ~ "not-survey", TRUE ~ "ambiguous")
}

# On-demand ("?") help block, in plain language.
.notes_help <- function() {
  bar <- strrep("-", 60)
  cat("\n", bar, "\n", sep = "")
  cat(" Reviewing flagged NOTES -- for each, tell me what it is:\n\n")
  cat("   Enter    accept the highlighted (*) guess\n")
  cat("   1        survey      -- note describes / confirms survey activity\n")
  cat("   2        metadata    -- survey conditions (weather / wind / time / sky)\n")
  cat("   3        not-survey  -- incidental / personal / off-site\n")
  cat("   u        unsure      -- park as 'ambiguous' (edit the csv later to resolve)\n")
  cat("   s        skip (stays un-reviewed, comes back next run)\n")
  cat("   q        save & quit                       ?  show this help\n")
  cat(bar, "\n", sep = "")
}

# Read prior human decisions (persistence). Missing file -> empty frame.
.read_reviewed <- function(path) {
  if (file.exists(path))
    read_csv(path, show_col_types = FALSE, col_types = cols(obs_id = "c", .default = "c"))
  else
    tibble(obs_id = character(), observer = character(), observed_on = character(),
           url = character(), category = character(), note = character())
}

review_notes <- function(notes_in = NOTES_IN, reviewed_path = NOTES_REVIEWED,
                         suggest_path = NOTES_SUGGEST,
                         interactive_ok = (Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"),
                         prompt_fn = readline, write = TRUE, max_items = Inf) {
  if (!file.exists(notes_in)) {
    message("No notes file at ", notes_in, " -- run finding_project_info() first.")
    return(invisible(NULL))
  }
  notes <- read_csv(notes_in, show_col_types = FALSE, col_types = cols(obs_id = "c", .default = "c"))
  if (!nrow(notes)) { message("No flagged notes to review."); return(invisible(NULL)) }

  # persistence: anything already in notes_reviewed.csv (incl. 'ambiguous') is
  # done and won't come back. To re-judge one, edit that row in the csv.
  prior <- .read_reviewed(reviewed_path)
  todo  <- notes |>
    filter(!(obs_id %in% prior$obs_id)) |>
    mutate(suggestion = note_suggest(note))

  # --- non-interactive: write the guesses to a SEPARATE preview file and stop.
  # (Never touches notes_reviewed.csv, so a guess is never mistaken for a human
  # decision.)
  if (!interactive_ok) {
    preview <- todo |> select(obs_id, observer, observed_on, url, suggestion, note)
    if (write && nrow(preview)) {
      dir.create(dirname(suggest_path), recursive = TRUE, showWarnings = FALSE)
      write.csv(preview, suggest_path, row.names = FALSE, na = "")
    }
    message(sprintf("Non-interactive: %d un-reviewed note(s), guesses -> %s", nrow(preview), suggest_path))
    print(as.data.frame(count(todo, suggestion, name = "n")))
    return(invisible(preview))
  }

  if (!nrow(todo)) {
    message(sprintf("All %d flagged note(s) already reviewed -- nothing new.", nrow(notes)))
    return(invisible(prior))
  }

  message(sprintf("%d note(s) to review (%d already done).", nrow(todo), nrow(prior)))
  .notes_help()
  decisions <- prior
  n_show <- min(nrow(todo), max_items)
  for (k in seq_len(n_show)) {
    row  <- todo[k, ]
    sugg <- row$suggestion
    si   <- match(sugg, NOTE_CATEGORIES)                 # NA when guess is "ambiguous"

    cat(sprintf("\n[%d/%d] %s  (%s)  %s\n", k, n_show,
                row$observer, row$observed_on, row$url))
    cat(sprintf("   note: \"%s\"\n", row$note))
    labs <- vapply(seq_along(NOTE_CATEGORIES), function(i)
      sprintf("%d=%s%s", i, NOTE_CATEGORIES[i], if (!is.na(si) && si == i) "*" else ""),
      character(1))
    cat("   ", paste(labs, collapse = "   "), sprintf("   (guess: %s)\n", sugg))
    cat("   <Enter>=accept*   u=unsure   s=skip   q=save & quit   ?=help\n")

    repeat { ans <- trimws(prompt_fn("> ")); low <- tolower(ans); if (low != "?") break; .notes_help() }
    if (low == "q") break
    if (low == "s") next
    choice <- if (ans == "")               sugg            # accept guess (may be "ambiguous")
              else if (low == "u")         "ambiguous"
              else if (ans %in% c("1","2","3")) NOTE_CATEGORIES[as.integer(ans)]
              else { cat("   ? didn't understand -- skipped\n"); next }

    decisions <- bind_rows(
      decisions,
      tibble(obs_id = row$obs_id, observer = row$observer, observed_on = row$observed_on,
             url = row$url, category = choice, note = row$note)
    ) |> distinct(obs_id, .keep_all = TRUE)
  }

  if (write && nrow(decisions) > nrow(prior)) {
    dir.create(dirname(reviewed_path), recursive = TRUE, showWarnings = FALSE)
    write.csv(decisions, reviewed_path, row.names = FALSE, na = "")
    cat(sprintf("\nSaved %d decision(s) (+%d new) -> %s\n",
                nrow(decisions), nrow(decisions) - nrow(prior), reviewed_path))
  } else cat("\nNo new decisions written.\n")
  invisible(decisions)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  message('Sourced. Run: review_notes()   (preview: BEESCABR_NONINTERACTIVE=1)')
