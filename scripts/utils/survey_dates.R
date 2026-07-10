# survey_dates.R
#
# Purpose: infer beeple survey dates from iNat obs within calendar windows,
# then build permanent official survey date records for beeple and interns.
#
# Inputs:
#   data/project_info/beeple_calendar_windows.csv  -- parsed from annual calendar PDFs
#       by scripts/utils/parse_beeple_calendars.py
#   data/project_info/intern_survey_dates.csv      -- exact intern dates, all years
#   data/project_info/surveyors_by_year.csv        -- name/username/role roster
#   data/outputs/inat_clean/cabr_inat_bee_clean.csv
#       (run scripts/clean/inat_bee_clean.R first)
#
# Outputs:
#   data/project_info/beeple_survey_dates_official.csv  -- PERMANENT beeple record
#       One row per calendar window. Transect columns hold iNat usernames.
#       Rows you mark "manual" or "skipped" are NEVER overwritten on re-runs.
#   data/project_info/intern_survey_dates_official.csv  -- PERMANENT intern record
#       One row per survey date. Full names + iNat usernames. Transects in order.
#   data/project_info/survey_dates_needs_review.csv     -- windows needing your attention
#       Filtered view of ambiguous/no_obs beeple windows. See instructions below.
#
# Run from the beescabr project root:
#   source("scripts/utils/survey_dates.R")

library(tidyverse)
library(lubridate)

# ---- config ----------------------------------------------------------------

CALENDAR_FILE    <- "data/project_info/beeple_calendar_windows.csv"
INTERN_DATES     <- "data/project_info/intern_survey_dates.csv"
ROSTER_FILE      <- "data/project_info/surveyors_by_year.csv"
INAT_CLEAN       <- "data/outputs/inat_clean/cabr_inat_bee_clean.csv"
BEEPLE_OFFICIAL  <- "data/project_info/beeple_survey_dates_official.csv"
INTERN_OFFICIAL  <- "data/project_info/intern_survey_dates_official.csv"
NEEDS_REVIEW     <- "data/project_info/survey_dates_needs_review.csv"

# Transect display order in beeple_survey_dates_official.csv.
# Add new transect names here if a future year introduces them.
TRANSECT_ORDER <- c("OT", "TP1", "TP2", "UPMON", "BST")

# First-name aliases: calendar typos / nicknames -> canonical first_name in roster.
# Add entries here if a future calendar uses a variant spelling.
NAME_ALIASES <- c()

# ============================================================
# PART 1: Infer beeple survey dates from iNat obs
# ============================================================

if (!file.exists(INAT_CLEAN)) {
  stop(
    "cabr_inat_bee_clean.csv not found. Run scripts/clean/inat_bee_clean.R first.\n",
    "  Expected at: ", INAT_CLEAN
  )
}

windows <- read_csv(CALENDAR_FILE, show_col_types = FALSE) |>
  mutate(
    window_start = as.Date(window_start),
    window_end   = as.Date(window_end),
    year         = as.integer(year),
    first_name   = if_else(first_name %in% names(NAME_ALIASES),
                           NAME_ALIASES[first_name],
                           first_name)
  )

beeple_roster <- read_csv(ROSTER_FILE, show_col_types = FALSE) |>
  filter(role == "beeple") |>
  select(year, username = inaturalist_username, first_name) |>
  mutate(year = as.integer(year))

obs <- read_csv(INAT_CLEAN, show_col_types = FALSE) |>
  filter(data_source == "beeple",
         triage == "keep") |>   # survey-tagged obs only; excludes personal/fun visits
  select(username = observer, observed_on = observed_date) |>
  mutate(observed_on = as.Date(observed_on)) |>
  distinct()

windows_with_user <- windows |>
  left_join(beeple_roster, by = c("year", "first_name"))

no_username <- windows_with_user |> filter(is.na(username))
if (nrow(no_username) > 0) {
  message(sprintf("WARNING: %d window row(s) have no matching username in the roster:",
                  nrow(no_username)))
  no_username |>
    distinct(year, first_name) |>
    arrange(year, first_name) |>
    pwalk(~ message(sprintf("  %d  %s", ..1, ..2)))
  message("  Add these names to surveyors_by_year.csv or update NAME_ALIASES above.")
}

beeple_results <- windows_with_user |>
  rowwise() |>
  mutate(
    obs_in_window = list(
      if (is.na(username)) {
        character(0)
      } else {
        obs |>
          filter(username == .data$username,
                 observed_on >= window_start,
                 observed_on <= window_end) |>
          pull(observed_on) |>
          as.character() |>
          unique() |>
          sort()
      }
    ),
    n_obs_days    = length(obs_in_window),
    all_obs_dates = if (n_obs_days == 0) NA_character_
                    else paste(obs_in_window, collapse = "; "),
    inferred_date = case_when(
      n_obs_days == 1 ~ obs_in_window[[1]],
      TRUE            ~ NA_character_
    ),
    status = case_when(
      is.na(username) ~ "no_username",
      n_obs_days == 0 ~ "no_obs",
      n_obs_days == 1 ~ "confirmed",
      TRUE            ~ "ambiguous"
    ),
    note = case_when(
      status == "no_username" ~
        "First name not found in beeple roster for this year; check NAME_ALIASES or roster.",
      status == "no_obs" ~
        "No iNat obs in window. Person may have skipped, or forgot to tag.",
      status == "ambiguous" ~
        paste0("Multiple obs days: ", all_obs_dates, " -- fill in correct date."),
      TRUE ~ NA_character_
    )
  ) |>
  ungroup() |>
  select(year, username, first_name, transect,
         window_start, window_end,
         inferred_date, all_obs_dates, n_obs_days,
         status, note) |>
  arrange(year, window_start, first_name)

message("\n--- Part 1: beeple date inference ---")
beeple_results |>
  count(status) |>
  arrange(desc(n)) |>
  pwalk(~ message(sprintf("  %-14s %d", ..1, ..2)))
message(sprintf("  %.1f%% of windows resolved to a confirmed single date.",
                mean(beeple_results$status == "confirmed") * 100))

# Dropout detection: flag anyone with 3+ no_obs windows in a year
dropout_suspects <- beeple_results |>
  filter(status == "no_obs") |>
  group_by(year, first_name, username) |>
  summarise(n_no_obs = n(), .groups = "drop") |>
  filter(n_no_obs >= 3)

if (nrow(dropout_suspects) > 0) {
  message(sprintf(
    "\n  POSSIBLE DROPOUTS -- %d person(s) with 3+ no_obs windows in a year:",
    nrow(dropout_suspects)
  ))
  dropout_suspects |>
    arrange(year, first_name) |>
    pwalk(~ message(sprintf("    %d  %-12s  (@%s)  %d no_obs windows", ..1, ..2, ..3, ..4)))
  message("  These people may have dropped out mid-season or just forgot to tag.")
  message("  Check their iNat profiles and update ", BEEPLE_OFFICIAL, " accordingly.")
}

# ============================================================
# PART 2: Build beeple_survey_dates_official.csv
# ============================================================
#
# One row per calendar window (year + window_start + window_end).
# Each transect column holds the iNat username of the assigned surveyor.
# The 'date' column is the inferred (or manually set) survey date.
#
# Status values:
#   confirmed  -- script auto-inferred a single date; safe to use as-is
#   ambiguous  -- multiple obs days in window; YOU need to pick the right date
#   no_obs     -- no obs found; person may have skipped or forgotten to tag
#   no_username-- name not matched to roster; fix NAME_ALIASES or roster
#   manual     -- YOU filled in this date; script will NEVER overwrite this row
#   skipped    -- YOU confirmed this person did not survey this window;
#                 script will NEVER overwrite this row

# Summarise per window: one status + one date for the whole window
window_summary <- beeple_results |>
  group_by(year, window_start, window_end) |>
  summarise(
    date = {
      d <- inferred_date[status == "confirmed"]
      u <- unique(na.omit(d))
      if (length(u) == 1) u[1] else NA_character_
    },
    status = case_when(
      any(status == "ambiguous")   ~ "ambiguous",
      any(status == "no_username") ~ "no_username",
      any(status == "no_obs")      ~ "no_obs",
      all(status == "confirmed")   ~ "confirmed",
      TRUE                          ~ "partial"
    ),
    notes = {
      bad  <- status %in% c("ambiguous", "no_obs", "no_username")
      if (any(bad))
        paste(paste0(transect[bad], ": ", note[bad]), collapse = "; ")
      else NA_character_
    },
    .groups = "drop"
  )

# Pivot transect usernames to wide
beeple_wide_new <- beeple_results |>
  select(year, window_start, window_end, transect, username) |>
  pivot_wider(
    id_cols    = c(year, window_start, window_end),
    names_from = transect,
    values_from = username,
    values_fn  = first
  ) |>
  left_join(window_summary, by = c("year", "window_start", "window_end")) |>
  mutate(date = as.Date(date)) |>
  # Reorder: known transects first, then any extras, then status/notes
  { df <- .
    trans_present <- intersect(TRANSECT_ORDER, names(df))
    trans_extra   <- setdiff(names(df),
                             c("year", "window_start", "window_end", "date",
                               "status", "notes", TRANSECT_ORDER))
    select(df, year, window_start, window_end, date,
           all_of(c(trans_present, trans_extra)), status, notes)
  } |>
  arrange(year, window_start)

# Merge with existing official file: LOCKED rows (manual/skipped) are never touched
if (file.exists(BEEPLE_OFFICIAL)) {
  beeple_existing <- read_csv(BEEPLE_OFFICIAL, show_col_types = FALSE) |>
    mutate(window_start = as.Date(window_start),
           window_end   = as.Date(window_end),
           date         = as.Date(date))

  locked <- beeple_existing |>
    filter(status %in% c("manual", "skipped"))

  beeple_official <- beeple_wide_new |>
    anti_join(locked, by = c("year", "window_start", "window_end")) |>
    bind_rows(locked) |>
    arrange(year, window_start)

  prev_resolved <- sum(beeple_existing$status %in% c("confirmed", "manual", "skipped"))
  new_resolved  <- sum(beeple_official$status %in% c("confirmed", "manual", "skipped"))
  if (new_resolved > prev_resolved)
    message(sprintf("  %d new window(s) resolved since last run.", new_resolved - prev_resolved))
} else {
  beeple_official <- beeple_wide_new
  message("  Creating beeple_survey_dates_official.csv for the first time.")
}

write_csv(beeple_official, BEEPLE_OFFICIAL, na = "")
message(sprintf("\nBeeple official -> %s  (%d rows)", BEEPLE_OFFICIAL, nrow(beeple_official)))

# ============================================================
# PART 3: Build intern_survey_dates_official.csv
# ============================================================

full_roster <- read_csv(ROSTER_FILE, show_col_types = FALSE) |>
  select(year, username = inaturalist_username, first_name, last_name, role) |>
  mutate(year = as.integer(year))

intern_raw <- read_csv(INTERN_DATES, show_col_types = FALSE) |>
  mutate(year = as.integer(year), date = as.Date(date))

intern_long <- intern_raw |>
  left_join(
    full_roster |> filter(role == "intern") |>
      select(year, first_name, last_name, username),
    by = c("year", "first_name")
  ) |>
  mutate(
    full_name = if_else(is.na(last_name), first_name, paste(first_name, last_name)),
    status    = if_else(!is.na(notes) & str_detect(notes, "training day"),
                        "training", "confirmed")
  )

# Parse transects_surveyed ("TP, BST, UPMON, OT") into transect_1 ... transect_4
intern_transects <- intern_raw |>
  filter(method == "non-lethal", !is.na(transects_surveyed)) |>
  group_by(year, date) |>
  slice(1) |>
  ungroup() |>
  mutate(
    trans      = str_split(transects_surveyed, ",\\s*"),
    transect_1 = map_chr(trans, ~ if (length(.x) >= 1) trimws(.x[1]) else NA_character_),
    transect_2 = map_chr(trans, ~ if (length(.x) >= 2) trimws(.x[2]) else NA_character_),
    transect_3 = map_chr(trans, ~ if (length(.x) >= 3) trimws(.x[3]) else NA_character_),
    transect_4 = map_chr(trans, ~ if (length(.x) >= 4) trimws(.x[4]) else NA_character_)
  ) |>
  select(year, date, transect_1, transect_2, transect_3, transect_4)

# Pivot to one row per (year, date): surveyor_1/username_1, surveyor_2/username_2
intern_official <- intern_long |>
  arrange(year, date, first_name) |>
  group_by(year, date, method, status) |>
  mutate(n = row_number()) |>
  ungroup() |>
  pivot_wider(
    id_cols     = c(year, date, method, status),
    names_from  = n,
    values_from = c(full_name, username),
    names_glue  = "{ifelse(.value == 'full_name', 'surveyor', 'username')}_{n}"
  ) |>
  left_join(intern_transects, by = c("year", "date")) |>
  select(year, date, method,
         surveyor_1, username_1, surveyor_2, username_2,
         transect_1, transect_2, transect_3, transect_4,
         status) |>
  arrange(year, date)

write_csv(intern_official, INTERN_OFFICIAL, na = "")
message(sprintf("Intern official -> %s  (%d rows)", INTERN_OFFICIAL, nrow(intern_official)))

# ============================================================
# PART 4: Needs-review file + instructions
# ============================================================

needs_review <- beeple_official |>
  filter(status %in% c("ambiguous", "no_obs")) |>
  arrange(year, window_start)

if (nrow(needs_review) > 0) {
  write_csv(needs_review, NEEDS_REVIEW, na = "")

  message(paste(rep("=", 60), collapse = ""))
  message(sprintf("  ACTION NEEDED: %d window(s) require manual review.", nrow(needs_review)))
  message(paste(rep("=", 60), collapse = ""))
  message("")
  message("  Step 1. Open: ", NEEDS_REVIEW)
  message("")
  message("  Step 2. For each row:")
  message("    AMBIGUOUS (multiple obs days in the window):")
  message("      - Look at the 'notes' column to see the candidate dates.")
  message("      - Fill in the correct date in the 'date' column.")
  message("      - Change 'status' to 'manual'.")
  message("")
  message("    NO_OBS (no observations found in the window):")
  message("      - If the person genuinely skipped this window:")
  message("          leave 'date' blank, change 'status' to 'skipped'.")
  message("      - If they may have forgotten to tag:")
  message("          check their iNat profile for that date range,")
  message("          fill in the date if found, change 'status' to 'manual'.")
  message("")
  message("  Step 3. Copy your updated rows into: ", BEEPLE_OFFICIAL)
  message("          (find and replace the matching ambiguous/no_obs rows)")
  message("")
  message("  Step 4. Re-run this script.")
  message("          Rows marked 'manual' or 'skipped' will be locked forever.")
  message(paste(rep("=", 60), collapse = ""))
} else {
  message("\nAll beeple windows resolved -- no review needed.")
  if (file.exists(NEEDS_REVIEW)) {
    file.remove(NEEDS_REVIEW)
    message("(Deleted stale needs_review file.)")
  }
}

# ============================================================
# Summary
# ============================================================

message("\n--- Done ---")
message("  Beeple windows:")
beeple_official |>
  count(status) |>
  arrange(desc(n)) |>
  pwalk(~ message(sprintf("    %-14s %d", ..1, ..2)))

message(sprintf("  Intern survey events: %d confirmed, %d training",
                sum(intern_official$status == "confirmed"),
                sum(intern_official$status == "training")))
message("")
message("  Next step: re-run scripts/clean/inat_bee_clean.R to add")
message("  on_survey_date flags and recover any missing-tag observations.")
