# infer_beeple_survey_dates.R
#
# Purpose: infer the actual date each beeple surveyor went out within their
# assigned 4-day calendar window, by finding the day(s) they posted iNat
# observations inside that window.
#
# Inputs:
#   data/project_info/beeple_calendar_windows.csv   -- parsed from the annual
#       PDF calendars by scripts/utils/parse_beeple_calendars.py; one row per
#       (year, surveyor first name, transect, window). Add a new year by running
#       that script with the new PDF.
#   data/project_info/surveyors_by_year.csv         -- username / first_name
#       / role roster; used to join first names to iNat usernames.
#   data/outputs/inat_clean/cabr_inat_bee_clean.csv -- cleaned beeple obs;
#       used to find which day within each window the person actually surveyed.
#       Run scripts/clean/inat_bee_clean.R first if this file is missing.
#
# Output:
#   data/project_info/beeple_survey_dates.csv
#   Columns:
#     year, username, first_name, transect,
#     window_start, window_end,   -- the scheduled 4-day window
#     inferred_date,              -- actual survey day (or NA if ambiguous/missing)
#     all_obs_dates,              -- all dates with obs in window (semicolon-sep)
#     n_obs_days,                 -- number of distinct obs days found in window
#     status,                     -- confirmed / ambiguous / no_obs / no_username
#     note
#
# Status values:
#   confirmed   -- exactly 1 obs day found in the window; inferred_date is set
#   ambiguous   -- 2+ obs days found; all_obs_dates lists them; inferred_date NA
#   no_obs      -- person is in the roster but has no obs in this window
#   no_username -- first_name in calendar has no iNat username in roster
#
# Logic notes:
#   - beeple surveyed SOME day within their 4-day window; this script finds
#     which day. Most windows will be "confirmed" (one day with obs).
#   - Only observations with triage == "keep" are used (i.e., obs that carry a
#     valid Cabrillo survey tag). Personal/fun CABR visits (triage == "flag")
#     are excluded -- so if Bonnie or Cindy goes to CABR for fun within a
#     survey window, that visit won't be mistaken for their survey day.
#   - "ambiguous" most likely means the person surveyed on two consecutive days.
#     Review manually.
#   - "no_obs" means the person may have skipped the window, or their obs are
#     missing from the cleaned data (check inat_bee_clean.R QC output).
#   - This script does NOT modify the cleaned iNat data -- it only reads it.
#     The output beeple_survey_dates.csv is a reference table for analysis.

library(tidyverse)
library(lubridate)

# ---- config ----------------------------------------------------------------

CALENDAR_FILE  <- "data/project_info/beeple_calendar_windows.csv"
ROSTER_FILE    <- "data/project_info/surveyors_by_year.csv"
INAT_CLEAN     <- "data/outputs/inat_clean/cabr_inat_bee_clean.csv"
OUTPUT_FILE    <- "data/project_info/beeple_survey_dates.csv"

# First-name aliases: calendar typos / nicknames -> canonical first_name in roster.
# Add entries here if a future calendar uses a variant spelling.
NAME_ALIASES <- c()

# ---- load inputs -----------------------------------------------------------

if (!file.exists(INAT_CLEAN)) {
  stop(
    "cabr_inat_bee_clean.csv not found. Run scripts/clean/inat_bee_clean.R first.\n",
    "  Expected at: ", INAT_CLEAN
  )
}

windows  <- read_csv(CALENDAR_FILE,  show_col_types = FALSE) |>
  mutate(
    window_start = as.Date(window_start),
    window_end   = as.Date(window_end),
    year         = as.integer(year),
    # apply name aliases before joining
    first_name   = if_else(first_name %in% names(NAME_ALIASES),
                           NAME_ALIASES[first_name],
                           first_name)
  )

roster <- read_csv(ROSTER_FILE, show_col_types = FALSE) |>
  filter(role == "beeple") |>
  select(year, username = inaturalist_username, first_name) |>
  mutate(year = as.integer(year))

obs <- read_csv(INAT_CLEAN, show_col_types = FALSE) |>
  filter(data_source == "beeple",
         triage == "keep") |>        # survey-tagged obs only; excludes personal/fun visits
  select(username = observer, observed_on = observed_date) |>
  mutate(observed_on = as.Date(observed_on)) |>
  distinct()

# ---- join windows -> usernames ---------------------------------------------

windows_with_user <- windows |>
  left_join(roster, by = c("year", "first_name"))

no_username <- windows_with_user |>
  filter(is.na(username))

if (nrow(no_username) > 0) {
  message(sprintf(
    "WARNING: %d window row(s) have no matching username in the roster:",
    nrow(no_username)
  ))
  no_username |>
    distinct(year, first_name) |>
    arrange(year, first_name) |>
    pwalk(~ message(sprintf("  %d  %s", ..1, ..2)))
  message("  Add these names to surveyors_by_year.csv or update NAME_ALIASES above.")
}

# ---- infer survey date per window -----------------------------------------

results <- windows_with_user |>
  rowwise() |>
  mutate(
    # find all obs days for this username within [window_start, window_end]
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
        "No iNat obs found in window. Person may have skipped, or obs missing from cleaned data.",
      status == "ambiguous" ~
        "Multiple obs days in window. Review manually -- likely surveyed on one of these days.",
      TRUE ~ NA_character_
    )
  ) |>
  ungroup() |>
  select(
    year, username, first_name, transect,
    window_start, window_end,
    inferred_date, all_obs_dates, n_obs_days,
    status, note
  ) |>
  arrange(year, window_start, first_name)

# ---- summary ---------------------------------------------------------------

message("\n--- infer_beeple_survey_dates summary ---")
results |>
  count(status) |>
  arrange(desc(n)) |>
  pwalk(~ message(sprintf("  %-14s %d", ..1, ..2)))

pct_confirmed <- mean(results$status == "confirmed") * 100
message(sprintf("  %.1f%% of windows resolved to a confirmed single date.", pct_confirmed))

ambiguous <- filter(results, status == "ambiguous")
if (nrow(ambiguous) > 0) {
  message(sprintf("\n  ACTION NEEDED -- %d ambiguous window(s):", nrow(ambiguous)))
  ambiguous |>
    select(year, first_name, transect, window_start, window_end, all_obs_dates) |>
    pwalk(~ message(sprintf(
      "    %d  %-10s  %-6s  %s – %s  obs: %s", ..1, ..2, ..3, ..4, ..5, ..6
    )))
}

no_obs_rows <- filter(results, status == "no_obs")
if (nrow(no_obs_rows) > 0) {
  message(sprintf("\n  INFO -- %d window(s) with no obs found (may indicate skipped surveys):",
                  nrow(no_obs_rows)))
  no_obs_rows |>
    select(year, first_name, transect, window_start, window_end) |>
    pwalk(~ message(sprintf(
      "    %d  %-10s  %-6s  %s – %s", ..1, ..2, ..3, ..4, ..5
    )))
}

# ---- write output ----------------------------------------------------------

write_csv(results, OUTPUT_FILE)
message(sprintf("\nWritten %d rows to %s", nrow(results), OUTPUT_FILE))
