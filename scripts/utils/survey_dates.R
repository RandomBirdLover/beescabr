# =============================================================
# utils/survey_dates.R
# beescabr pipeline -- build the master survey_dates.csv
# Rewritten 2026-07-14 from scratch (tidy, one row per surveyor-event).
#
# Combines every survey done by interns (exact dates, from the intern log) and
# beeple (4-day calendar windows, from beeple_calendar_windows.csv), tagged
# with role/method/technique from the roster, and -- for anyone who used
# iNaturalist -- CONFIRMED against their Cabrillo-tagged observations.
#
# "Confirmed" = we found the survey actually happened:
#   * beeple + 2024+ interns (photo/iNat): TRUE iff >=1 of their observations
#     carries a valid Cabrillo survey tag on the date (intern) or in the
#     window (beeple). That tag check is already done upstream -- an obs with
#     a Cabrillo keyword in its tags OR observation-fields is triage=="keep".
#   * 2021-2023 interns (net/lethal, never used iNat): TRUE from the written
#     log, with a note; there is nothing online to check them against.
#
# Output columns (tidy -- one row per person per survey):
#   year, role, method, technique, first_name, last_name, inat_username,
#   date, window_start, window_end, transect, confirmed, n_obs, note
#
# Inputs:
#   data/project_info/beeple_calendar_windows.csv  (parse_beeple_calendars.R)
#   data/project_info/intern_survey_dates.csv      (exact intern dates)
#   data/project_info/surveyors_by_year.csv        (roster: role/method/etc)
#   data/outputs/inat_clean/cabr_inat_bee_clean.csv (run inat_bee_clean.R first)
#
# Output:
#   data/project_info/survey_dates.csv
#
# Run: source("scripts/utils/survey_dates.R"); build_survey_dates()
# =============================================================

library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(lubridate)

SD_WINDOWS  <- "data/project_info/beeple_calendar_windows.csv"
SD_INTERN   <- "data/project_info/intern_survey_dates.csv"
SD_ROSTER   <- "data/project_info/surveyors_by_year.csv"
SD_INAT     <- "data/outputs/inat_clean/cabr_inat_bee_clean.csv"
SD_OUT      <- "data/project_info/survey_dates.csv"

SD_COLUMNS <- c("year", "role", "method", "technique",
                "first_name", "last_name", "inat_username",
                "date", "window_start", "window_end", "transect",
                "confirmed", "n_obs", "source", "note")
# source: how the row's date was established --
#   "observation"  a real Cabrillo-tagged survey day (date is ground truth)
#   "calendar-only" a planned beeple window that produced no tagged obs
#   "intern-log"    an exact date from the written intern log

# Calendar first-name -> roster first-name aliases (nicknames / typos).
# Add entries like c("Juliet" = "Julia") if a calendar uses a variant spelling.
SD_NAME_ALIASES <- c()
sd_alias <- function(x) {
  if (length(SD_NAME_ALIASES) == 0) return(x)
  recode(x, !!!SD_NAME_ALIASES)
}

# ---- keep-tagged iNat observations (valid Cabrillo survey tag) --------------
# One row per (username, date): observation count, survey_type, and the
# transect the person recorded that day (from tags/obs-fields; ~81% present).
# "keep" means a Cabrillo keyword landed in the tags OR the observation-fields
# (decided upstream in inat_bee_clean.R). Empty-safe if the clean file absent.
sd_load_keep_obs <- function(path = SD_INAT) {
  empty <- tibble(inat_username = character(), obs_date = as.Date(character()),
                  survey_type = character(), transect = character(), n = integer())
  if (!file.exists(path)) return(empty)
  read_csv(path, show_col_types = FALSE) |>
    filter(triage == "keep", !is.na(observer), !is.na(observed_date)) |>
    transmute(inat_username = observer, obs_date = as.Date(observed_date),
              survey_type, transect = na_if(as.character(transect), "")) |>
    group_by(inat_username, obs_date) |>
    summarise(survey_type = dplyr::first(stats::na.omit(survey_type)),
              transect    = dplyr::first(stats::na.omit(transect)),
              n = dplyr::n(), .groups = "drop")
}

# ---- intern surveys: exact dates -------------------------------------------
sd_build_interns <- function(intern_df, roster, keep_obs) {
  roster_i <- roster |>
    filter(role == "intern") |>
    select(year, first_name, last_name, inat_username, technique) |>
    distinct(year, first_name, .keep_all = TRUE)

  intern_df |>
    mutate(year = as.integer(year), date = as.Date(date),
           first_name = sd_alias(first_name)) |>
    left_join(roster_i, by = c("year", "first_name")) |>
    mutate(
      role       = "intern",
      transect   = if_else(!is.na(transects_surveyed) & transects_surveyed != "",
                           transects_surveyed, NA_character_),
      window_start = as.Date(NA), window_end = as.Date(NA),
      is_training  = !is.na(notes) & str_detect(str_to_lower(notes), "training")
    ) |>
    # confirm the iNat-using interns against their tagged obs on the exact date
    left_join(keep_obs |> select(inat_username, obs_date, n),
              by = c("inat_username", "date" = "obs_date")) |>
    mutate(
      source    = "intern-log",
      uses_inat = method == "non-lethal" & !is.na(inat_username),
      n_obs = if_else(uses_inat, coalesce(n, 0L), NA_integer_),
      confirmed = case_when(
        uses_inat            ~ n_obs > 0,
        method == "lethal"   ~ TRUE,   # ground-truth from the written log
        TRUE                 ~ NA      # non-lethal intern with no known username
      ),
      note = case_when(
        is_training                       ~ "training day -- exclude from analysis",
        method == "lethal"                ~ "from intern log; net survey, no iNaturalist",
        uses_inat & n_obs == 0            ~ "no Cabrillo-tagged obs found on this date",
        method == "non-lethal" & is.na(inat_username) ~ "no iNaturalist username on file",
        TRUE                              ~ NA_character_
      )
    ) |>
    select(any_of(SD_COLUMNS))
}

# ---- beeple surveys: observation-driven, calendar for context --------------
# The calendar is the PLAN; the tagged observations are what actually happened.
# So the survey dates come from the obs (every person x tagged day = a survey),
# and the calendar supplies the transect when the obs lacks one and reveals
# planned windows that produced no tagged obs (a likely skip or forgotten tag).
sd_build_beeple <- function(windows, roster, keep_obs) {
  roster_b <- roster |>
    filter(role == "beeple") |>
    select(year, first_name, last_name, inat_username, method, technique) |>
    distinct(year, first_name, .keep_all = TRUE)
  # name/method/technique are stable per person; index by username for the
  # observation-driven rows (an obs's year is taken from its own date).
  by_user <- roster_b |>
    distinct(inat_username, .keep_all = TRUE) |>
    select(inat_username, first_name, last_name, method, technique) |>
    filter(!is.na(inat_username))

  # planned window-assignments with a resolved username
  wu <- windows |>
    mutate(year = as.integer(year),
           window_start = as.Date(window_start), window_end = as.Date(window_end),
           first_name = sd_alias(first_name)) |>
    left_join(roster_b, by = c("year", "first_name"))

  # ---- (A) real survey days from beeple observations ----
  bdays <- keep_obs |>
    filter(survey_type == "beeple", !is.na(inat_username)) |>
    transmute(inat_username, date = obs_date, transect_obs = transect, n_obs = n) |>
    mutate(year = as.integer(format(date, "%Y")))

  # For each obs-day, find the planned window (if any) it falls in -> transect
  # fallback + window context. slice(1) keeps a single window per day.
  day_win <- bdays |>
    select(inat_username, date) |>
    inner_join(wu |> filter(!is.na(inat_username)) |>
                 select(inat_username, window_start, window_end, transect_assigned = transect),
               by = "inat_username", relationship = "many-to-many") |>
    filter(date >= window_start, date <= window_end) |>
    group_by(inat_username, date) |>
    slice(1) |> ungroup()

  obs_rows <- bdays |>
    left_join(day_win, by = c("inat_username", "date")) |>
    left_join(by_user, by = "inat_username") |>
    mutate(
      role = "beeple", source = "observation", confirmed = TRUE,
      transect = coalesce(transect_obs, transect_assigned),
      note = if_else(is.na(window_start), "surveyed outside any planned calendar window",
                     NA_character_)
    ) |>
    select(any_of(SD_COLUMNS))

  # ---- (B) planned windows that produced NO tagged obs ----
  win_has_obs <- wu |>
    mutate(.wid = row_number()) |>
    left_join(bdays |> select(inat_username, date), by = "inat_username",
              relationship = "many-to-many") |>
    group_by(.wid) |>
    summarise(any_obs = any(!is.na(date) & date >= window_start & date <= window_end),
              .groups = "drop")

  miss_rows <- wu |>
    mutate(.wid = row_number()) |>
    left_join(win_has_obs, by = ".wid") |>
    filter(is.na(inat_username) | !coalesce(any_obs, FALSE)) |>
    mutate(
      role = "beeple", source = "calendar-only",
      date = as.Date(NA), n_obs = 0L,
      confirmed = if_else(is.na(inat_username), NA, FALSE),
      note = if_else(is.na(inat_username),
                     "first name not matched to a beeple in the roster",
                     "planned window, no Cabrillo-tagged obs (skipped or untagged)")
    ) |>
    select(any_of(SD_COLUMNS))

  bind_rows(obs_rows, miss_rows)
}

# ---- orchestrator ----------------------------------------------------------
build_survey_dates <- function(windows_path = SD_WINDOWS, intern_path = SD_INTERN,
                               roster_path = SD_ROSTER, inat_path = SD_INAT,
                               out = SD_OUT, write = TRUE) {
  roster    <- read_csv(roster_path, show_col_types = FALSE) |>
    rename(inat_username = inaturalist_username) |>
    mutate(year = as.integer(year), first_name = str_trim(first_name),
           last_name = str_trim(last_name), inat_username = na_if(str_trim(inat_username), ""))
  keep_obs  <- sd_load_keep_obs(inat_path)
  intern_df <- read_csv(intern_path, show_col_types = FALSE)
  windows   <- read_csv(windows_path, show_col_types = FALSE)

  master <- bind_rows(
    sd_build_interns(intern_df, roster, keep_obs),
    sd_build_beeple(windows, roster, keep_obs)
  ) |>
    mutate(across(where(is.character), ~ na_if(.x, ""))) |>
    arrange(year, date, window_start, role, first_name) |>
    select(all_of(SD_COLUMNS))

  if (write) {
    dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
    write.csv(master, out, row.names = FALSE, na = "")
    message("Wrote ", nrow(master), " survey rows -> ", out)
  }
  master
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) build_survey_dates()
