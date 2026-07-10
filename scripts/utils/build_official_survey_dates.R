# build_official_survey_dates.R
#
# Purpose: combine intern survey dates (exact, from source PDF) and beeple
# survey dates (inferred from iNat obs by infer_beeple_survey_dates.R) into
# a single canonical reference table.
#
# Inputs:
#   data/project_info/intern_survey_dates.csv     -- exact intern dates, all years
#   data/project_info/beeple_survey_dates.csv     -- inferred beeple dates
#       (run scripts/utils/infer_beeple_survey_dates.R first)
#   data/project_info/surveyors_by_year.csv       -- name/username/role roster
#
# Output:
#   data/project_info/cabr_bee_official_survey_dates.csv
#   Columns:
#     year         -- survey year
#     date         -- survey date (NA if ambiguous/unknown for beeple)
#     username     -- iNat username (NA if none on record)
#     first_name
#     last_name
#     role         -- "intern" or "beeple"
#     method       -- "lethal", "non-lethal"
#     transect     -- start transect if known; NA otherwise
#     status       -- "confirmed", "ambiguous", "no_obs", "no_username", "training"
#     notes        -- source notes or inference notes

library(tidyverse)

# ---- config ----------------------------------------------------------------

INTERN_DATES   <- "data/project_info/intern_survey_dates.csv"
BEEPLE_DATES   <- "data/project_info/beeple_survey_dates.csv"
ROSTER_FILE    <- "data/project_info/surveyors_by_year.csv"
OUTPUT_FILE    <- "data/project_info/cabr_bee_official_survey_dates.csv"

# ---- load roster -----------------------------------------------------------

roster <- read_csv(ROSTER_FILE, show_col_types = FALSE) |>
  select(year, username = inaturalist_username, first_name, last_name, role, method) |>
  mutate(year = as.integer(year))

# ---- intern dates ----------------------------------------------------------

if (!file.exists(INTERN_DATES)) {
  stop("intern_survey_dates.csv not found at: ", INTERN_DATES)
}

intern_raw <- read_csv(INTERN_DATES, show_col_types = FALSE) |>
  mutate(
    year = as.integer(year),
    date = as.Date(date)
  )

intern_joined <- intern_raw |>
  left_join(
    roster |> filter(role == "intern") |> select(year, first_name, last_name, username),
    by = c("year", "first_name")
  ) |>
  mutate(
    role   = "intern",
    status = if_else(
      !is.na(notes) & str_detect(notes, "training day"),
      "training",
      "confirmed"
    )
  ) |>
  select(year, date, username, first_name, last_name, role, method, transect, status, notes)

# ---- beeple dates ----------------------------------------------------------

if (!file.exists(BEEPLE_DATES)) {
  stop(
    "beeple_survey_dates.csv not found. ",
    "Run scripts/utils/infer_beeple_survey_dates.R first.\n",
    "  Expected at: ", BEEPLE_DATES
  )
}

beeple_raw <- read_csv(BEEPLE_DATES, show_col_types = FALSE) |>
  mutate(
    year          = as.integer(year),
    inferred_date = as.Date(inferred_date)
  )

beeple_joined <- beeple_raw |>
  left_join(
    roster |> filter(role == "beeple") |> select(year, first_name, last_name, method),
    by = c("year", "first_name")
  ) |>
  mutate(
    role   = "beeple",
    method = coalesce(method, "non-lethal"),  # beeple are always non-lethal
    notes  = coalesce(note, NA_character_)
  ) |>
  select(
    year,
    date      = inferred_date,
    username,
    first_name,
    last_name,
    role,
    method,
    transect,
    status,
    notes
  )

# ---- combine ---------------------------------------------------------------

combined <- bind_rows(intern_joined, beeple_joined) |>
  arrange(year, date, role, first_name) |>
  mutate(year = as.integer(year))

# ---- summary ---------------------------------------------------------------

message("\n--- build_official_survey_dates summary ---")

combined |>
  count(role, status) |>
  arrange(role, desc(n)) |>
  pwalk(~ message(sprintf("  %-8s  %-14s %d", ..1, ..2, ..3)))

n_intern_dates <- combined |>
  filter(role == "intern", status == "confirmed") |>
  distinct(year, date) |>
  nrow()

n_beeple_confirmed <- combined |>
  filter(role == "beeple", status == "confirmed") |>
  nrow()

n_beeple_ambig <- combined |>
  filter(role == "beeple", status == "ambiguous") |>
  nrow()

n_beeple_no_obs <- combined |>
  filter(role == "beeple", status == "no_obs") |>
  nrow()

message(sprintf("\n  Intern survey events (confirmed, unique dates): %d", n_intern_dates))
message(sprintf("  Beeple windows resolved:  %d confirmed, %d ambiguous, %d no_obs",
                n_beeple_confirmed, n_beeple_ambig, n_beeple_no_obs))

if (n_beeple_ambig > 0) {
  message("\n  ACTION NEEDED -- ambiguous beeple windows (review manually):")
  combined |>
    filter(role == "beeple", status == "ambiguous") |>
    select(year, first_name, transect, notes) |>
    pwalk(~ message(sprintf("    %d  %-10s  %-6s  %s", ..1, ..2, ..3, ..4)))
}

# ---- write output ----------------------------------------------------------

write_csv(combined, OUTPUT_FILE)
message(sprintf("\nWritten %d rows to %s", nrow(combined), OUTPUT_FILE))
