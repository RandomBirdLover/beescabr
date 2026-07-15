# =============================================================
# clean/survey_tag_qc.R
# beescabr pipeline -- survey tag/location QC (missing-tag + misplaced)
# Created 2026-07-14.
#
# Two cross-checks that fall out of the survey_dates architecture. Both run on
# the FULL SD-County flattened export (data/cache/export_flat.rds) -- survey
# membership is decided by tags/obs-fields on the whole set, and the CABR
# boundary is used only afterward to partition/flag (never to pre-filter, or
# the misplaced obs would be thrown away before we could catch them).
#
# STEP 5 -- MISSING-TAG RECOVERY (survey_untagged_candidates.csv):
#   Obs by a known surveyor, INSIDE the CABR box, on a CONFIRMED survey day,
#   that carry no valid Cabrillo survey tag. Almost certainly real survey data
#   where the hashtag was forgotten. Each row gets a suggested_tag to add.
#
# STEP 6 -- MISPLACED-LOCATION FLAG (survey_misplaced_candidates.csv):
#   Obs that DO carry a Cabrillo survey tag (in the tag list or an obs-field)
#   but sit OUTSIDE the CABR box -- coordinates are probably wrong. Review.
#
# Inputs:
#   data/cache/export_flat.rds                       (full SD-County obs + tags)
#   data/outputs/inat_clean/cabr_inat_bee_clean.csv  (CABR-box obs + triage)
#   data/project_info/survey_dates.csv               (survey_dates.R output)
#   data/project_info/crosswalk_master.csv        (tag/field crosswalk)
#   data/project_info/surveyors_by_year.csv          (roster)
#
# Run: source("scripts/clean/survey_tag_qc.R"); survey_tag_qc()
# =============================================================

library(dplyr)
library(stringr)
library(tidyr)
library(readr)

QC_EXPORT    <- "data/cache/export_flat.rds"
QC_CLEAN     <- "data/outputs/inat_clean/cabr_inat_bee_clean.csv"
QC_DATES     <- "data/project_info/survey_dates.csv"
QC_CROSSWALK <- "data/project_info/crosswalk_master.csv"
QC_ROSTER    <- "data/project_info/surveyors_by_year.csv"
QC_UNTAGGED  <- "data/outputs/inat_clean/qc/survey_untagged_candidates.csv"
QC_MISPLACED <- "data/outputs/inat_clean/qc/survey_misplaced_candidates.csv"

qc_norm <- function(s) tolower(gsub("^#", "", trimws(s)))

# Normalized variant -> canonical/category, for the survey (not transect/field)
# tag categories that establish Cabrillo membership.
qc_survey_variants <- function(crosswalk) {
  crosswalk |>
    filter(type == "tag", category %in% c("Beeple", "Intern", "General")) |>
    transmute(v = if_else(is.na(inat_variants) | trimws(inat_variants) == "",
                          name, paste(name, inat_variants, sep = "; "))) |>
    separate_rows(v, sep = ";\\s*") |>
    mutate(key = qc_norm(v)) |>
    filter(nchar(key) > 0, !grepl("^\\(", key), !(key %in% c("n/a", "na"))) |>
    pull(key) |> unique()
}

# The standard survey hashtag to suggest for a role+year (what the surveyor
# should have used). Interns 2024 have their own tag; beeple use the yearly one.
qc_suggested_tag <- function(role, year) {
  dplyr::case_when(
    role == "intern" & year == 2024 ~ "Cabrillo2024InternBeeSurvey",
    role == "intern"                ~ NA_character_,   # net interns: no iNat tag
    TRUE                            ~ paste0("Cabrillo", year, "BeeSurvey")
  )
}

survey_tag_qc <- function(export = QC_EXPORT, clean_path = QC_CLEAN,
                          dates_path = QC_DATES, crosswalk_path = QC_CROSSWALK,
                          roster_path = QC_ROSTER, write = TRUE) {
  crosswalk <- read_csv(crosswalk_path, show_col_types = FALSE)
  clean     <- read_csv(clean_path, show_col_types = FALSE)
  dates     <- read_csv(dates_path, show_col_types = FALSE)
  roster    <- read_csv(roster_path, show_col_types = FALSE) |>
    transmute(year = as.integer(year), user = inaturalist_username, role) |>
    filter(!is.na(user)) |> distinct(user, role) |>
    distinct(user, .keep_all = TRUE)
  survey_variants <- qc_survey_variants(crosswalk)

  # ---- STEP 5: missing-tag recovery (within CABR) ----
  survey_days <- dates |>
    filter(!is.na(date), confirmed == TRUE, source %in% c("observation", "intern-log")) |>
    transmute(user = inat_username, date = as.Date(date)) |>
    filter(!is.na(user)) |> distinct()

  untagged <- clean |>
    filter(triage != "keep", !is.na(observer), !is.na(observed_date)) |>
    transmute(obs_id, url, user = observer, date = as.Date(observed_date),
              transect, scientific_name, triage, triage_reason) |>
    inner_join(survey_days, by = c("user", "date")) |>
    left_join(roster, by = "user") |>
    mutate(year = as.integer(format(date, "%Y")),
           suggested_tag = qc_suggested_tag(role, year)) |>
    arrange(date, user) |>
    select(obs_id, url, user, role, date, transect, scientific_name,
           triage, triage_reason, suggested_tag)

  # ---- STEP 6: misplaced-location flag (tagged but outside CABR) ----
  x <- readRDS(export)
  # tag-list signal
  by_taglist <- x |>
    transmute(id, tag_list) |>
    filter(!is.na(tag_list), tag_list != "") |>
    mutate(tag = str_split(tag_list, ",\\s*")) |> unnest(tag) |>
    mutate(key = qc_norm(tag)) |>
    filter(key %in% survey_variants) |> pull(id) |> unique()
  # obs-field signal (a survey tag typed into any observation field)
  fld <- names(x)[startsWith(names(x), "field:")]
  by_field <- x |> select(id, all_of(fld)) |>
    pivot_longer(-id, values_to = "val") |>
    filter(!is.na(val), val != "", qc_norm(val) %in% survey_variants) |>
    pull(id) |> unique()

  tagged_ids <- union(by_taglist, by_field)
  misplaced <- x |>
    filter(id %in% tagged_ids, !(id %in% clean$obs_id)) |>
    transmute(obs_id = id, url, user = user_login, date = as.Date(observed_on),
              latitude, longitude, place_guess, scientific_name,
              flag = "Cabrillo-tagged but outside CABR box -- check coordinates")

  if (write) {
    dir.create(dirname(QC_UNTAGGED), recursive = TRUE, showWarnings = FALSE)
    write.csv(untagged,  QC_UNTAGGED,  row.names = FALSE, na = "")
    write.csv(misplaced, QC_MISPLACED, row.names = FALSE, na = "")
    message("STEP 5 missing-tag candidates: ", nrow(untagged), " -> ", QC_UNTAGGED)
    message("STEP 6 misplaced candidates:   ", nrow(misplaced), " -> ", QC_MISPLACED)
  }
  invisible(list(untagged = untagged, misplaced = misplaced))
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) survey_tag_qc()
