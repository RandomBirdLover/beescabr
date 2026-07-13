# =============================================================
# clean/triage.R
# beescabr pipeline -- tag crosswalk + observation triage
# Created: 2026-07-13 (extracted from inat_bee_clean.R)
#
# Pure-ish tag logic split out of the clean script so the keep/flag/exclude
# decision and the tag-variant crosswalk are unit-testable in isolation:
#   norm_key()            normalize a tag for matching
#   build_tag_map()       crosswalk -> (key, canonical, category) table
#   triage_from_tag_list() per-obs survey membership from a comma tag_list
#
# Depends on: dplyr, tidyr, stringr.
# =============================================================

library(dplyr)
library(tidyr)
library(stringr)

# Normalize a tag: strip leading #, trim, lowercase.
norm_key <- function(x) tolower(gsub("^#", "", trimws(x)))

# ------------------------------------------------------------
# build_tag_map(): expand the crosswalk's tag rows (name + variants) into a
# long (key, canonical, category) lookup, dropping empty/placeholder keys.
# ------------------------------------------------------------
build_tag_map <- function(crosswalk) {
  crosswalk |>
    filter(type == "tag") |>
    transmute(
      canonical = name,
      category,
      variants = if_else(is.na(inat_variants) | trimws(inat_variants) == "",
                         name, paste(name, inat_variants, sep = "; "))
    ) |>
    separate_rows(variants, sep = ";\\s*") |>
    mutate(key = norm_key(variants)) |>
    filter(nchar(key) > 0, !grepl("^\\(", key), !(key %in% c("n/a", "na"))) |>
    distinct(key, canonical, category)
}

# ------------------------------------------------------------
# triage_from_tag_list(): given obs_id + comma-separated tag_list, return
# list(triage = per-obs summary, unknown = unrecognized tags). Every obs
# gets a row even with no tags (defaults filled). Logic verbatim from the
# original clean script.
# ------------------------------------------------------------
triage_from_tag_list <- function(id_tags_df, tmap) {
  tags_long <- id_tags_df |>
    filter(!is.na(tag_list), nchar(trimws(tag_list)) > 0) |>
    mutate(tag = str_split(tag_list, ",\\s*")) |>
    unnest(tag) |>
    mutate(tag = trimws(tag), key = norm_key(tag)) |>
    filter(nchar(key) > 0) |>
    left_join(tmap, by = "key")

  triage_summary <- tags_long |>
    group_by(obs_id) |>
    summarise(
      is_exclude  = any(category == "Exclude", na.rm = TRUE),
      exclude_tag = paste(sort(unique(na.omit(canonical[category == "Exclude"]))), collapse = "; "),
      has_survey  = any(category %in% c("Beeple", "Intern", "General"), na.rm = TRUE),
      survey_tags = paste(sort(unique(na.omit(canonical[category %in% c("Beeple","Intern","General")]))), collapse = "; "),
      survey_year = {
        yr <- str_extract(na.omit(canonical[category %in% c("Beeple", "Intern")]), "\\d{4}")
        yr <- yr[!is.na(yr)]
        if (length(yr)) paste(sort(unique(yr)), collapse = "; ") else NA_character_
      },
      survey_type = {
        cats <- na.omit(category[category %in% c("Beeple", "Intern")])
        if (length(cats)) paste(sort(unique(tolower(cats))), collapse = "/") else NA_character_
      },
      transect = {
        tr <- sort(unique(na.omit(canonical[category == "Transect"])))
        if (length(tr)) paste(tr, collapse = "; ") else NA_character_
      },
      is_10min = any(canonical == "CabrilloBee10MinuteSurvey", na.rm = TRUE),
      .groups = "drop"
    )

  triage_complete <- tibble(obs_id = id_tags_df$obs_id) |>
    left_join(triage_summary, by = "obs_id") |>
    mutate(
      is_exclude = coalesce(is_exclude, FALSE),
      has_survey = coalesce(has_survey, FALSE),
      is_10min   = coalesce(is_10min, FALSE)
    )

  unknown <- tags_long |>
    filter(is.na(canonical), !is.na(tag), nchar(tag) > 0) |>
    count(tag, sort = TRUE, name = "n_obs")

  list(triage = triage_complete, unknown = unknown)
}
