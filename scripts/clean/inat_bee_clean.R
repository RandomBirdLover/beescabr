# =============================================================
# clean/inat_bee_clean.R
# beescabr pipeline -- non-lethal iNat bee data cleaning
# Rewritten: 2026-07-13 (API + DuckDB rewrite; callable function form)
#
# Exposes clean_inat_bees(con): reads the observation cache, triages against
# the tag crosswalk, folds obs-fields, and writes cabr_inat_bee_clean.csv.
# Ingest is the CALLER's job. Running this file directly still ingests then
# cleans. Source of truth is the DuckDB cache (the CSV export is retired).
#
# obs-field data comes from the API `ofvs` array via the flatten layer, which
# reads taxon-datatype fields from taxon$name (validated -- values populated).
#
# Run standalone: Rscript scripts/clean/inat_bee_clean.R
#   BEESCABR_SKIP_INGEST=1 reuses the cache without hitting the API.
# =============================================================

library(dplyr)
library(stringr)
library(readr)
library(sf)

local({
  need <- function(sym, file) if (!exists(sym)) source(file.path("scripts", file))
  need("PATHS",                    "config.R")
  need("write_fresh",              "utils/utils.R")
  need("build_tag_map",            "clean/triage.R")
  need("store_connect",            "db/store_conn.R")
  need("count_observations",       "db/observations_store.R")
  need("taxon_cache_get",          "db/taxon_store.R")
  need("inat_request",             "api/inat_http.R")
  need("flatten_observation",      "api/inat_flatten.R")
  need("resolve_taxonomy",         "api/inat_cache.R")
  need("ingest_observations",      "pipelines/ingest_inat.R")
  need("read_observations_export", "pipelines/read_inat.R")
})

# Safe column helpers (obs-field names contain ':' and '->').
col_or_na <- function(df, name, n = nrow(df)) if (name %in% names(df)) df[[name]] else rep(NA_character_, n)
coalesce_cols <- function(df, cols) {
  present <- intersect(cols, names(df))
  if (length(present) == 0) return(rep(NA_character_, nrow(df)))
  do.call(dplyr::coalesce, lapply(present, function(c) df[[c]]))
}

# ------------------------------------------------------------
# clean_inat_bees(con): full triage + clean against a populated cache.
# Returns the clean data frame invisibly.
# ------------------------------------------------------------
clean_inat_bees <- function(con) {
  if (!exists("cabr_survey_box")) source("scripts/spatial/spatial_utils.R")

  info_dir       <- "data/project_info"
  roster_path    <- list.files(info_dir, pattern = "^surveyors_by_year.*\\.csv$",   full.names = TRUE)[1]
  crosswalk_path <- list.files(info_dir, pattern = "^project_tags_fields.*\\.csv$", full.names = TRUE)[1]

  # ---- roster + crosswalk ----
  roster <- read_csv(roster_path, show_col_types = FALSE)
  usernames <- roster |>
    filter(method == "non-lethal", !is.na(inaturalist_username), trimws(inaturalist_username) != "") |>
    pull(inaturalist_username) |> unique()
  role_lookup <- roster |>
    filter(inaturalist_username %in% usernames) |>
    distinct(inaturalist_username, role) |>
    group_by(inaturalist_username) |>
    summarise(data_source = paste(sort(unique(role)), collapse = "/"), .groups = "drop")
  crosswalk <- read_csv(crosswalk_path, show_col_types = FALSE)
  tag_map <- build_tag_map(crosswalk)
  message(length(usernames), " surveyors | ", nrow(tag_map), " tag-map keys")

  # ---- read cache -> roster filter -> survey-box clip ----
  bees <- read_observations_export(con)
  export_roster <- bees |> filter(user_login %in% usernames)
  message(sprintf("  %d of %d obs from roster surveyors", nrow(export_roster), nrow(bees)))

  pts <- export_roster |>
    filter(!is.na(latitude), !is.na(longitude)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
    st_transform(st_crs(cabr_survey_box))
  in_box <- lengths(st_within(pts, cabr_survey_box)) > 0

  obs_base <- pts |> filter(in_box) |> st_drop_geometry() |>
    rename(obs_id = id, observer = user_login)

  obs <- obs_base |> mutate(observed_date = as.Date(observed_on), data_from = "cache")
  obs$flower_visited  <- coalesce_cols(obs_base, FLOWER_VISITED_SOURCES)
  obs$nesting         <- coalesce_cols(obs_base, NESTING_SOURCES)
  obs$bee_behavior    <- col_or_na(obs_base, "field:bee behavior")
  obs$mating_behavior <- col_or_na(obs_base, "field:behavior: mating")
  obs$on_ground       <- col_or_na(obs_base, "field:on ground?")
  obs$nesting_bee     <- col_or_na(obs_base, "field:nesting bee")
  obs$tags_override   <- col_or_na(obs_base, "field:tags")
  obs$transect_field  <- col_or_na(obs_base, "field:bee survey transect name")
  message(sprintf("  %d obs inside CABR survey box", nrow(obs)))

  # ---- triage ----
  triage_res <- triage_from_tag_list(obs |> select(obs_id, tag_list), tag_map)
  obs_triage   <- triage_res$triage
  unknown_tags <- triage_res$unknown
  dir.create(dirname(PATHS$inat_unknown_tags), recursive = TRUE, showWarnings = FALSE)
  if (nrow(unknown_tags) > 0) {
    message(sprintf("%d tag(s) not in crosswalk -> %s", nrow(unknown_tags), PATHS$inat_unknown_tags))
    write_fresh(unknown_tags, PATHS$inat_unknown_tags, na = "")
  } else message("All tags recognized by crosswalk.")

  # ---- Tags obs_field override ----
  tags_override_triage <- obs |>
    filter(!is.na(tags_override), nchar(trimws(tags_override)) > 0) |>
    select(obs_id, tags_override) |>
    mutate(key = norm_key(tags_override)) |>
    left_join(tag_map |> filter(category %in% c("Beeple", "Intern", "General")), by = "key") |>
    filter(!is.na(canonical)) |>
    transmute(obs_id, ov_has_survey = TRUE, ov_survey_type = tolower(category),
              ov_survey_year = str_extract(canonical, "\\d{4}"), ov_survey_tags = canonical)

  obs_triage <- obs_triage |>
    left_join(tags_override_triage, by = "obs_id") |>
    mutate(
      has_survey_from_tags_field = coalesce(ov_has_survey, FALSE),
      has_survey  = has_survey | has_survey_from_tags_field,
      survey_type = if_else(!is.na(ov_survey_type), ov_survey_type, survey_type),
      survey_year = if_else(!is.na(ov_survey_year), ov_survey_year, survey_year),
      survey_tags = case_when(
        !is.na(ov_survey_tags) & (is.na(survey_tags) | survey_tags == "") ~ ov_survey_tags,
        !is.na(ov_survey_tags) ~ paste(survey_tags, ov_survey_tags, sep = "; "),
        TRUE ~ survey_tags)
    ) |>
    select(-starts_with("ov_")) |>
    mutate(
      triage = case_when(is_exclude ~ "exclude", has_survey ~ "keep", TRUE ~ "flag"),
      triage_reason = case_when(
        is_exclude ~ paste0("exclude tag: ", exclude_tag),
        has_survey_from_tags_field ~ paste0("valid survey tag via Tags obs_field override: ", coalesce(survey_tags, "")),
        has_survey ~ "valid Cabrillo survey tag",
        TRUE ~ "no recognized survey tag -- review")
    )

  # ---- assemble + taxonomy (resolved inline during read) ----
  all_obs <- obs |>
    left_join(role_lookup, by = c("observer" = "inaturalist_username")) |>
    left_join(obs_triage, by = "obs_id") |>
    mutate(
      is_exclude = coalesce(is_exclude, FALSE), has_survey = coalesce(has_survey, FALSE),
      is_10min = coalesce(is_10min, FALSE), triage = coalesce(triage, "flag"),
      triage_reason = coalesce(triage_reason, "no tags on observation -- review"),
      has_survey_from_tags_field = coalesce(has_survey_from_tags_field, FALSE)
    ) |>
    rename(family = taxon_family_name, subfamily = taxon_subfamily_name,
           tribe = taxon_tribe_name, genus = taxon_genus_name,
           species = taxon_species_name, subspecies = taxon_subspecies_name) |>
    mutate(transect_conflict = !is.na(transect_field) & transect_field != "" &
             !is.na(transect) & transect != "" &
             str_to_lower(transect_field) != str_to_lower(transect))
  n_conflict <- sum(all_obs$transect_conflict, na.rm = TRUE)
  if (n_conflict > 0)
    message(sprintf("NOTE: %d obs where the transect obs-field disagrees with the tag-derived transect.", n_conflict))

  clean <- all_obs |>
    mutate(missing_coords = is.na(latitude) | is.na(longitude)) |>
    select(
      obs_id, url, observer, data_source, observed_date,
      latitude, longitude, quality_grade, captive_cultivated, coordinates_obscured, missing_coords,
      triage, triage_reason, has_survey_from_tags_field, survey_year, survey_type,
      transect, transect_field, transect_conflict, is_10min,
      taxon_id, scientific_name, common_name, rank,
      family, subfamily, tribe, genus, subgenus, complex, species, subspecies,
      flower_visited, bee_behavior, on_ground, nesting, nesting_bee, mating_behavior,
      tag_list, description, data_from
    )
  write_fresh(clean, PATHS$inat_clean, na = "")
  message("\n========== CLEAN SUMMARY ==========")
  message("Total obs: ", nrow(clean))
  print(count(clean, triage, sort = TRUE))
  message("Saved -> ", PATHS$inat_clean)

  # ---- optional date-based recovery ----
  beeple_official <- "data/project_info/beeple_survey_dates_official.csv"
  intern_official <- "data/project_info/intern_survey_dates_official.csv"
  if (file.exists(beeple_official) && file.exists(intern_official)) {
    message("\n--- Date-based recovery ---")
    TRANSECT_COLS <- c("OT", "TP1", "TP2", "UPMON", "BST")
    beeple_dates <- read_csv(beeple_official, show_col_types = FALSE) |>
      filter(status %in% c("confirmed", "manual")) |>
      select(date, any_of(TRANSECT_COLS)) |>
      tidyr::pivot_longer(-date, names_to = "t", values_to = "username") |>
      filter(!is.na(username), !is.na(date)) |> transmute(username, date = as.Date(date))
    intern_dates <- read_csv(intern_official, show_col_types = FALSE) |>
      filter(status == "confirmed") |>
      select(date, any_of(c("username_1", "username_2"))) |>
      tidyr::pivot_longer(-date, names_to = NULL, values_to = "username") |>
      filter(!is.na(username), !is.na(date)) |> transmute(username, date = as.Date(date))
    confirmed_events <- bind_rows(beeple_dates, intern_dates) |> distinct() |> mutate(on_survey_date = TRUE)
    clean <- clean |>
      left_join(confirmed_events, by = c("observer" = "username", "observed_date" = "date")) |>
      mutate(
        on_survey_date = coalesce(on_survey_date, FALSE),
        triage = if_else(triage == "flag" & on_survey_date, "recovered_by_date", triage),
        triage_reason = if_else(triage == "recovered_by_date",
                                "no survey tag but obs date matches confirmed survey date", triage_reason)
      )
    message(sprintf("  %d obs recovered (on confirmed survey date, lacked tag)",
                    sum(clean$triage == "recovered_by_date", na.rm = TRUE)))
    write_fresh(clean, PATHS$inat_clean, na = "")
    message("  Updated with on_survey_date + recovery -> ", PATHS$inat_clean)
  } else {
    message("\nSkipping date recovery -- run survey_dates.R first, then re-run.")
  }

  invisible(clean)
}

# ------------------------------------------------------------
# Standalone entrypoint (skipped when sourced by run_pipeline.R).
# ------------------------------------------------------------
if (!exists("BEESCABR_SOURCED_BY_RUNNER")) {
  main <- function() {
    con <- store_connect()
    on.exit(store_disconnect(con), add = TRUE)
    if (Sys.getenv("BEESCABR_SKIP_INGEST", "0") != "1") ingest_observations(con)
    else message("BEESCABR_SKIP_INGEST=1 -- using existing cache (", count_observations(con), " obs)")
    clean_inat_bees(con)
  }
  main()
}
