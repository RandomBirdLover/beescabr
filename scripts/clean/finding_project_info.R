# =============================================================
# clean/finding_project_info.R
# beescabr pipeline -- THE provenance layer ("the brain")
# Created 2026-07-14. See docs/ARCHITECTURE_project_info.md.
#
# Decides, ONCE, for every observation (bee now; plant when ingested): is this
# part of our project, whose is it, when, which transect. Taxonomy-blind on
# purpose. Everything downstream (inat_bee_clean.R / inat_plant_clean.R) just
# LOOKS UP the answer by obs_id -- no more ping-ponging, no tag logic in the
# clean scripts.
#
# The simple, validated model (Brandi's design):
#   * Is it a survey? -> ANY Cabrillo tag/field (all survey variants collapsed,
#     incl. the general 10-minute + metadata tags). Exclude tags win.
#   * What year?      -> the observation's own date.
#   * Beeple/intern?  -> the ROSTER (observer + year); missing -> "unknown".
#   * Transect?       -> the transect tag OR the transect obs-field.
#   * Flags is_10min / is_metadata ride along.
#   * in_cabr         -> the real CABR boundary (a LABEL; nothing is dropped).
#
# INPUTS
#   data/cache/export_flat.rds                 (bee obs; plant export added later)
#   data/project_info/crosswalk_master.csv  (tag/field crosswalk)
#   data/project_info/surveyors_by_year.csv    (roster)
#   data/project_info/beeple_calendar_windows.csv (from parse_beeple_calendars.R)
#   data/spatial/boundaries/cabr/cabr_survey_box.shp
#
# The intern schedule is NOT a separate file -- it lives IN survey_dates.csv as
# the `source == "intern-log"` rows (the only ground truth for the 2021-2023 net
# interns, who never used iNaturalist). Each run PRESERVES those rows and rebuilds
# the beeple/observation rows around them, so survey_dates.csv is built upon in
# place. Edit intern dates directly in survey_dates.csv.
#
# OUTPUTS
#   data/project_info/project_unclean_bee_observations.csv  <- NEW: the per-obs lookup
#   data/project_info/survey_dates.csv                      <- built upon in place
#   data/project_info/crosswalk_unknown_bee_tags.csv        <- unrecognized hashtags
#   data/project_info/crosswalk_unknown_bee_fields.csv      <- unrecognized obs-field names
#   data/project_info/crosswalk_unknown_bee_notes.csv       <- notes carrying survey keywords
#   data/outputs/inat_clean/qc/survey_untagged_bee_observations.csv
#   data/outputs/inat_clean/qc/survey_misplaced_bee_observations.csv
#
# Run: source("scripts/clean/finding_project_info.R"); finding_project_info()
# =============================================================

library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(sf)

# ---- paths ----
FPI_EXPORTS   <- list(list(path = "data/cache/export_flat.rds", kind = "bee"))
# When plants are ingested, add: list(path="data/cache/export_flat_plant.rds", kind="plant")
FPI_CROSSWALK <- "data/project_info/crosswalk_master.csv"
FPI_ROSTER    <- "data/project_info/surveyors_by_year.csv"
FPI_WINDOWS   <- "data/project_info/beeple_calendar_windows.csv"
FPI_BOUNDARY  <- "data/spatial/boundaries/cabr/cabr_survey_box.shp"

FPI_MEMBERSHIP     <- "data/project_info/project_unclean_bee_observations.csv"  # the per-obs lookup
FPI_SURVEY_DATES   <- "data/project_info/survey_dates.csv"
FPI_UNKNOWN_TAGS   <- "data/project_info/crosswalk_unknown_bee_tags.csv"    # unknown hashtags
FPI_UNKNOWN_FIELDS <- "data/project_info/crosswalk_unknown_bee_fields.csv"  # unknown obs-field NAMES
FPI_UNKNOWN_NOTES  <- "data/project_info/crosswalk_unknown_bee_notes.csv"   # notes w/ survey keywords
FPI_QC_UNTAGGED    <- "data/outputs/inat_clean/qc/survey_untagged_bee_observations.csv"
FPI_QC_MISPLACED   <- "data/outputs/inat_clean/qc/survey_misplaced_bee_observations.csv"

FPI_MEMBER_COLS <- c("obs_id", "kind", "observer", "observed_on", "survey_type",
                     "survey_year", "transect", "is_10min", "is_metadata",
                     "status", "status_reason", "in_cabr")
SD_COLUMNS <- c("year", "role", "method", "technique", "first_name", "last_name",
                "inat_username", "date", "window_start", "window_end", "transect",
                "confirmed", "n_obs", "source", "note")

fpi_norm <- function(s) tolower(gsub("^#", "", trimws(s)))

# ------------------------------------------------------------
# crosswalk -> normalized variant -> canonical/category (survey/transect/exclude)
# ------------------------------------------------------------
fpi_build_tagmap <- function(crosswalk) {
  # New concept-per-row crosswalk: tag spellings live in `inat_tag_variants`
  # (and the `name` itself, for the exclude/location rows whose name IS the tag).
  # Variants may be separated by ; OR , so split on both. what_for is the group.
  crosswalk |>
    filter(!is.na(name), trimws(name) != "") |>
    transmute(concept = name, what_for = tolower(trimws(what_for)),
              variants = paste(name, coalesce(inat_tag_variants, ""), sep = "; ")) |>
    separate_rows(variants, sep = "[;,]\\s*") |>
    mutate(key = fpi_norm(variants)) |>
    filter(nchar(key) > 0, !grepl("^\\(", key), !(key %in% c("n/a", "na"))) |>
    distinct(key, concept, what_for)
}

# ------------------------------------------------------------
# Per-obs signals from BOTH the tag list AND the obs-fields (a survey tag typed
# into any field counts too -- that's the manual "tags" override). Returns one
# row per obs_id with membership signals, plus the long unmatched-token table
# for the unknown-tags report.
# ------------------------------------------------------------
fpi_signals <- function(df, tagmap) {
  # tokens from tag_list
  from_tags <- df |>
    select(obs_id, tag_list) |>
    filter(!is.na(tag_list), tag_list != "") |>
    mutate(tok = str_split(tag_list, ",\\s*")) |> unnest(tok) |>
    transmute(obs_id, tok = trimws(tok), src = "tag")
  # tokens from non-empty obs-fields
  fld <- names(df)[startsWith(names(df), "field:")]
  fld <- fld[vapply(df[fld], function(c) any(!is.na(c) & c != ""), logical(1))]
  from_fields <- df |>
    select(obs_id, all_of(fld)) |>
    pivot_longer(-obs_id, values_to = "tok") |>
    filter(!is.na(tok), tok != "") |>
    transmute(obs_id, tok = trimws(tok), src = "field")

  toks <- bind_rows(from_tags, from_fields) |>
    mutate(key = fpi_norm(tok)) |> filter(nchar(key) > 0) |>
    left_join(tagmap, by = "key", relationship = "many-to-many")

  signals <- toks |>
    group_by(obs_id) |>
    summarise(
      has_survey  = any(what_for %in% c("survey", "meta_data", "beeple", "intern"), na.rm = TRUE),
      is_exclude  = any(what_for == "exclude", na.rm = TRUE),
      exclude_tag = paste(sort(unique(na.omit(concept[what_for == "exclude"]))), collapse = "; "),
      is_10min    = any(concept == "cabr_bee_10_min_survey", na.rm = TRUE),
      is_metadata = any(concept == "cabr_bee_meta_data", na.rm = TRUE),
      transect    = { tr <- sort(unique(na.omit(concept[what_for == "transect"])))
                      if (length(tr)) paste(tr, collapse = "; ") else NA_character_ },
      .groups = "drop"
    )

  # Free-text notes: old records sometimes wrote "10 min" in the description
  # instead of using the field/tag. Fold that into is_10min so we don't miss it.
  notes_flag <- df |>
    transmute(obs_id, note = tolower(coalesce(description, ""))) |>
    mutate(note_10min = str_detect(note, "10\\s*-?\\s*min")) |>
    filter(note_10min) |> select(obs_id, note_10min)
  signals <- signals |>
    left_join(notes_flag, by = "obs_id") |>
    mutate(is_10min = is_10min | coalesce(note_10min, FALSE)) |>
    select(-any_of("note_10min"))

  # unknown HASHTAGS only (field names/notes handled by their own reports).
  unknown_tags <- toks |> filter(src == "tag", is.na(concept)) |>
    select(obs_id, tok)
  list(signals = signals, unknown_tags = unknown_tags)
}

# ------------------------------------------------------------
# Unknown obs-FIELD NAMES: field:* columns present in the data whose name isn't
# in the crosswalk's obs_field entries. Scoped to fields OUR surveyors actually
# filled, so a stranger's exotic field isn't noise. For step-2a categorizing.
# ------------------------------------------------------------
fpi_unknown_fields <- function(df, crosswalk, our_users) {
  known <- crosswalk |>
    filter(!is.na(inat_field_variants), trimws(inat_field_variants) != "") |>
    transmute(v = inat_field_variants) |>
    separate_rows(v, sep = "[;,]\\s*") |>
    mutate(k = tolower(trimws(v))) |>
    filter(k != "", !(k %in% c("n/a", "na"))) |> pull(k) |> unique()
  fld <- names(df)[startsWith(names(df), "field:")]
  unknown_cols <- fld[!(tolower(trimws(sub("^field:", "", fld))) %in% known)]
  if (!length(unknown_cols))
    return(tibble(field = character(), n_obs = integer(), n_our = integer()))
  our <- df$observer %in% our_users
  res <- purrr::map_dfr(unknown_cols, function(col) {
    ne <- !is.na(df[[col]]) & df[[col]] != ""
    tibble(field = sub("^field:", "", col), n_obs = sum(ne), n_our = sum(ne & our))
  }) |> filter(n_our > 0) |> arrange(desc(n_our), desc(n_obs))

  # attach real iNat field IDs if the map has been built (build_field_id_map.R)
  idmap_path <- "data/project_info/inat_field_id_map.csv"
  if (nrow(res) && file.exists(idmap_path)) {
    idmap <- read_csv(idmap_path, show_col_types = FALSE) |>
      transmute(k = tolower(trimws(field_name)), inat_field_id)
    res <- res |> mutate(k = tolower(trimws(field))) |>
      left_join(idmap, by = "k") |> select(-k) |>
      relocate(inat_field_id, .after = field)
  }
  res
}

# ------------------------------------------------------------
# Notes to review: our-surveyor observations whose free-text description holds a
# survey keyword (10-min, times, weather, transect, "bee survey"...). These may
# carry survey info that isn't structured into a tag/field yet.
# ------------------------------------------------------------
fpi_unknown_notes <- function(df, our_users) {
  kw <- "10\\s*-?\\s*min|start time|end time|\\bweather\\b|\\bwind\\b|temperature|transect|bee survey|cabrillo"
  df |>
    filter(observer %in% our_users, !is.na(description), description != "") |>
    mutate(note = str_squish(description), hit = str_extract_all(tolower(note), kw)) |>
    filter(lengths(hit) > 0) |>
    transmute(obs_id, observer, observed_on, url,
              keywords = vapply(hit, function(h) paste(sort(unique(h)), collapse = "; "), character(1)),
              note = substr(note, 1, 200)) |>
    arrange(observer, observed_on)
}

# ------------------------------------------------------------
# in_cabr via the real boundary (point-in-polygon). Rows with missing coords
# get in_cabr = NA (can't place them).
# ------------------------------------------------------------
fpi_in_cabr <- function(base, boundary_path) {
  boundary <- st_read(boundary_path, quiet = TRUE)
  has_xy <- !is.na(base$latitude) & !is.na(base$longitude)
  res <- rep(NA, nrow(base))
  pts <- st_as_sf(base[has_xy, ], coords = c("longitude", "latitude"), crs = 4326) |>
    st_transform(st_crs(boundary))
  inside <- lengths(st_within(pts, boundary)) > 0
  res[has_xy] <- inside
  as.logical(res)
}

# ------------------------------------------------------------
# Assemble the membership table.
# ------------------------------------------------------------
fpi_membership <- function(base, signals, roster, boundary_path) {
  roster_lk <- roster |>
    transmute(year = as.integer(year), observer = inaturalist_username, role) |>
    filter(!is.na(observer), observer != "") |>
    distinct(observer, year, .keep_all = TRUE)

  base |>
    left_join(signals, by = "obs_id") |>
    mutate(across(c(has_survey, is_exclude, is_10min, is_metadata), ~ coalesce(., FALSE)),
           in_cabr = fpi_in_cabr(base, boundary_path),
           year = as.integer(format(observed_on, "%Y"))) |>
    left_join(roster_lk, by = c("observer", "year")) |>
    mutate(
      status = case_when(is_exclude ~ "exclude",
                         has_survey ~ "keep",
                         coalesce(in_cabr, FALSE) ~ "flag",
                         TRUE ~ "not_survey"),
      survey_type  = if_else(status == "keep", coalesce(role, "unknown"), NA_character_),
      survey_year  = if_else(status == "keep", as.character(year), NA_character_),
      status_reason = case_when(
        status == "exclude" ~ paste0("exclude tag: ", exclude_tag),
        status == "keep" & survey_type == "unknown" ~ "tagged, but observer not in roster for this year -> onboard",
        status == "keep" ~ "valid Cabrillo survey tag",
        status == "flag" ~ "in CABR, no survey tag -- review",
        TRUE ~ "outside CABR, no survey tag")
    ) |>
    transmute(obs_id, kind, observer, observed_on, survey_type, survey_year,
              transect, is_10min, is_metadata, status, status_reason, in_cabr) |>
    arrange(observed_on, obs_id)
}

# ------------------------------------------------------------
# survey_dates.csv -- observation-driven for iNat users (beeple), calendar
# windows for planned-but-untagged, and the intern schedule PRESERVED from the
# existing survey_dates.csv (the `source == "intern-log"` rows). Built in place.
# ------------------------------------------------------------
fpi_survey_dates <- function(membership, windows, roster, existing_path = FPI_SURVEY_DATES) {
  keep <- membership |> filter(status == "keep", coalesce(in_cabr, FALSE))

  ros_b_name <- roster |> filter(role == "beeple") |>
    transmute(inat_username = na_if(inaturalist_username, ""), first_name, last_name,
              method, technique) |>
    filter(!is.na(inat_username)) |> distinct(inat_username, .keep_all = TRUE)

  # per (username,date) keep-obs summary
  obs_day <- keep |>
    group_by(inat_username = observer, date = observed_on) |>
    summarise(survey_type = dplyr::first(survey_type),
              transect = dplyr::first(stats::na.omit(transect)),
              n_obs = dplyr::n(), .groups = "drop")

  # ----- interns: read the schedule from survey_dates.csv (EITHER schema) -----
  # survey_dates.csv may be the full master (has a `source` col -> keep its
  # intern-log rows) OR the simple hand-kept intern schedule (year,date,
  # first_name,role,method,...). Handle both so a re-run never breaks.
  existing <- if (file.exists(existing_path)) read_csv(existing_path, show_col_types = FALSE) else NULL
  interns <- if (!is.null(existing) && "source" %in% names(existing)) {
    existing |> filter(source == "intern-log") |>
      mutate(date = as.Date(date), window_start = as.Date(window_start),
             window_end = as.Date(window_end)) |>
      select(any_of(SD_COLUMNS))
  } else if (!is.null(existing) && all(c("date", "first_name", "role", "method") %in% names(existing))) {
    ros_i <- roster |> filter(role == "intern") |>
      transmute(year = as.integer(year), first_name, last_name,
                inat_username = na_if(inaturalist_username, ""), technique) |>
      distinct(year, first_name, .keep_all = TRUE)
    existing |>
      mutate(year = as.integer(year), date = as.Date(date)) |>
      left_join(ros_i, by = c("year", "first_name")) |>
      left_join(obs_day |> select(inat_username, date, n_hit = n_obs),
                by = c("inat_username", "date")) |>
      mutate(
        role = "intern", source = "intern-log",
        window_start = as.Date(NA), window_end = as.Date(NA),
        transect = if_else(!is.na(transects_surveyed) & transects_surveyed != "",
                           transects_surveyed, NA_character_),
        is_training = !is.na(notes) & str_detect(str_to_lower(notes), "training"),
        uses_inat = method == "non-lethal" & !is.na(inat_username),
        n_obs = if_else(uses_inat, coalesce(n_hit, 0L), NA_integer_),
        confirmed = case_when(uses_inat ~ n_obs > 0, method == "lethal" ~ TRUE, TRUE ~ NA),
        note = case_when(
          is_training ~ "training day -- exclude from analysis",
          method == "lethal" ~ "from intern log; net survey, no iNaturalist",
          uses_inat & n_obs == 0 ~ "no Cabrillo-tagged obs found on this date",
          method == "non-lethal" & is.na(inat_username) ~ "no iNaturalist username on file",
          TRUE ~ NA_character_)
      ) |>
      select(any_of(SD_COLUMNS))
  } else {
    warning("survey_dates.csv missing or unrecognized schema -- no intern rows this run.")
    tibble()
  }

  # ----- beeple (observation-driven) -----
  wu <- windows |>
    mutate(year = as.integer(year), window_start = as.Date(window_start),
           window_end = as.Date(window_end)) |>
    left_join(roster |> filter(role == "beeple") |>
                transmute(year = as.integer(year), first_name,
                          inat_username = na_if(inaturalist_username, "")),
              by = c("year", "first_name"))

  bdays <- obs_day |> filter(survey_type %in% c("beeple", "unknown"))

  day_win <- bdays |> select(inat_username, date) |>
    inner_join(wu |> filter(!is.na(inat_username)) |>
                 select(inat_username, window_start, window_end, transect_assigned = transect),
               by = "inat_username", relationship = "many-to-many") |>
    filter(date >= window_start, date <= window_end) |>
    group_by(inat_username, date) |> slice(1) |> ungroup()

  obs_rows <- bdays |>
    left_join(day_win, by = c("inat_username", "date")) |>
    left_join(ros_b_name, by = "inat_username") |>
    mutate(year = as.integer(format(date, "%Y")),
           role = if_else(survey_type == "unknown", "unknown", "beeple"),
           method = coalesce(method, "non-lethal"), technique = coalesce(technique, "photo"),
           source = "observation", confirmed = TRUE,
           transect = coalesce(transect, transect_assigned),
           note = if_else(is.na(window_start), "surveyed outside any planned calendar window",
                          NA_character_)) |>
    select(any_of(SD_COLUMNS))

  # planned windows with no tagged obs
  win_hit <- wu |> mutate(.wid = row_number()) |>
    left_join(bdays |> select(inat_username, date), by = "inat_username",
              relationship = "many-to-many") |>
    group_by(.wid) |>
    summarise(any_obs = any(!is.na(date) & date >= window_start & date <= window_end),
              .groups = "drop")
  miss_rows <- wu |> mutate(.wid = row_number()) |>
    left_join(win_hit, by = ".wid") |>
    filter(is.na(inat_username) | !coalesce(any_obs, FALSE)) |>
    mutate(role = "beeple", method = "non-lethal", technique = "photo",
           source = "calendar-only", date = as.Date(NA), n_obs = 0L,
           confirmed = if_else(is.na(inat_username), NA, FALSE),
           note = if_else(is.na(inat_username),
                          "first name not matched to a beeple in the roster",
                          "planned window, no Cabrillo-tagged obs (skipped or untagged)")) |>
    select(any_of(SD_COLUMNS))

  bind_rows(interns, obs_rows, miss_rows) |>
    mutate(across(where(is.character), ~ na_if(.x, ""))) |>
    arrange(year, date, window_start, role, first_name) |>
    select(all_of(SD_COLUMNS))
}

# ------------------------------------------------------------
# ORCHESTRATOR
# ------------------------------------------------------------
finding_project_info <- function(write = TRUE) {
  crosswalk <- read_csv(FPI_CROSSWALK, show_col_types = FALSE)
  roster    <- read_csv(FPI_ROSTER, show_col_types = FALSE)
  windows   <- read_csv(FPI_WINDOWS, show_col_types = FALSE)
  tagmap    <- fpi_build_tagmap(crosswalk)

  # load + normalize each export (bee now; plant later)
  base <- purrr::map_dfr(FPI_EXPORTS, function(s) {
    x <- readRDS(s$path)
    x$obs_id <- x$id; x$kind <- s$kind
    x |> mutate(observer = user_login, observed_on = as.Date(observed_on)) |>
      select(obs_id, kind, observer, observed_on, latitude, longitude, url,
             tag_list, description, starts_with("field:"))
  })

  sig <- fpi_signals(base, tagmap)
  membership <- fpi_membership(base, sig$signals, roster, FPI_BOUNDARY)
  survey_dates <- fpi_survey_dates(membership, windows, roster)

  # Step-2a review reports -- all scoped to OUR surveyors (a stranger's tag/field
  # isn't ours to categorize). Three separate files: hashtags, obs-field names, notes.
  our_users <- roster$inaturalist_username |> na.omit() |> unique()
  our_users <- our_users[our_users != ""]
  unknown_tags <- sig$unknown_tags |>
    left_join(base |> select(obs_id, observer), by = "obs_id") |>
    filter(observer %in% our_users) |>
    group_by(tag = tok) |>
    summarise(n_obs = dplyr::n(),
              surveyors = paste(sort(unique(observer)), collapse = "; "), .groups = "drop") |>
    mutate(cabrillo_ish = grepl("cabrill|beesurvey|beemonitor|monument|upmon|bst|transect|10min|beeple",
                                tolower(tag))) |>
    arrange(desc(cabrillo_ish), desc(n_obs))
  unknown_fields <- fpi_unknown_fields(base, crosswalk, our_users)
  unknown_notes  <- fpi_unknown_notes(base, our_users)

  # QC: missing-tag (in CABR, on a confirmed survey day, no tag)
  survey_days <- survey_dates |>
    filter(!is.na(date), confirmed == TRUE, source %in% c("observation", "intern-log")) |>
    transmute(user = inat_username, date = as.Date(date)) |>
    filter(!is.na(user)) |> distinct()
  qc_untagged <- membership |>
    filter(status == "flag", coalesce(in_cabr, FALSE)) |>
    transmute(obs_id, user = observer, date = observed_on) |>
    inner_join(survey_days, by = c("user", "date")) |>
    left_join(base |> select(obs_id, url), by = "obs_id") |>
    arrange(date, user)

  # QC: misplaced (tagged/keep but outside CABR)
  qc_misplaced <- membership |>
    filter(status == "keep", !coalesce(in_cabr, TRUE)) |>
    left_join(base |> select(obs_id, url, latitude, longitude), by = "obs_id") |>
    transmute(obs_id, observer, observed_on, survey_type, latitude, longitude, url,
              flag = "Cabrillo-tagged but outside CABR box -- check coordinates")

  if (write) {
    dir.create(dirname(FPI_QC_UNTAGGED), recursive = TRUE, showWarnings = FALSE)
    write.csv(membership,     FPI_MEMBERSHIP,     row.names = FALSE, na = "")
    write.csv(survey_dates,   FPI_SURVEY_DATES,   row.names = FALSE, na = "")
    write.csv(unknown_tags,   FPI_UNKNOWN_TAGS,   row.names = FALSE, na = "")
    write.csv(unknown_fields, FPI_UNKNOWN_FIELDS, row.names = FALSE, na = "")
    write.csv(unknown_notes,  FPI_UNKNOWN_NOTES,  row.names = FALSE, na = "")
    write.csv(qc_untagged,    FPI_QC_UNTAGGED,    row.names = FALSE, na = "")
    write.csv(qc_misplaced,   FPI_QC_MISPLACED,   row.names = FALSE, na = "")
    message("Wrote:")
    message("  project_unclean_bee_observations.csv  ", nrow(membership),     " rows")
    message("  survey_dates.csv                      ", nrow(survey_dates),   " rows")
    message("  crosswalk_unknown_bee_tags.csv        ", nrow(unknown_tags),   " hashtags to review")
    message("  crosswalk_unknown_bee_fields.csv      ", nrow(unknown_fields), " obs-field names to review")
    message("  crosswalk_unknown_bee_notes.csv       ", nrow(unknown_notes),  " notes to review")
    message("  survey_untagged_bee_observations.csv  ", nrow(qc_untagged),    " rows")
    message("  survey_misplaced_bee_observations.csv ", nrow(qc_misplaced),   " rows")
  }
  invisible(list(membership = membership, survey_dates = survey_dates,
                 unknown_tags = unknown_tags, unknown_fields = unknown_fields,
                 unknown_notes = unknown_notes, qc_untagged = qc_untagged,
                 qc_misplaced = qc_misplaced))
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) finding_project_info()
