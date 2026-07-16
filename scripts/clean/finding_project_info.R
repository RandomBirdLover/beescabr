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
FPI_REVIEW         <- "data/project_info/survey_windows_to_review.csv"   # beeple windows to rule on (persistent)
FPI_UNKNOWN_TAGS   <- "data/project_info/crosswalk_unknown_bee_tags.csv"    # unknown hashtags
FPI_UNKNOWN_FIELDS <- "data/project_info/crosswalk_unknown_bee_fields.csv"  # unknown obs-field NAMES
FPI_UNKNOWN_NOTES  <- "data/project_info/crosswalk_unknown_bee_notes.csv"   # notes w/ survey keywords
SD_COLUMNS <- c("year", "role", "source", "date", "window_start", "window_end",
                "transects", "surveyors", "inat_username", "method", "technique",
                "confirmed", "confirmed_by", "n_obs", "n_days", "note")

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
# survey_dates.csv  +  survey_windows_to_review.csv   (rewritten 2026-07-16)
#   * interns -> PRESERVED as-is from survey_dates.csv (source=="intern-log").
#     People maintain intern dates by editing survey_dates.csv directly; the
#     brain never invents them -- it just carries them forward + rebuilds beeple.
#   * beeple  -> ONE row per calendar WINDOW. Confirmed if the assigned surveyor
#     has, inside the window: a Cabrillo-tagged obs (confirmed_by="tag") OR --
#     forgot-to-tag -- an in-CABR obs with NO survey tag (status=="flag";
#     confirmed_by="surveyor+window"). n_days flags multi-day windows.
#   * windows with neither -> survey_windows_to_review.csv (empty / off-site /
#     excluded-in-box / no-username) for a by-hand ruling; prior `decision`
#     cells persist across runs so only NEW windows resurface.
# survey_dates.csv = CONFIRMED surveys only. Returns list(survey_dates, review).
# ------------------------------------------------------------
fpi_norm_transect <- function(x) {
  u <- toupper(gsub("^#", "", trimws(as.character(x))))
  dplyr::case_when(
    u %in% c("", "NA", "N/A") ~ NA_character_,
    startsWith(u, "TP")       ~ "TP",     # TP / TP1 / TP2 -> TP (tidepools merged)
    startsWith(u, "UPMON")    ~ "UPMON",
    startsWith(u, "BST")      ~ "BST",
    u == "OT"                 ~ "OT",
    TRUE                      ~ u
  )
}

fpi_survey_dates <- function(membership, windows, roster,
                             existing_path = FPI_SURVEY_DATES, review_path = FPI_REVIEW) {
  blank <- function(x) is.na(x) | trimws(as.character(x)) == ""

  # ---- INTERNS: preserved as-is from survey_dates.csv (people edit them there) ----
  # We NEVER invent intern dates. Whatever `source == "intern-log"` rows already
  # exist in survey_dates.csv are carried forward unchanged; only beeple is
  # rebuilt around them. Add / fix intern surveys by editing survey_dates.csv.
  interns <- tibble()
  if (file.exists(existing_path)) {
    ex <- suppressWarnings(read_csv(existing_path, show_col_types = FALSE))
    if ("source" %in% names(ex)) {
      it <- ex |> filter(source == "intern-log")
      if (nrow(it) > 0) {
        # tolerate the pre-rework schema (first_name / transect) on the first run
        if (!"surveyors" %in% names(it) && "first_name" %in% names(it)) it$surveyors <- it$first_name
        if (!"transects" %in% names(it) && "transect"   %in% names(it)) it$transects <- it$transect
        for (col in SD_COLUMNS) if (!col %in% names(it)) it[[col]] <- NA
        interns <- it |>
          mutate(role = "intern", source = "intern-log",
                 date = as.Date(date), window_start = as.Date(NA), window_end = as.Date(NA),
                 training = !is.na(note) & grepl("training", tolower(note)),
                 confirmed = coalesce(as.logical(confirmed), !training),
                 confirmed_by = coalesce(as.character(confirmed_by),
                                         if_else(training, NA_character_, "log"))) |>
          select(any_of(SD_COLUMNS))
      }
    }
  }

  # ---- BEEPLE: one row per window ----
  ros_b <- roster |> filter(tolower(role) == "beeple") |>
    transmute(year = as.integer(year), first_name,
              uname = ifelse(blank(inaturalist_username), NA_character_, trimws(inaturalist_username)),
              method = coalesce(method, "non-lethal"), technique = coalesce(technique, "photo")) |>
    distinct(year, first_name, .keep_all = TRUE)

  w <- windows |>
    mutate(year = as.integer(year),
           window_start = as.Date(window_start), window_end = as.Date(window_end),
           transect = fpi_norm_transect(transect)) |>
    left_join(ros_b, by = c("year", "first_name")) |>
    mutate(.wid = row_number())

  mem <- membership |>
    transmute(uname = observer, d = as.Date(observed_on),
              is_keep = status == "keep", is_flag = status == "flag",
              in_cabr = coalesce(in_cabr, FALSE))

  in_win <- w |> filter(!is.na(uname)) |>
    select(.wid, uname, window_start, window_end) |>
    inner_join(mem, by = "uname", relationship = "many-to-many") |>
    filter(d >= window_start, d <= window_end)

  conf <- in_win |> filter(is_keep | is_flag) |>
    group_by(.wid) |>
    mutate(has_keep = any(is_keep)) |>
    filter(if_else(has_keep, is_keep, is_flag & !is_keep)) |>
    summarise(confirmed_by = if_else(any(has_keep), "tag", "surveyor+window"),
              date = min(d), n_days = n_distinct(d), n_obs = dplyr::n(),
              .groups = "drop")

  beeple <- w |>
    inner_join(conf, by = ".wid") |>
    transmute(year, role = "beeple", source = "beeple-window", date,
              window_start, window_end, transects = transect,
              surveyors = first_name, inat_username = uname,
              method, technique, confirmed = TRUE, confirmed_by, n_obs, n_days,
              note = if_else(n_days > 1,
                             paste0("multi-day window (", n_days, " days) -- verify one survey or several"),
                             NA_character_)) |>
    select(any_of(SD_COLUMNS))

  # ---- REVIEW: windows with no confirming obs ----
  any_in_win <- in_win |> group_by(.wid) |>
    summarise(n_any = dplyr::n(), n_incabr = sum(in_cabr), .groups = "drop")
  review <- w |> filter(!(.wid %in% conf$.wid)) |>
    left_join(any_in_win, by = ".wid") |>
    transmute(year, first_name, inat_username = uname,
              window_start, window_end, transect,
              review_reason = case_when(
                is.na(uname)                ~ "no-username",
                coalesce(n_any, 0L) == 0    ~ "empty",
                coalesce(n_incabr, 0L) == 0 ~ "off-site",
                TRUE                        ~ "excluded-in-box"),
              n_obs_in_window = coalesce(n_any, 0L),
              decision = NA_character_, decision_note = NA_character_) |>
    arrange(year, first_name, window_start)

  # persist prior rulings by window key
  if (file.exists(review_path)) {
    prior <- suppressWarnings(read_csv(review_path, show_col_types = FALSE))
    if (all(c("year","first_name","window_start","window_end","transect","decision") %in% names(prior))) {
      pd <- prior |>
        mutate(window_start = as.Date(window_start), window_end = as.Date(window_end)) |>
        filter(!blank(decision)) |>
        select(year, first_name, window_start, window_end, transect, decision, decision_note)
      review <- review |> select(-decision, -decision_note) |>
        left_join(pd, by = c("year","first_name","window_start","window_end","transect"))
    }
  }

  survey_dates <- bind_rows(interns, beeple) |>
    mutate(across(where(is.character), ~ na_if(.x, ""))) |>
    arrange(year, date, window_start, role, surveyors) |>
    select(any_of(SD_COLUMNS))

  list(survey_dates = survey_dates, review = review)
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
  sd_out         <- fpi_survey_dates(membership, windows, roster)
  survey_dates   <- sd_out$survey_dates
  review_windows <- sd_out$review

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

  if (write) {
    dir.create(dirname(FPI_MEMBERSHIP), recursive = TRUE, showWarnings = FALSE)
    write.csv(membership,     FPI_MEMBERSHIP,     row.names = FALSE, na = "")
    write.csv(survey_dates,   FPI_SURVEY_DATES,   row.names = FALSE, na = "")
    write.csv(review_windows, FPI_REVIEW,         row.names = FALSE, na = "")
    write.csv(unknown_tags,   FPI_UNKNOWN_TAGS,   row.names = FALSE, na = "")
    write.csv(unknown_fields, FPI_UNKNOWN_FIELDS, row.names = FALSE, na = "")
    write.csv(unknown_notes,  FPI_UNKNOWN_NOTES,  row.names = FALSE, na = "")
    message("Wrote:")
    message("  project_unclean_bee_observations.csv  ", nrow(membership),     " rows")
    message("  survey_dates.csv                      ", nrow(survey_dates),   " confirmed surveys")
    message("  survey_windows_to_review.csv          ", nrow(review_windows), " windows to review")
    message("  crosswalk_unknown_bee_tags.csv        ", nrow(unknown_tags),   " hashtags to review")
    message("  crosswalk_unknown_bee_fields.csv      ", nrow(unknown_fields), " obs-field names to review")
    message("  crosswalk_unknown_bee_notes.csv       ", nrow(unknown_notes),  " notes to review")
  }
  invisible(list(membership = membership, survey_dates = survey_dates, review_windows = review_windows,
                 unknown_tags = unknown_tags, unknown_fields = unknown_fields,
                 unknown_notes = unknown_notes))
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) finding_project_info()
