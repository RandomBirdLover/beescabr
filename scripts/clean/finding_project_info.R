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
# the `source == "intern-log"` rows (the ground truth for interns -- BOTH lethal net
# days AND non-lethal iNat days). Each run PRESERVES those rows and rebuilds the beeple
# rows around them, so survey_dates.csv is built upon in place. Edit intern dates
# directly in survey_dates.csv.
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

# transect resolver (majority rule) -- defines resolve_transects(); guarded so the
# brain still runs if the file isn't present.
if (file.exists("scripts/clean/survey_transects.R")) source("scripts/clean/survey_transects.R")

# ---- paths ----
# Every export listed here is pooled through the SAME membership + survey-date
# logic, tagged by `kind`. The plant export is built by ingest_plants.R; it's
# safe to list before the first plant pull -- an absent path is skipped below.
FPI_EXPORTS   <- list(
  list(path = "data/cache/export_flat.rds",       kind = "bee"),
  list(path = "data/cache/export_flat_plant.rds", kind = "plant")
)
FPI_CROSSWALK <- "data/project_info/crosswalk_master.csv"
FPI_ROSTER    <- "data/project_info/surveyors_by_year.csv"
FPI_WINDOWS   <- "data/project_info/beeple_calendar_windows.csv"
FPI_BOUNDARY  <- "data/spatial/boundaries/cabr/cabr_survey_box.shp"

FPI_MEMBERSHIP     <- "data/project_info/project_unclean_bee_observations.csv"  # the per-obs lookup
FPI_SURVEY_DATES   <- "data/project_info/survey_dates.csv"
FPI_REVIEW         <- "data/project_info/survey_windows_to_review.csv"   # beeple windows to rule on (persistent)
FPI_MISTAGS        <- "data/project_info/survey_mistagged_transect_obs.csv"  # stray transect tags outvoted by the day's majority
FPI_TIES           <- "data/project_info/survey_transect_ties_to_review.csv"  # equal-split days to rule (review_transect_ties)
FPI_UNKNOWN_TAGS   <- "data/project_info/crosswalk_unknown_bee_tags.csv"    # unknown hashtags
FPI_UNKNOWN_FIELDS <- "data/project_info/crosswalk_unknown_bee_fields.csv"  # unknown obs-field NAMES
FPI_UNKNOWN_NOTES  <- "data/project_info/crosswalk_unknown_bee_notes.csv"   # notes w/ survey keywords
SD_COLUMNS <- c("year", "role", "source", "date",
                "transects", "surveyors", "inat_username", "method", "technique",
                "confirmed", "confirmed_by", "n_obs", "n_days", "note")

# survey-date tuning:
#   * tol_days -- a planned survey may drift this many days from its calendar window and
#     still count as "covered" by a nearby tagged survey (people survey a week early/late).
SD_WINDOW_TOL_DAYS <- 10L

fpi_norm <- function(s) tolower(gsub("^#", "", trimws(s)))

# Split a ";"/"," -delimited variant list WITHOUT splitting on a delimiter that
# sits inside parentheses. FOR FIELD VARIANTS ONLY -- some iNat field NAMES embed
# their allowed values, e.g. "soil type (sandy; loam; clay)" is ONE name, not three,
# so a plain split would shred them. TAGS must NOT use this: their parenthetical
# common names ("Sweat Bee (Lasioglossum sp)") sit beside standalone tags and must
# stay split, or tags like bokeh/ceratina resurface as "unknown".
fpi_split_variants <- function(s) {
  s <- as.character(s)
  if (length(s) != 1L) return(unlist(lapply(s, fpi_split_variants), use.names = FALSE))
  if (is.na(s) || !nzchar(trimws(s))) return(character(0))
  out <- character(0); buf <- ""; depth <- 0L
  for (ch in strsplit(s, "", fixed = TRUE)[[1]]) {
    if (ch == "(") depth <- depth + 1L
    else if (ch == ")") depth <- if (depth > 0L) depth - 1L else 0L
    if (depth == 0L && (ch == ";" || ch == ",")) { out <- c(out, buf); buf <- "" }
    else buf <- paste0(buf, ch)
  }
  out <- trimws(c(out, buf)); out[nzchar(out)]
}

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
    separate_rows(variants, sep = "[;,]\\s*") |>   # TAGS: plain split (NOT paren-aware)
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
    mutate(v = lapply(v, fpi_split_variants)) |>
    tidyr::unnest(v) |>
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
              transect, is_10min, is_metadata, status, status_reason, in_cabr, obscured) |>
    arrange(observed_on, obs_id)
}

# ------------------------------------------------------------
# survey_dates.csv  +  survey_windows_to_review.csv  (tag-first rewrite 2026-07-17)
# Survey dates for BOTH methods (lethal net + non-lethal iNaturalist) and BOTH roles
# (intern + beeple):
#   * INTERNS (lethal net AND non-lethal iNat) -> PRESERVED as-is from survey_dates.csv
#     (the source=="intern-log" rows). We never invent OR regenerate them; edit them
#     there. See the TODO in the body -- interns are PAID, so an authoritative date
#     should always exist.
#   * BEEPLE -> rebuilt TAG-FIRST: every Cabrillo-TAGGED obs by a beeple (roster role
#     that year) is a real survey that day. role/method/technique come from the roster.
#     The tag is the evidence: no calendar match, no location test, no minimum count, so
#     a thin winter day (1 bee + a few plants, or plant-only) still counts. One row per
#     surveyor per DAY; transects listed. confirmed_by = "tag". Interns are NOT rebuilt
#     here (preserved above) -- regenerating them from tags would double-count.
#   * the beeple CALENDAR is only the PLAN, used to catch MISSING surveys. A planned
#     window is "covered" if ANY tagged survey (anyone, any transect) lands within
#     tol_days of it -- people covered shifts and swapped transects. Windows with NO
#     survey evidence nearby -> survey_windows_to_review.csv ("planned, nothing tagged
#     -- did it happen?"). HEADS-UP ONLY: ruling a window does NOT add a survey -- nothing
#     is ever hand-added to survey_dates (no tag = not a survey day).
#     NOTE: this catches missing DATES, not a specific missing transect (see PITFALLS).
#     (The transect-coordinate QC moved to qc_misplaced_transect.R, run by the clean scripts.)
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
                             existing_path = FPI_SURVEY_DATES, review_path = FPI_REVIEW,
                             tol_days = SD_WINDOW_TOL_DAYS) {
  blank <- function(x) is.na(x) | trimws(as.character(x)) == ""

  # ---- INTERNS: preserved as-is from survey_dates.csv (people edit them there) ----
  # >>> TODO -- FIND THE SURVEY DATES THEY WERE HIRED FOR <<<
  # Interns are PAID for their survey days, so an authoritative date should always exist.
  # They live IN survey_dates.csv as the source=="intern-log" rows -- BOTH lethal net days
  # AND non-lethal iNat days. The brain NEVER invents or regenerates them: it carries every
  # source=="intern-log" row forward UNCHANGED and rebuilds only the beeple rows around
  # them. Add / fix intern surveys by editing survey_dates.csv.
  interns <- tibble()
  if (file.exists(existing_path)) {
    ex <- suppressWarnings(read_csv(existing_path, show_col_types = FALSE))
    if ("source" %in% names(ex)) {
      it <- ex |> filter(source == "intern-log")
      if (nrow(it) > 0) {
        for (col in SD_COLUMNS) if (!col %in% names(it)) it[[col]] <- NA
        interns <- it |>
          mutate(date = as.Date(date), confirmed = as.logical(confirmed)) |>
          select(any_of(SD_COLUMNS))
      }
    }
  }

  # ---- ROSTER lookup: role / method / technique per (username, year), + any-year gate ----
  ros <- roster |>
    transmute(year = as.integer(year), first_name,
              uname = ifelse(blank(inaturalist_username), NA_character_, trimws(inaturalist_username)),
              role = tolower(trimws(role)),
              method = coalesce(method, "non-lethal"), technique = coalesce(technique, "photo")) |>
    filter(!is.na(uname), uname != "")
  ros_yr  <- ros |> distinct(uname, year, .keep_all = TRUE)              # that-year role/method
  ros_any <- ros |> distinct(uname, .keep_all = TRUE) |>                 # fallback if that year missing
    transmute(uname, a_first = first_name, a_role = role,
              a_method = method, a_technique = technique)
  known_unames <- unique(ros$uname)   # every roster username (any year) -- the "one of ours" gate

  # ---- TAGGED BEEPLE SURVEYS -- the rebuilt non-lethal beeple record ----
  # Every Cabrillo-TAGGED obs by a BEEPLE (roster role FOR THAT YEAR) is a real survey that
  # day. The tag is the evidence -- no calendar, no location test, no minimum count. One row
  # per surveyor per DAY; transects listed. INTERNS are NOT rebuilt here -- their tagged days
  # are already preserved from survey_dates.csv above, so regenerating them from tags would
  # double-count. Scoped by that-year role because someone can be intern one year, beeple
  # another. A non-roster tag can't fake a survey (there are none in the data anyway).
  tagged <- membership |>
    filter(status == "keep") |>
    transmute(obs_id, uname = observer, date = as.Date(observed_on),
              yr = suppressWarnings(as.integer(format(as.Date(observed_on), "%Y"))),
              tr = fpi_norm_transect(transect)) |>
    filter(!is.na(uname), !is.na(date), uname %in% known_unames) |>
    left_join(ros_yr  |> select(uname, year, yr_role = role), by = c("uname", "yr" = "year")) |>
    left_join(ros_any |> select(uname, any_role = a_role), by = "uname") |>
    filter(coalesce(yr_role, any_role) == "beeple")   # BEEPLE only; interns preserved above

  tagged_sd <- tagged |>
    group_by(uname, date) |>
    summarise(yr = dplyr::first(yr), n_obs = dplyr::n(),
              transects = { tt <- sort(unique(na.omit(tr)))
                            if (length(tt)) paste(tt, collapse = "; ") else NA_character_ },
              .groups = "drop") |>
    left_join(ros_yr |> select(uname, year, first_name, role, method, technique),
              by = c("uname", "yr" = "year")) |>
    left_join(ros_any, by = "uname") |>
    transmute(year = yr, role = coalesce(role, a_role, "beeple"),
              source = "inat-tag", date,
              transects, surveyors = coalesce(first_name, a_first, uname),
              inat_username = uname,
              method = coalesce(method, a_method, "non-lethal"),
              technique = coalesce(technique, a_technique, "photo"),
              confirmed = TRUE, confirmed_by = "tag", n_obs, n_days = 1L,
              note = NA_character_) |>
    select(any_of(SD_COLUMNS))

  # ---- MISSING-SURVEY REVIEW (who- and transect-BLIND) ----
  # The beeple calendar is the PLAN. A planned window is "covered" if ANY tagged survey
  # date (anyone, any transect) falls within tol_days of it -- people covered shifts and
  # swapped transects, so we only ask "did a survey happen near then?", not "did THIS
  # person do THIS transect?". Windows with zero survey evidence nearby surface for a
  # human. (Trade-off: catches missing DATES, not a specific dropped transect -- PITFALLS.)
  all_survey_dates <- sort(unique(c(tagged_sd$date,
                                    if (nrow(interns)) as.Date(interns$date) else as.Date(character(0)))))
  nearest_gap <- function(ws, we) {
    if (!length(all_survey_dates) || is.na(ws) || is.na(we)) return(NA_integer_)
    min(pmax(0L, as.integer(ws - all_survey_dates), as.integer(all_survey_dates - we)))
  }
  wrole <- roster |>
    transmute(year = as.integer(year), first_name,
              uname = ifelse(blank(inaturalist_username), NA_character_, trimws(inaturalist_username))) |>
    distinct(year, first_name, .keep_all = TRUE)
  w <- windows |>
    mutate(year = as.integer(year),
           window_start = as.Date(window_start), window_end = as.Date(window_end),
           transect = fpi_norm_transect(transect)) |>
    left_join(wrole, by = c("year", "first_name"))
  w$nearest <- if (nrow(w)) vapply(seq_len(nrow(w)),
                                   function(i) nearest_gap(w$window_start[i], w$window_end[i]),
                                   integer(1)) else integer(0)

  review <- w |>
    filter(is.na(nearest) | nearest > tol_days, window_start <= Sys.Date()) |>
    transmute(year, first_name, inat_username = uname,
              window_start, window_end, transect,
              review_reason = "no-survey-near",
              suggestion = paste0("SUGGEST NO -- no tagged survey by anyone within ",
                                  tol_days, " days of this planned window"),
              n_obs_in_window = 0L,
              decision = NA_character_, decision_note = NA_character_) |>
    arrange(year, first_name, window_start)

  # persist prior rulings by window key (only NEW windows resurface)
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

  # ---- survey_dates = interns (preserved) + beeple (tag-first). NOTHING is hand-added:
  # a review window ruled "survey" is NOT injected -- no tag means it's not a survey day.
  # The review queue is a heads-up only (which planned windows have no tagged survey).
  survey_dates <- bind_rows(interns, tagged_sd) |>
    mutate(across(where(is.character), ~ na_if(.x, ""))) |>
    arrange(year, date, role, surveyors) |>
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

  # load + normalize each export (bee + plant). A listed-but-absent export (e.g.
  # plant, before the first plant pull) is skipped -- map_dfr drops the NULL.
  base <- purrr::map_dfr(FPI_EXPORTS, function(s) {
    if (!file.exists(s$path)) {
      message("  (export absent, skipping ", s$kind, ": ", s$path, ")")
      return(NULL)
    }
    x <- readRDS(s$path)
    x$obs_id <- x$id; x$kind <- s$kind
    if (!"coordinates_obscured" %in% names(x)) x$coordinates_obscured <- FALSE
    x |> mutate(observer = user_login, observed_on = as.Date(observed_on),
                obscured = coalesce(as.logical(coordinates_obscured), FALSE)) |>
      select(obs_id, kind, observer, observed_on, obscured, latitude, longitude, url,
             tag_list, description, starts_with("field:"))
  })

  sig <- fpi_signals(base, tagmap)
  membership <- fpi_membership(base, sig$signals, roster, FPI_BOUNDARY)
  # resolve each beeple survey day to its MAJORITY transect (stamps every obs of that
  # surveyor+day; interns untouched). Feeds survey_dates a single, clean transect.
  mistags <- NULL; ties <- NULL
  if (exists("resolve_transects")) {
    rt <- resolve_transects(membership); membership <- rt$membership
    mistags <- rt$mistags; ties <- rt$ties
  }
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
    if (!is.null(mistags)) write.csv(mistags, FPI_MISTAGS, row.names = FALSE, na = "")
    if (!is.null(ties))    write.csv(ties,    FPI_TIES,    row.names = FALSE, na = "")
    write.csv(unknown_tags,   FPI_UNKNOWN_TAGS,   row.names = FALSE, na = "")
    write.csv(unknown_fields, FPI_UNKNOWN_FIELDS, row.names = FALSE, na = "")
    write.csv(unknown_notes,  FPI_UNKNOWN_NOTES,  row.names = FALSE, na = "")
    message("Wrote:")
    message("  project_unclean_bee_observations.csv  ", nrow(membership),     " rows")
    message("  survey_dates.csv                      ", nrow(survey_dates),   " confirmed surveys")
    message("  survey_windows_to_review.csv          ", nrow(review_windows), " windows to review")
    if (!is.null(mistags)) message("  survey_mistagged_transect_obs.csv     ", nrow(mistags), " stray transect tags to fix")
    if (!is.null(ties) && nrow(ties)) message("  survey_transect_ties_to_review.csv    ", nrow(ties), " tie day(s) to rule")
    message("  crosswalk_unknown_bee_tags.csv        ", nrow(unknown_tags),   " hashtags to review")
    message("  crosswalk_unknown_bee_fields.csv      ", nrow(unknown_fields), " obs-field names to review")
    message("  crosswalk_unknown_bee_notes.csv       ", nrow(unknown_notes),  " notes to review")
  }
  invisible(list(membership = membership, survey_dates = survey_dates, review_windows = review_windows,
                 mistags = mistags, ties = ties, unknown_tags = unknown_tags, unknown_fields = unknown_fields,
                 unknown_notes = unknown_notes))
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) finding_project_info()
