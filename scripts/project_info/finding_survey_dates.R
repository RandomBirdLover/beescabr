# =============================================================
# project_info/finding_survey_dates.R
# beescabr -- builds the per-survey record (master_per_survey_info.csv): one row per
# surveyor per survey DAY, BOTH methods (lethal net + non-lethal iNat) and BOTH roles
# (intern + beeple). Split out of finding_project_info.R (the brain) on 2026-07-18.
#
# Defines fpi_survey_dates() -- called by the brain AFTER membership + transect resolution --
# and its helper fpi_norm_transect(). Depends on the brain's constants (SD_COLUMNS,
# FPI_SURVEY_DATES, FPI_REVIEW, SD_WINDOW_TOL_DAYS) + finding_specimen_dates(); sourced BY the
# brain (defaults resolve at call time), not run standalone.
# =============================================================
suppressWarnings(suppressMessages({library(dplyr); library(readr); library(tibble); library(tidyr)}))

# ------------------------------------------------------------
# master_per_survey_info.csv  +  review_beeple_survey_windows.csv  (tag-first rewrite 2026-07-17)
# Survey dates for BOTH methods (lethal net + non-lethal iNaturalist) and BOTH roles
# (intern + beeple):
#   * INTERNS (lethal net AND non-lethal iNat) -> PRESERVED as-is from master_per_survey_info.csv
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
#     survey evidence nearby -> review_beeple_survey_windows.csv ("planned, nothing tagged
#     -- did it happen?"). HEADS-UP ONLY: ruling a window does NOT add a survey -- nothing
#     is ever hand-added to survey_dates (no tag = not a survey day).
#     NOTE: this catches missing DATES, not a specific missing transect (see PITFALLS).
#     (Flagging misplaced / mistagged obs is a TODO in the clean scripts -- inat_bee_clean.R /
#     inat_plant_clean.R; the old qc_misplaced_transect.R is in _to_delete/ for reference.)
# master_per_survey_info.csv = CONFIRMED surveys only. Returns list(survey_dates, review).
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
                             intern_log_path = FPI_INTERN_LOG,
                             tol_days = SD_WINDOW_TOL_DAYS) {
  blank <- function(x) is.na(x) | trimws(as.character(x)) == ""

  # ---- INTERNS: read from the curated intern-survey-day LOG (master_intern_survey_log.csv) ----
  # Interns' survey days (BOTH lethal net days AND non-lethal iNat days) live in a curated
  # INPUT file -- data/project_info/survey_date_sources/master_intern_survey_log.csv -- as the source=="intern-log"
  # rows. The brain reads them UNCHANGED and rebuilds only the beeple rows around them; the
  # master is pure generated output. Interns are PAID, so an authoritative date should always
  # exist -- add / fix intern surveys by editing master_intern_survey_log.csv (NOT the master, which is
  # OVERWRITTEN every run: a hand-added intern row kept only there is silently wiped on the next
  # regeneration -- that is how 2024-05-05, a tagged intern iNat day that is neither in the
  # beeple tag-rebuild nor a specimen date, kept vanishing). FALLBACK: if the log file is absent,
  # read the legacy intern-log rows straight from the existing master (old in-place design).
  read_intern_log <- function(path) {
    if (is.null(path) || is.na(path) || !file.exists(path)) return(tibble())
    ex <- suppressWarnings(read_csv(path, show_col_types = FALSE))
    if (!"source" %in% names(ex)) return(tibble())
    it <- ex |> filter(source == "intern-log")
    if (!nrow(it)) return(tibble())
    for (col in SD_COLUMNS) if (!col %in% names(it)) it[[col]] <- NA
    it |>
      mutate(date = as.Date(date), confirmed = as.logical(confirmed),
             n_obs = as.character(n_obs), n_speci = as.character(n_speci)) |>
      select(any_of(SD_COLUMNS))
  }
  interns <- read_intern_log(intern_log_path)                    # curated source of truth (preferred)
  if (!nrow(interns)) interns <- read_intern_log(existing_path)  # fallback: legacy in-master rows

  # ---- SPECIMENS: per-date lethal-net specimen counts (from the specimen record) ----
  # finding_specimen_dates() returns the aggregated specimen record (date, n_specimens,
  # collectors) from the newest .xlsx, in memory. It
  # drives two things: (a) n_speci is stamped on every LETHAL survey day by date (in assembly
  # below), and (b) a NEW intern lethal row is created for each specimen date with NO intern-log
  # row yet -- a netting day the intern log missed, the specimens being the proof it happened.
  # Every specimen collector is an intern (S O'Dell / CSBI interns), so these are role=intern,
  # method=lethal. They are REGENERATED each run from the record (source=="specimen-record"), so
  # unlike intern-log rows they are not preserved -- edit the specimen .xlsx, never these rows.
  # finding_specimen_dates() aggregates the newest specimen .xlsx IN MEMORY (no CSV on disk).
  spec_lu <- tibble(date = as.Date(character()), n_speci = integer(), collectors = character())
  sp <- tryCatch(if (exists("finding_specimen_dates")) finding_specimen_dates() else NULL,
                 error = function(e) NULL)
  if (!is.null(sp) && nrow(sp) && all(c("date", "n_specimens") %in% names(sp)))
    spec_lu <- sp |> transmute(date = as.Date(date), n_speci = as.integer(n_specimens),
                               collectors = if ("collectors" %in% names(sp)) as.character(collectors) else NA_character_)
  intern_dates <- if (nrow(interns)) as.Date(interns$date) else as.Date(character(0))
  spec_rows <- spec_lu |>
    filter(!date %in% intern_dates) |>                       # specimen days with no intern-log row
    transmute(year = as.integer(format(date, "%Y")), role = "intern",
              source = "specimen-record", date,
              transects = "UPMON; TP; BST", surveyors = coalesce(collectors, "CSBI Interns"),
              inat_username = NA_character_, method = "lethal", technique = "net",
              confirmed = TRUE, confirmed_by = "specimen",
              n_obs = NA_character_, n_speci = NA_character_,   # n_speci filled by the date join in assembly
              n_days = 1L, note = "from specimen record; no intern-log entry") |>
    select(any_of(SD_COLUMNS))

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
  # are already preserved from master_per_survey_info.csv above, so regenerating them from tags would
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
    select(any_of(SD_COLUMNS)) |>
    mutate(n_obs = as.character(n_obs))

  # ---- MISSING-SURVEY REVIEW (who- and transect-BLIND) ----
  # The beeple calendar is the PLAN. A planned window is "covered" if ANY tagged survey
  # date (anyone, any transect) falls within tol_days of it -- people covered shifts and
  # swapped transects, so we only ask "did a survey happen near then?", not "did THIS
  # person do THIS transect?". Windows with zero survey evidence nearby surface for a
  # human. (Trade-off: catches missing DATES, not a specific dropped transect -- PITFALLS.)
  all_survey_dates <- sort(unique(c(tagged_sd$date, spec_lu$date,
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

  # ---- survey_dates = interns (preserved) + specimen-record intern rows + beeple (tag-first).
  # NOTHING from the window review is hand-added -- a window ruled "survey" is NOT injected (no
  # tag = not a survey day); that queue is a heads-up only. Specimen rows ARE added, but only
  # from the specimen record (hard evidence a netting day happened), never from the calendar.
  # Then n_speci is stamped by date onto LETHAL days and the method n/a rule is applied: a
  # non-lethal (photo) survey cannot have specimens (n_speci = n/a); a lethal (net) survey has
  # no iNaturalist obs (n_obs = n/a). Lethal days absent from the specimen record get blank n_speci.
  # The specimen count is a per-DATE total, so it lands on exactly ONE lethal row per date
  # (leth_i == 1) -- if two interns ever logged the same netting day, the day's total is not
  # duplicated across both rows (which would double-count on a sum).
  survey_dates <- bind_rows(interns, spec_rows, tagged_sd) |>
    mutate(date = as.Date(date)) |>
    left_join(spec_lu |> transmute(date, spec_rec = as.character(n_speci)), by = "date") |>
    group_by(date) |>
    mutate(leth_i = cumsum(coalesce(method, "") == "lethal")) |>   # rank lethal rows within a date
    ungroup() |>
    mutate(
      n_speci = dplyr::case_when(
        coalesce(method, "") == "non-lethal"                               ~ "n/a",
        coalesce(method, "") == "lethal" & leth_i == 1L & !is.na(spec_rec) ~ spec_rec,
        TRUE                                                               ~ NA_character_
      ),
      n_obs = if_else(coalesce(method, "") == "lethal", "n/a", as.character(n_obs))
    ) |>
    select(-spec_rec, -leth_i) |>
    mutate(across(where(is.character), ~ na_if(.x, ""))) |>
    arrange(year, date, role, surveyors) |>
    select(any_of(SD_COLUMNS))

  list(survey_dates = survey_dates, review = review)
}
