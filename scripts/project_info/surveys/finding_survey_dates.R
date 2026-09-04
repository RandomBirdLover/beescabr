# =============================================================
# project_info/surveys/finding_survey_dates.R
# beescabr -- builds the per-survey record (master_per_survey_info_generated.csv): one row per
# surveyor per survey DAY, BOTH methods (lethal net + non-lethal iNat) and BOTH roles
# (intern + beeple). Split out of finding_project_info.R (the brain) on 2026-07-18.
#
# Defines fpi_survey_dates() -- called by the brain AFTER membership + transect resolution --
# and its helper fpi_norm_transect(). Depends on the brain's constants (SD_COLUMNS,
# FPI_SURVEY_DATES, FPI_REVIEW, SD_WINDOW_TOL_DAYS) + finding_specimen_dates(); sourced BY the
# brain (defaults resolve at call time), not run standalone.
# =============================================================
suppressWarnings(suppressMessages({library(dplyr); library(readr); library(tibble); library(tidyr)}))
if (!exists("person_name")) source("scripts/utils/people.R")
`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------------------------------------------
# master_per_survey_info_generated.csv  +  qc_review_survey_beeple_date_windows_generated.csv  (tag-first rewrite 2026-07-17)
# Survey dates for BOTH methods (lethal net + non-lethal iNaturalist) and BOTH roles
# (intern + beeple):
#   * INTERNS (lethal net AND non-lethal iNat) -> PRESERVED as-is from master_per_survey_info_generated.csv
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
#     survey evidence nearby -> qc_review_survey_beeple_date_windows_generated.csv ("planned, nothing tagged
#     -- did it happen?"). HEADS-UP ONLY: ruling a window does NOT add a survey -- nothing
#     is ever hand-added to survey_dates (no tag = not a survey day).
#     NOTE: this catches missing DATES, not a specific missing transect (see PITFALLS).
#     (Flagging misplaced / mistagged obs is a TODO in the clean scripts -- inat_bee_clean.R /
#     inat_plant_clean.R; the old qc_misplaced_transect.R was deleted from the repo.)
# master_per_survey_info_generated.csv = CONFIRMED surveys only. Returns list(survey_dates, review).
# ------------------------------------------------------------
#' Normalise a transect code to one of the four the project uses
#'
#' @param x Transect as typed, in any of its spellings.
#' @return `"BST"`, `"UPMON"`, `"TP"`, `"OT"` or `NA`. TP1 and TP2 both become
#'   TP: the tidepools transect is one transect walked in both directions.
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

# ------------------------------------------------------------
# Surveyor names. intern-log and inat-tag rows say "First Last"; specimen-record
# rows are left VERBATIM elsewhere in this file, because that string is the
# museum label written on the specimen ("S O'Dell") and the label is the record.
#
# A first name is not an identity: two people share "Julia" (Keum, a 2024 intern;
# Showalter, a 2025-26 beeple). So expansion is scoped to the roster YEAR, and a
# token that does not resolve to exactly one person that year is left exactly as
# written -- a name a human can see and fix beats a name we guessed.
# ------------------------------------------------------------
# The beeple calendar names people by FIRST NAME only. Two surveyors are called
# Julia (Keum, a 2024 intern; Showalter, a 2025-26 beeple) and the derived roster
# spans every season, so a first name does not identify anyone on its own. Prefer
# whoever actually tagged a survey that year; if that still leaves two, return NA.
# A blank cell in the review file is a question for a human. A wrong handle is not.
sd_calendar_uname <- function(year, first_name, roster, evidence = character(0)) {
  # The calendar IS the beeple schedule, so only a beeple can be the answer. An
  # intern was never on it, whatever their first name.
  keep <- if ("role" %in% names(roster)) tolower(trimws(roster$role)) == "beeple" else TRUE
  cand <- roster$uname[keep &
                       as.integer(roster$year) == as.integer(year) &
                       tolower(trimws(roster$first_name)) == tolower(trimws(first_name))]
  cand <- unique(cand[!is.na(cand) & nzchar(cand)])
  if (length(cand) == 1L) return(cand)
  if (length(cand) > 1L) {
    seen <- cand[tolower(cand) %in% tolower(evidence)]
    if (length(seen) == 1L) return(seen)
  }
  NA_character_
}

sd_full_names <- function(x, year, roster) {
  if (!length(x)) return(x)
  yr <- rep_len(as.integer(year), length(x))
  code <- if ("collector_code" %in% names(roster)) roster$collector_code else NA_character_
  ppl <- roster |>
    transmute(ryear = as.integer(year), first_name, last_name, code = code,
              full = person_name(first_name, last_name)) |>
    filter(!is.na(full)) |>
    distinct(ryear, full, .keep_all = TRUE)
  # keys are built per YEAR: "Julia" is Julia Keum in 2024 and Julia Showalter in 2025,
  # so a roster-wide key would credit a coin flip (person_name_keys drops it entirely).
  keys <- lapply(split(ppl, ppl$ryear),
                 function(d) person_name_keys(d$full, d$first_name, d$last_name, d$code))
  SEP <- "[[:space:]]*[,;&][[:space:]]*"          # "," pairs, "&" pairs, ";" groups
  # A label sometimes names a GROUP, not a person ("CSBI Interns", 800 of 1,145 specimens).
  # Which interns were out that day was never written down, so the NAME column says plain
  # "interns". The label itself survives untouched in specimen_collector.
  GROUP <- "^(CSBI[[:space:]]+)?Interns$"
  one <- function(s, y) {
    if (is.na(s) || !nzchar(trimws(s))) return(s)
    k <- keys[[as.character(y)]]
    toks <- strsplit(s, SEP, perl = TRUE)[[1]]
    seps <- regmatches(s, gregexpr(SEP, s, perl = TRUE))[[1]]   # kept verbatim: they carry meaning
    if (!is.null(k)) {
      hit  <- unname(k[tolower(toks)])
      toks <- ifelse(is.na(hit), toks, hit)                     # no match -> left exactly as written
    }
    toks <- ifelse(grepl(GROUP, toks, ignore.case = TRUE), "interns", toks)
    paste0(toks, c(seps, ""), collapse = "")
  }
  vapply(seq_along(x), function(i) one(x[[i]], yr[[i]]), character(1))
}

#' Work out which survey each record belongs to
#'
#' @param membership Which observations belong to the project.
#' @param windows The scheduled survey windows.
#' @param roster Who was surveying.
#' @param existing_path Survey dates already settled, which are not revisited.
#' @param review_path Where to write records that matched no window.
#' @param intern_log_path Which people were interns, per year.
#' @param tol_days How many days either side of a window still counts as it.
#' @param people The roster, when already loaded.
#' @return The per-survey table, plus a review file of what did not fit.
fpi_survey_dates <- function(membership, windows, roster,
                             existing_path = FPI_SURVEY_DATES, review_path = FPI_REVIEW,
                             intern_log_path = FPI_INTERN_LOG,
                             tol_days = SD_WINDOW_TOL_DAYS,
                             people = NULL) {
  blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
  if (is.null(people)) people <- tryCatch(
    read.csv(PATHS$people, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame())

  # ---- INTERNS: read from the curated intern-survey-day LOG (master_intern_survey_log_manual.csv) ----
  # Interns' survey days (BOTH lethal net days AND non-lethal iNat days) live in a curated
  # INPUT file -- data/project_info/surveys/survey_date_sources/master_intern_survey_log_manual.csv -- as the source=="intern-log"
  # rows. The brain reads them UNCHANGED and rebuilds only the beeple rows around them; the
  # master is pure generated output. Interns are PAID, so an authoritative date should always
  # exist -- add / fix intern surveys by editing master_intern_survey_log_manual.csv (NOT the master, which is
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
    # The log stores person_ids, so a person's name is typed once (in people_manual.csv) and
    # never again. Names are rendered here for the generated master, which a human reads.
    if ("person_ids" %in% names(it) && nrow(people)) {
      it$surveyors <- people_display(it$person_ids, people)
      # inat_username is NOT derived: it says whose ACCOUNT evidences the survey, which is
      # not the same as who was there. One 2024 day has two surveyors and one tagger.
    }
    for (col in SD_COLUMNS) if (!col %in% names(it)) it[[col]] <- NA
    it |>
      mutate(date = as.Date(date), confirmed = as.logical(confirmed),
             n_obs = as.character(n_obs), n_speci = as.character(n_speci)) |>
      select(any_of(SD_COLUMNS))
  }
  interns <- read_intern_log(intern_log_path)                    # curated source of truth (preferred)
  if (!nrow(interns)) interns <- read_intern_log(existing_path)  # fallback: legacy in-master rows
  # The log is hand-typed and a first name is quick to type, but "Julia" is two people
  # in different years. Expand against the roster so the master says who it means.
  if (nrow(interns)) interns$surveyors <- sd_full_names(interns$surveyors, interns$year, roster)

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
              transects = "UPMON; TP; BST",
              # Two columns, two jobs. specimen_collector is the label string VERBATIM ("S O'Dell")
              # so a specimen stays traceable to what is written on its pin -- never rewrite it.
              # surveyors names the PERSON ("Sam O'Dell"), so one person reads the same in every
              # row of this file whichever source it came from.
              surveyors = sd_full_names(coalesce(collectors, "CSBI Interns"), year, roster),
              specimen_collector = coalesce(collectors, "CSBI Interns"),
              inat_username = NA_character_, method = "lethal", technique = "net",
              confirmed = TRUE, confirmed_by = "specimen",
              n_obs = NA_character_, n_speci = NA_character_,   # n_speci filled by the date join in assembly
              n_days = 1L, note = "from specimen record; no intern-log entry") |>
    select(any_of(SD_COLUMNS))

  # ---- ROSTER lookup: role / method / technique per (username, year), + any-year gate ----
  ros <- roster |>
    transmute(year = as.integer(year), first_name, last_name,
              uname = ifelse(blank(inaturalist_username), NA_character_, trimws(inaturalist_username)),
              role = tolower(trimws(role)),
              method = coalesce(method, "non-lethal"), technique = coalesce(technique, "photo")) |>
    filter(!is.na(uname), uname != "")
  ros_yr  <- ros |> distinct(uname, year, .keep_all = TRUE)              # that-year role/method
  ros_any <- ros |> distinct(uname, .keep_all = TRUE) |>                 # fallback if that year missing
    transmute(uname, a_first = first_name, a_last = last_name, a_role = role,
              a_method = method, a_technique = technique)
  known_unames <- unique(ros$uname)   # every roster username (any year) -- the "one of ours" gate

  # ---- TAGGED BEEPLE SURVEYS -- the rebuilt non-lethal beeple record ----
  # Every Cabrillo-TAGGED obs by a BEEPLE (roster role FOR THAT YEAR) is a real survey that
  # day. The tag is the evidence -- no calendar, no location test, no minimum count. One row
  # per surveyor per DAY; transects listed. INTERNS are NOT rebuilt here -- their tagged days
  # are already preserved from master_per_survey_info_generated.csv above, so regenerating them from tags would
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
    left_join(ros_yr |> select(uname, year, first_name, last_name, role, method, technique),
              by = c("uname", "yr" = "year")) |>
    left_join(ros_any, by = "uname") |>
    transmute(year = yr, role = coalesce(role, a_role, "beeple"),
              source = "inat-tag", date,
              transects,
              # full name via the USERNAME join -- unambiguous even for a shared first name
              surveyors = coalesce(person_name(first_name, last_name),
                                   person_name(a_first, a_last), uname),
              inat_username = uname, specimen_collector = NA_character_,
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
    transmute(year = as.integer(year), first_name, role,
              uname = ifelse(blank(inaturalist_username), NA_character_, trimws(inaturalist_username)))
  w <- windows |>
    mutate(year = as.integer(year),
           window_start = as.Date(window_start), window_end = as.Date(window_end),
           transect = fpi_norm_transect(transect))
  # who actually tagged a survey that year -- the tie-breaker for a shared first name
  ev <- if (nrow(tagged_sd)) split(tagged_sd$inat_username, as.integer(tagged_sd$year)) else list()
  w$uname <- if (!nrow(w)) character(0) else vapply(seq_len(nrow(w)), function(i)
    sd_calendar_uname(w$year[i], w$first_name[i], wrole,
                      evidence = unique(ev[[as.character(w$year[i])]] %||% character(0))),
    character(1))
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
