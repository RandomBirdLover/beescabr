# =============================================================
# project_info/build_participation.R
# beescabr -- participation_generated.csv: who was in the field, which year, in what capacity.
#   master_per_survey_info_generated.csv + people_manual.csv  ->  participation_generated.csv
#
# GENERATED. Never hand-typed, and the reason is a bug this replaced: the old
# surveyor_roster.csv was filled partly from the beeple CALENDAR, so it recorded
# who was ASSIGNED. Michael Ready had a 2022 row for six planned windows he never
# showed up to, and the 2022 headcount read 17 instead of 16.
#
# The pipeline already refuses to turn an assignment into a survey -- "the calendar
# is only the PLAN... no tag = not a survey day" (finding_survey_dates.R). This
# file applies that same rule to people: you participated if the record says you
# were there, not if a calendar said you would be.
#
# Nobody types "beeple" or "intern" anywhere. It falls out of which SOURCE the
# survey day came from: the intern log and specimen labels make an intern, a
# tagged observation makes a beeple.
# =============================================================
if (!exists("person_id_keys")) source("scripts/utils/people_ids.R")

PARTICIPATION_COLUMNS <- c("person_id", "year", "role", "method")

# participation_from_surveys(): PURE. One row per (person, year, role, method).
# A name that resolves to nobody -- a group label like "CSBI Interns" -- yields no
# row: we never invent which interns held a net that morning.
participation_from_surveys <- function(surveys, people) {
  empty <- data.frame(person_id = character(0), year = integer(0),
                      role = character(0), method = character(0), stringsAsFactors = FALSE)
  if (!nrow(surveys)) return(empty)
  keys <- person_id_keys(people)
  sq   <- function(x) trimws(ifelse(is.na(x), "", as.character(x)))
  col  <- function(nm) if (nm %in% names(surveys)) surveys[[nm]] else rep("", nrow(surveys))

  rows <- lapply(seq_len(nrow(surveys)), function(i) {
    who <- character(0)
    # the observer's own handle is the strongest evidence on a tagged row
    u <- tolower(sq(col("inat_username")[i]))
    for (h in trimws(unlist(strsplit(u, ",")))) {
      if (nzchar(h) && h != "n/a") { id <- unname(keys[h]); if (!is.na(id)) who <- c(who, id) }
    }
    who <- c(who, person_ids_in(col("surveyors")[i], people, keys))
    who <- unique(who[!is.na(who)])
    if (!length(who)) return(NULL)
    data.frame(person_id = who, year = as.integer(surveys$year[i]),
               role = sq(surveys$role[i]), method = sq(surveys$method[i]),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out)) return(empty)
  out <- unique(out[, PARTICIPATION_COLUMNS, drop = FALSE])
  out[order(out$person_id, out$year, out$role, out$method), , drop = FALSE]
}

build_participation <- function(out = NULL) {
  if (!exists("PATHS")) source("scripts/config.R")
  rd <- function(p) if (!is.null(p) && file.exists(p))
    read.csv(p, stringsAsFactors = FALSE, check.names = FALSE) else data.frame()
  ppl <- rd(PATHS$people)
  if (!nrow(ppl)) { message("build_participation: no people_manual.csv -- skipped"); return(invisible(NULL)) }
  part <- participation_from_surveys(rd(PATHS$per_survey), ppl)
  out  <- if (is.null(out)) PATHS$participation else out
  write.csv(part, out, row.names = FALSE, na = "")
  invisible(part)
}
