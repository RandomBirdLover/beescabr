# =============================================================
# project_info/roster_view.R
# beescabr -- the per-year view the tag rebuild needs, derived from inputs.
#
# The tag rebuild has to know who was a BEEPLE in a given year, because interns
# are preserved from the intern log and regenerating them from tags would count
# their days twice. That fact used to live in surveyor_roster.csv's per-year
# `role` column. people_manual.csv has no years -- when someone was in the field is
# DERIVED now -- so this rebuilds the view from inputs only.
#
# The rule: you are an intern in a year if the intern log names you that year.
# Otherwise you are a beeple. Nothing here reads participation_generated.csv or the
# generated master, so there is no circularity: intern log -> role -> tag
# rebuild -> master -> participation, one direction only.
#
# Beeple never net (0 of 46 person-years in six seasons), so their method is
# fixed; an intern's comes from the log row, which records what they actually did.
# =============================================================
if (!exists("person_id_keys")) source("scripts/utils/people_ids.R")

roster_view <- function(people, intern_log, years) {
  sq <- function(x) trimws(ifelse(is.na(x), "", as.character(x)))
  if (!is.null(people) && nrow(people) && !"surveyor_type" %in% names(people))
    people$surveyor_type <- ""    # older people.csv: fall back to deriving from the log
  if (!nrow(people) || !length(years))
    return(data.frame(year = integer(0), first_name = character(0), last_name = character(0),
                      inaturalist_username = character(0), collector_code = character(0),
                      role = character(0), method = character(0), technique = character(0),
                      stringsAsFactors = FALSE))
  sv <- people[as.logical(people$surveyor) %in% TRUE, , drop = FALSE]
  years <- sort(unique(as.integer(years)))
  g <- expand.grid(i = seq_len(nrow(sv)), year = years, KEEP.OUT.ATTRS = FALSE)

  # (person_id, year) pairs the intern log names, with what they did that day
  il <- if (!is.null(intern_log) && nrow(intern_log) && "person_ids" %in% names(intern_log)) {
    do.call(rbind, lapply(seq_len(nrow(intern_log)), function(k) {
      ids <- trimws(unlist(strsplit(sq(intern_log$person_ids[k]), "[,;&]")))
      ids <- ids[nzchar(ids)]
      if (!length(ids)) return(NULL)
      data.frame(person_id = ids, year = as.integer(intern_log$year[k]),
                 method = sq(intern_log$method[k]), technique = sq(intern_log$technique[k]),
                 stringsAsFactors = FALSE)
    }))
  } else NULL
  key <- if (is.null(il)) character(0) else paste(il$person_id, il$year)

  out <- data.frame(
    year                 = g$year,
    first_name           = sq(sv$first_name[g$i]),
    last_name            = sq(sv$last_name[g$i]),
    inaturalist_username = sq(sv$inaturalist_username[g$i]),
    collector_code       = sq(sv$collector_code[g$i]),
    stringsAsFactors = FALSE)
  hit <- match(paste(sq(sv$person_id[g$i]), g$year), key)
  # people_manual.csv DECLARES the kind of surveyor. Deriving it from the log alone
  # made every unlogged year "beeple", so a 2024 intern read as a 2025 beeple and
  # could be offered as the answer to a beeple calendar window. The log still wins
  # for a year it names: if they netted that season, they were an intern that season.
  declared <- if ("surveyor_type" %in% names(sv)) tolower(sq(sv$surveyor_type[g$i])) else ""
  out$role      <- ifelse(!is.na(hit), "intern",
                   ifelse(declared %in% c("beeple", "intern"), declared, "beeple"))
  out$method    <- ifelse(is.na(hit), "non-lethal", il$method[hit])
  out$technique <- ifelse(is.na(hit), "photo",      il$technique[hit])
  out$method    <- ifelse(nzchar(out$method),    out$method,    "non-lethal")
  out$technique <- ifelse(nzchar(out$technique), out$technique, "photo")
  out[order(out$year, out$last_name, out$first_name), , drop = FALSE]
}
