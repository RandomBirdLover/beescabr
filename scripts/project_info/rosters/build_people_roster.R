# =============================================================
# project_info/rosters/build_people_roster.R
# beescabr -- merges the three hand-maintained rosters into ONE people table:
#   surveyor_roster.csv + identifier_roster.csv + research_team_roster.csv
#     -> data/project_info/rosters/people_manual.csv   (one row per human)
#
# WHY: the three files all held IDENTITY, so every person in more than one was
# copy-pasted between them and had already drifted (two emails for one person, a
# phone in one year's row and not the next). One row per human means a fact is
# stated once and cannot disagree with itself.
#
# This is a ONE-TIME migration. After it runs, people_manual.csv is the hand-maintained
# input and the three rosters are retired. Re-running it is safe (it rebuilds
# from whatever rosters are present) but it is not part of the pipeline.
#
#   source("scripts/project_info/rosters/build_people_roster.R")
# =============================================================
if (!exists("person_id_mint")) source("scripts/utils/people_ids.R")

PEOPLE_COLUMNS <- c("person_id", "first_name", "last_name", "inaturalist_username",
                    "email", "phone_number", "affiliation", "collector_code",
                    "determiner_code", "taxa_identified", "id_count", "surveyor", "surveyor_type", "identifier",
                    "researcher", "team_title", "team_order", "photo", "photo_focus", "photo_zoom",
                    "photo_credit", "notes")

.pr_sq <- function(x) trimws(ifelse(is.na(x), "", as.character(x)))
.pr_nm <- function(d) trimws(paste(.pr_sq(d$first_name), .pr_sq(d$last_name)))

# Join key: the iNat handle when there is one (unambiguous), else the full name.
# Never the name when a handle exists -- two people could share a name, and a
# handle is the only thing iNaturalist guarantees unique.
.pr_key <- function(d) {
  u <- tolower(.pr_sq(d$inaturalist_username))
  ifelse(nzchar(u), u, tolower(.pr_nm(d)))
}

# people_from_rosters(): PURE. Returns one row per human, with a `conflicts`
# attribute listing every field where two rosters disagree. A conflict is
# REPORTED and the first value kept -- a human resolves it, we never guess which
# roster is right.
people_from_rosters <- function(surveyor, identifier, team) {
  rs <- list(surveyor = surveyor, identifier = identifier, team = team)
  for (n in names(rs)) if (is.null(rs[[n]])) rs[[n]] <- data.frame()
  ks <- unique(unlist(lapply(rs, function(d) if (nrow(d)) .pr_key(d) else character(0))))

  # every distinct non-blank value a roster gives for one person + field
  vals <- function(k, col, which = names(rs)) {
    v <- unlist(lapply(rs[which], function(d) {
      if (!nrow(d) || !col %in% names(d)) return(character(0))
      .pr_sq(d[[col]][.pr_key(d) == k])
    }))
    unique(v[nzchar(v)])
  }
  pick <- function(k, col, which = names(rs)) { v <- vals(k, col, which); if (length(v)) v[1] else "" }

  out <- data.frame(person_id = person_id_mint(length(ks)), stringsAsFactors = FALSE)
  # id_count is a hand-typed iNaturalist statistic. It orders the acknowledgements
  # list and nothing else; it goes stale silently and should eventually be derived.
  for (col in c("first_name", "last_name", "inaturalist_username", "email",
                "phone_number", "affiliation", "taxa_identified", "id_count", "notes"))
    out[[col]] <- vapply(ks, pick, "", col = col)
  # label codes are roster-specific: a collector code is a surveyor fact, a
  # determiner code an identifier fact. Same string can be both (Sam is both).
  out$collector_code  <- vapply(ks, pick, "", col = "collector_code",  which = "surveyor")
  out$determiner_code <- vapply(ks, pick, "", col = "determiner_code", which = "identifier")
  out$surveyor   <- ks %in% (if (nrow(rs$surveyor))   .pr_key(rs$surveyor)   else character(0))
  out$identifier <- ks %in% (if (nrow(rs$identifier)) .pr_key(rs$identifier) else character(0))
  out$researcher <- ks %in% (if (nrow(rs$team))       .pr_key(rs$team)       else character(0))
  out$team_title <- vapply(ks, pick, "", col = "role", which = "team")   # job title, not a project role
  # The research-team page order was carried by ROW ORDER in the old roster (PI first).
  # Row order is not data -- a sort or a re-merge silently loses it -- so it becomes a column.
  tk <- if (nrow(rs$team)) .pr_key(rs$team) else character(0)
  out$team_order <- ifelse(ks %in% tk, as.character(match(ks, tk)), "")
  for (col in c("photo", "photo_focus", "photo_zoom", "photo_credit"))
    out[[col]] <- vapply(ks, pick, "", col = col, which = "team")

  cf <- do.call(rbind, lapply(ks, function(k) {
    rows <- lapply(c("first_name", "last_name", "inaturalist_username", "email",
                     "phone_number", "affiliation"), function(col) {
      v <- vals(k, col)
      if (length(v) < 2) return(NULL)
      data.frame(person = pick(k, "first_name"), field = col,
                 values = paste(v, collapse = " | "), stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  }))
  if (is.null(cf)) cf <- data.frame(person = character(0), field = character(0),
                                    values = character(0), stringsAsFactors = FALSE)
  out <- out[, intersect(PEOPLE_COLUMNS, names(out)), drop = FALSE]
  attr(out, "conflicts") <- cf
  out
}

RESEARCH_TEAM_ROSTER <- "data/project_info/rosters/research_team_roster.csv"   # RETIRED, deleted 2026-09-02

build_people_roster <- function(out = NULL, surveyor = NULL, identifier = NULL, team = NULL) {
  if (!exists("PATHS")) source("scripts/config.R")
  rd <- function(p) if (!is.null(p) && file.exists(p))
    read.csv(p, stringsAsFactors = FALSE, check.names = FALSE) else data.frame()
  surveyor   <- if (is.null(surveyor))   PATHS$surveyor_roster   else surveyor
  identifier <- if (is.null(identifier)) PATHS$identifier_roster else identifier
  team       <- if (is.null(team))       RESEARCH_TEAM_ROSTER    else team
  ppl <- people_from_rosters(rd(surveyor), rd(identifier), rd(team))
  cf  <- attr(ppl, "conflicts")
  out <- if (is.null(out)) PATHS$people else out
  # The source rosters are RETIRED and deleted. Re-running this would otherwise
  # overwrite people_manual.csv -- the hand-maintained record of 48 humans -- with nothing.
  if (!nrow(ppl))
    stop("build_people_roster: found no people in the source rosters. They were retired ",
         "on 2026-09-02 and people_manual.csv is now the hand-maintained input -- refusing to ",
         "overwrite it. Restore the rosters from git history if you really mean to re-migrate.",
         call. = FALSE)
  write.csv(ppl, out, row.names = FALSE, na = "")
  message(sprintf("Wrote %s  (%d people: %d surveyor / %d identifier / %d researcher)",
                  out, nrow(ppl), sum(ppl$surveyor), sum(ppl$identifier), sum(ppl$researcher)))
  if (nrow(cf)) {
    message("\n  ", nrow(cf), " CONFLICT(S) -- two rosters disagree. Resolve by hand, then re-run:")
    for (i in seq_len(nrow(cf)))
      message(sprintf("    %-18s %-16s %s", cf$person[i], cf$field[i], cf$values[i]))
  }
  invisible(ppl)
}

if (sys.nframe() == 0) build_people_roster()
