# =============================================================
# utils/people.R
# beescabr -- turning a person's name into an identity.
#
# Surveyor names reach us in three shapes and all three mean the same person:
#
#   "Sam O'Dell"   intern-log and inat-tag rows in master_per_survey_info_generated.csv
#   "S O'Dell"     specimen-record rows -- the museum label written on the
#                  specimen. That string is the record, so it is never rewritten.
#   "Sam"          older data, before the log was expanded to full names.
#
# The rule that keeps this honest: a first name is not an identity. Two people
# on the roster are named Julia (Keum, a 2024 intern; Showalter, a 2025-26
# beeple), so a bare "Julia" gets NO key -- an uncredited record is a fixable
# gap, a record credited to the wrong person is not.
# =============================================================

# "First Last", with stray whitespace squished (the roster has a typo'd trailing
# space in at least one first_name, which would otherwise print as a double space).
# NA when there is no name at all.
person_name <- function(first, last) {
  nm <- trimws(gsub("[[:space:]]+", " ",
                    paste(ifelse(is.na(first), "", first), ifelse(is.na(last), "", last))))
  ifelse(nzchar(nm), nm, NA_character_)
}

# Lookup vector: lowercased name-as-written -> canonical person. Covers the full
# name, the museum-label form, and the first name only where it is unambiguous.
#
# `code` is the label form DECLARED in people_manual.csv (collector_code for a netter,
# determiner_code for a specimen determiner). It is checked first because inference cannot
# reproduce every label: "X. Gaeta" and a middle initial are both out of its reach. An
# unfilled cell adds no key -- it must never match a blank.
person_name_keys <- function(name, first, last, code = NULL) {
  first <- trimws(ifelse(is.na(first), "", first))
  last  <- trimws(ifelse(is.na(last),  "", last))
  full  <- tolower(name)
  init  <- tolower(trimws(paste(substr(first, 1, 1), last)))
  fst   <- tolower(first)
  fst[fst %in% fst[duplicated(fst)]] <- ""        # a first name two people share is not a key
  cd    <- if (is.null(code)) character(length(name)) else tolower(trimws(ifelse(is.na(code), "", code)))
  k <- c(setNames(name, cd), setNames(name, full), setNames(name, init), setNames(name, fst))
  k <- k[nzchar(names(k)) & !is.na(names(k))]
  k[!duplicated(names(k))]                        # declared code wins, then the full name
}

# =============================================================
# PART 2 -- person_id: assign it once, resolve it forever.
#
# Part 1 above turns the three name shapes into one identity. This turns that
# identity into a stable id. They were separate files; neither is usable without
# the other, so a caller had to know which half held the function it wanted.
# =============================================================
if (!exists("person_name_keys")) source("scripts/utils/people.R")

PERSON_ID_PREFIX <- "p"
PERSON_ID_WIDTH  <- 3L

# Mint ids for new people, never reusing one that has been issued.
person_id_mint <- function(n, existing = character(0)) {
  used <- suppressWarnings(as.integer(sub(paste0("^", PERSON_ID_PREFIX), "", trimws(existing))))
  nxt  <- if (length(used[!is.na(used)])) max(used, na.rm = TRUE) + 1L else 1L
  sprintf(paste0(PERSON_ID_PREFIX, "%0", PERSON_ID_WIDTH, "d"), seq(nxt, length.out = n))
}

# Every written form of every person -> person_id. One lookup for the whole project:
# full name, museum-label form, unique first name, iNat handle, declared codes.
person_id_keys <- function(people) {
  nm <- person_name(people$first_name, people$last_name)
  ok <- !is.na(nm)
  id <- trimws(as.character(people$person_id))
  k  <- c(person_name_keys(id[ok], people$first_name[ok], people$last_name[ok], people$collector_code[ok]),
          person_name_keys(id[ok], people$first_name[ok], people$last_name[ok], people$determiner_code[ok]))
  add <- function(k, vals, ids) {
    v <- tolower(trimws(ifelse(is.na(vals), "", vals)))
    keep <- nzchar(v)
    c(k, setNames(ids[keep], v[keep]))
  }
  k <- add(k, people$inaturalist_username, id)
  k <- add(k, tolower(nm), id)
  k <- k[nzchar(names(k)) & !is.na(names(k))]
  k[!duplicated(names(k))]
}

# Resolve any written form to a person_id. NA when it matches nobody -- never a guess.
person_id_of <- function(x, people, keys = person_id_keys(people)) {
  unname(keys[tolower(trimws(ifelse(is.na(x), "", x)))])
}

# A "surveyors"-style cell ("Amy Geffre, Xiomara Gaeta") -> the person_ids in it.
person_ids_in <- function(cell, people, keys = person_id_keys(people)) {
  tk <- trimws(unlist(strsplit(ifelse(is.na(cell), "", cell), "[,;&]")))
  tk <- tk[nzchar(tk)]
  unique(na.omit(person_id_of(tk, people, keys)))
}

# ---- ids -> what a human reads ----------------------------------------------
# The hand-maintained files store person_ids so a name is typed once, in
# people_manual.csv, ever. The GENERATED master still shows names and handles, because
# someone reviewing it has to be able to read it. An id that matches nobody is
# kept verbatim rather than dropped -- a typo must be visible, not silent.
.pid_lookup <- function(ids, people, col, blank_ok = TRUE) {
  one <- function(cell) {
    if (is.na(cell) || !nzchar(trimws(cell))) return(cell)
    tk  <- trimws(unlist(strsplit(cell, "[,;&]")))
    tk  <- tk[nzchar(tk)]
    hit <- people[[col]][match(tk, trimws(as.character(people$person_id)))]
    out <- ifelse(is.na(hit) | !nzchar(trimws(ifelse(is.na(hit), "", hit))), NA_character_, trimws(hit))
    if (blank_ok) out <- ifelse(is.na(out), tk, out)      # unknown id stays visible
    out <- out[!is.na(out)]
    if (!length(out)) return("")
    paste(out, collapse = ", ")
  }
  vapply(seq_along(ids), function(i) one(ids[[i]]), character(1))
}

# "p002, p001" -> "Cindy Pencek, Sam O'Dell"
people_display <- function(ids, people) {
  nm <- person_name(people$first_name, people$last_name)
  .pid_lookup(ids, transform(people, .nm = ifelse(is.na(nm), "", nm)), ".nm", blank_ok = TRUE)
}

# "p001, p002" -> "wranglebees, carrotpeople"; people without a handle are skipped,
# and a cell where nobody has one becomes "n/a" (what the master has always written).
people_handles <- function(ids, people) {
  out <- .pid_lookup(ids, people, "inaturalist_username", blank_ok = FALSE)
  ifelse(!is.na(ids) & nzchar(trimws(ifelse(is.na(ids), "", ids))) & !nzchar(out), "n/a", out)
}
