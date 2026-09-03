# =============================================================
# utils/people_ids.R -- person_id: assign it once, resolve it forever.
#
# A person_id is OPAQUE and STABLE. It is never derived from a name, so a
# surname change, a new iNat handle, or a differently-spelled museum label
# does not break a single join. Same rule taxon_id follows.
#
# The "p" prefix is not decoration. A bare 26 comes back from read.csv as an
# INTEGER, and in R match(26L, "26") is NA while 26 == "26" is TRUE -- so a
# number-shaped id fails silently and mis-credits a person. "p026" is always a
# string, is obviously a person rather than a year or a count, and sorts right.
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
