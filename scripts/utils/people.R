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
# `code` is the label form DECLARED by a roster (surveyor_roster.csv's collector_code,
# identifier_roster.csv's determiner_code). It is checked first because inference cannot
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
