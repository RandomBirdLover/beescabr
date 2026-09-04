# =============================================================
# inat_observations/engine/api/inat_flatten.R
# beescabr pipeline -- PURE JSON -> tibble transforms
# Created: 2026-07-13 (API + DuckDB rewrite)
#
# Every function here is PURE: it takes parsed iNat JSON (a nested list,
# e.g. from jsonlite::fromJSON(..., simplifyVector = FALSE)) and returns a
# tibble. No network, no DB, no global state. This is the highest-risk
# code in the rewrite (the ofvs datatype branching in particular), so it
# is deliberately isolated and unit-tested against real sample JSON in
# tests/testthat/test-flatten.R.
#
# Depends on: tibble, dplyr, jsonlite (for callers). No side effects.
# =============================================================

library(tibble)
library(dplyr)

# NULL-coalescing helper (avoids taking a hard rlang dependency here).
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Coerce a possibly-NULL scalar to a length-1 value, NA of the right type
# when absent. Guards against list-valued or empty JSON fields.
.scalar <- function(x, na = NA_character_) {
  if (is.null(x) || length(x) == 0) return(na)
  x[[1]]
}

# Safe nested extractor: .get(x, "a", "b") returns x[["a"]][["b"]], or NULL if
# any level is missing or is not a list. This is what keeps a single malformed
# record from crashing the whole read -- real iNat data occasionally has an
# ofv whose `taxon` is a bare id string rather than the expected object, or an
# ofvs entry that isn't an object at all. Without this, `ofv$taxon$name` throws
# "$ operator is invalid for atomic vectors" and aborts all 76k+ observations.
.get <- function(x, ...) {
  for (k in c(...)) {
    if (!is.list(x)) return(NULL)
    x <- x[[k]]
    if (is.null(x)) return(NULL)
  }
  x
}

# ------------------------------------------------------------
# flatten_ofvs()
# Pivot an observation's `ofvs` array into a single-row named list of
# `field:<lower(name)>` = value columns, matching the retired CSV export's
# column names exactly.
#
# CRITICAL datatype branching:
#   datatype == "taxon"  -> the human-readable value is ofv$taxon$name
#                           (ofv$value is only the numeric taxon id string).
#   anything else        -> ofv$value is the literal value.
# Reading ofv$value blindly for taxon fields is exactly why the old code
# thought API obs-fields were "unreliable" -- it saw "77511" instead of
# "Isocoma menziesii".
#
# Duplicate fields (same name twice on one obs): first non-empty wins.
# Returns a named list (may be empty) suitable for tibble construction.
# ------------------------------------------------------------
flatten_ofvs <- function(ofvs) {
  out <- list()
  if (is.null(ofvs) || length(ofvs) == 0) return(out)

  for (ofv in ofvs) {
    if (!is.list(ofv)) next   # skip a malformed (non-object) ofvs entry
    nm <- .scalar(ofv$name, NA_character_)
    if (is.na(nm) || nm == "") next

    dt <- .scalar(ofv$datatype, NA_character_)
    val <- if (!is.na(dt) && dt == "taxon") {
      # .get returns NULL (=> NA) if `taxon` is absent or a bare id string
      # rather than the expected object -- no crash on malformed records.
      .scalar(.get(ofv, "taxon", "name"), NA_character_)
    } else {
      .scalar(ofv$value, NA_character_)
    }

    col <- paste0("field:", tolower(nm))
    # First non-empty value wins for duplicate fields.
    if (is.null(out[[col]]) || is.na(out[[col]]) || out[[col]] == "") {
      out[[col]] <- val
    }
  }
  out
}

# ------------------------------------------------------------
# flatten_observation()
# Turn one /observations result object into a single-row tibble of the
# CORE export columns (identity, coordinates, quality, tags, description,
# the observation's own taxon id/name) plus all `field:*` obs-field
# columns. The full ranked taxonomy (taxon_kingdom_name ... taxon_subspecies_name)
# is NOT added here -- it is resolved separately from the taxon cache and
# joined by taxon_id (see api/inat_cache.R + pipelines/read_inat.R), because
# the /observations payload only carries ancestor_ids, not their names.
# ------------------------------------------------------------
flatten_observation <- function(o) {
  if (!is.list(o)) return(NULL)   # malformed record -> dropped by bind_rows

  coord <- .get(o, "geojson", "coordinates")
  lon <- if (is.null(coord) || length(coord) < 2) NA_real_ else as.numeric(coord[[1]])
  lat <- if (is.null(coord) || length(coord) < 2) NA_real_ else as.numeric(coord[[2]])
  # TRUE coordinates when we're trusted (own record / individual trust / trusted project):
  # authenticated pulls add private_geojson + private_location for those; absent otherwise.
  coords_trusted <- FALSE
  pcoord <- .get(o, "private_geojson", "coordinates")
  ploc   <- .scalar(o$private_location, NA_character_)
  if (!is.null(pcoord) && length(pcoord) >= 2) {
    lon <- as.numeric(pcoord[[1]]); lat <- as.numeric(pcoord[[2]]); coords_trusted <- TRUE
  } else if (!is.na(ploc) && grepl(",", ploc)) {
    ll <- suppressWarnings(as.numeric(strsplit(ploc, ",")[[1]]))          # private_location is "lat,lng"
    if (length(ll) == 2 && !anyNA(ll)) { lat <- ll[1]; lon <- ll[2]; coords_trusted <- TRUE }
  }

  tags <- o$tags %||% list()
  tag_list <- if (length(tags) == 0) NA_character_ else paste(
    vapply(tags, function(t) if (is.list(t)) (.scalar(t$name, "")) else as.character(t), character(1)),
    collapse = ", "
  )

  core <- tibble(
    id                   = as.integer(.scalar(o$id, NA_integer_)),
    uuid                 = .scalar(o$uuid, NA_character_),
    observed_on          = .scalar(o$observed_on, NA_character_),
    time_observed_at     = .scalar(o$time_observed_at, NA_character_),
    user_id              = as.integer(.scalar(.get(o, "user", "id"), NA_integer_)),
    user_login           = .scalar(.get(o, "user", "login"), NA_character_),
    user_name            = .scalar(.get(o, "user", "name"), NA_character_),
    quality_grade        = .scalar(o$quality_grade, NA_character_),
    license              = .scalar(o$license_code, NA_character_),
    url                  = paste0("https://www.inaturalist.org/observations/", .scalar(o$id, "")),
    tag_list             = tag_list,
    description          = .scalar(o$description, NA_character_),
    captive_cultivated   = isTRUE(o$captive),
    place_guess          = .scalar(o$place_guess, NA_character_),
    latitude             = lat,
    longitude            = lon,
    positional_accuracy  = as.numeric(.scalar(o$positional_accuracy, NA_real_)),
    coordinates_obscured = isTRUE(o$obscured) && !coords_trusted,   # FALSE once we hold the real spot
    coords_trusted       = coords_trusted,
    geoprivacy           = .scalar(o$geoprivacy, NA_character_),
    taxon_geoprivacy     = .scalar(o$taxon_geoprivacy, NA_character_),
    scientific_name      = .scalar(.get(o, "taxon", "name"), NA_character_),
    common_name          = .scalar(.get(o, "taxon", "preferred_common_name"), NA_character_),
    iconic_taxon_name    = .scalar(.get(o, "taxon", "iconic_taxon_name"), NA_character_),
    taxon_id             = as.integer(.scalar(.get(o, "taxon", "id"), NA_integer_))
  )

  ofv_cols <- flatten_ofvs(o$ofvs)
  if (length(ofv_cols) > 0) {
    for (nm in names(ofv_cols)) core[[nm]] <- ofv_cols[[nm]]
  }
  core
}

# ------------------------------------------------------------
# parse_taxon_ranks()
# Given one /taxa/{id} result object (which includes a full `ancestors`
# array of {id, name, rank}), build a single-row tibble of the ranked-name
# columns the CSV export used (taxon_kingdom_name ... taxon_subspecies_name)
# plus subgenus, complex, and complex_taxon_id.
#
# The species/subspecies name columns carry the FULL binomial/trinomial
# exactly as the export did (e.g. "Agapostemon texanus") -- epithet
# extraction happens downstream. This is pure: same in, same out.
# ------------------------------------------------------------
.RANK_TO_COLUMN <- c(
  kingdom     = "taxon_kingdom_name",
  phylum      = "taxon_phylum_name",
  subphylum   = "taxon_subphylum_name",
  class       = "taxon_class_name",
  subclass    = "taxon_subclass_name",
  order       = "taxon_order_name",
  suborder    = "taxon_suborder_name",
  infraorder  = "taxon_infraorder_name",
  superfamily = "taxon_superfamily_name",
  family      = "taxon_family_name",
  epifamily   = "taxon_epifamily_name",
  subfamily   = "taxon_subfamily_name",
  tribe       = "taxon_tribe_name",
  subtribe    = "taxon_subtribe_name",
  genus       = "taxon_genus_name",
  species     = "taxon_species_name",
  subspecies  = "taxon_subspecies_name"
)

parse_taxon_ranks <- function(taxon) {
  # Derived from .RANK_TO_COLUMN so the two never drift out of sync (the 5 sub-
  # ranks -- subphylum, subclass, suborder, infraorder, epifamily -- were added
  # 2026-07 and flow through everywhere these standard columns do).
  rank_cols <- unname(.RANK_TO_COLUMN)
  row <- as.list(rep(NA_character_, length(rank_cols)))
  names(row) <- rank_cols

  subgenus         <- NA_character_
  complex          <- NA_character_
  complex_taxon_id <- NA_integer_

  # Consider the taxon itself first, then its ancestors. The taxon's own
  # rank matters for subgenus/complex-level identifications (mirrors the
  # 2026-06-25 fix in the old native_bee_checklist.R STEP 4).
  entries <- c(list(taxon), taxon$ancestors %||% list())

  for (a in entries) {
    if (!is.list(a)) next
    rk <- .scalar(a$rank, NA_character_)
    nm <- .scalar(a$name, NA_character_)
    if (is.na(rk)) next

    if (rk %in% names(.RANK_TO_COLUMN)) {
      col <- .RANK_TO_COLUMN[[rk]]
      if (is.na(row[[col]])) row[[col]] <- nm
    }
    if (rk == "subgenus" && is.na(subgenus)) subgenus <- nm
    if (rk == "complex" && is.na(complex)) {
      complex <- nm
      complex_taxon_id <- as.integer(.scalar(a$id, NA_integer_))
    }
  }

  out <- tibble(taxon_id = as.integer(.scalar(taxon$id, NA_integer_)))
  for (col in rank_cols) out[[col]] <- row[[col]]
  out$subgenus         <- subgenus
  out$complex          <- complex
  out$complex_taxon_id <- complex_taxon_id
  # The observation taxon's OWN rank (species / genus / subgenus / tribe /
  # ...). Lets consumers label an identification's resolution without a
  # separate lookup join.
  out$rank             <- .scalar(taxon$rank, NA_character_)
  out
}

# ------------------------------------------------------------
# parse_taxon_ancestry()
# Given one /taxa/{id} result object (with its `ancestors` array), return a long
# tibble -- one row per ancestor taxon AND the taxon itself -- of (taxon_id, rank,
# name). This is the raw material for the taxonomy lookup's normalized tree: every
# parent taxon (genus, subgenus, complex, family, ...) can be given its OWN row
# with its OWN iNat id from here, instead of a species row borrowing a parent's id.
# PURE: same taxon in, same rows out.
# ------------------------------------------------------------
parse_taxon_ancestry <- function(taxon) {
  entries <- c(list(taxon), taxon$ancestors %||% list())
  ids <- integer(0); ranks <- character(0); nms <- character(0)
  for (a in entries) {
    if (!is.list(a)) next
    id <- suppressWarnings(as.integer(.scalar(a$id, NA_integer_)))
    rk <- .scalar(a$rank, NA_character_)
    nm <- .scalar(a$name, NA_character_)
    if (is.na(id) || is.na(rk)) next
    ids <- c(ids, id); ranks <- c(ranks, rk); nms <- c(nms, nm)
  }
  tibble(taxon_id = ids, rank = ranks, name = nms)
}
