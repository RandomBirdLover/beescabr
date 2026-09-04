# =============================================================
# reference/taxonomy/subspecies_from_cache.R
# beescabr -- promote the subspecies iNaturalist has ALREADY given us into the lookup.
#
# WHY: every taxon fetched from iNat is cached whole, and a taxon record carries its
# children. Colletes hyalinus (217441) has held its two subspecies -- gaudialis 345235
# and hyalinus 1052957 -- in the local cache since the first ingest. The lookup never
# read them, so 60 specimens keyed to gaudialis had no node to match: no id, and (until
# the roll-back fix) no lineage either.
#
# This reads what is already on disk. It makes NO API calls, so it cannot slow a build
# or fail offline -- a species with no cache entry is simply skipped.
#
# Only rank == "subspecies" children are taken. iNat also publishes varieties, forms and
# hybrids below species; those are not what a specimen label means by a trinomial, and
# admitting them would put nodes in the lookup that no determination can ever match.
#
# Pure: the fetcher is injected. Tested in tests/testthat/test-subspecies-from-cache.R.
# =============================================================

# fetch_fn(taxon_id) -> the cached taxon record as a list (or NULL when not cached).
# Returns `lookup` with any missing subspecies rows appended, each inheriting its
# parent species' lineage. Idempotent: re-running adds nothing.
subspecies_from_cache <- function(lookup, fetch_fn) {
  if (is.null(lookup) || !nrow(lookup) || !"rank" %in% names(lookup)) return(lookup)
  have <- suppressWarnings(as.integer(lookup$taxon_id))
  parents <- which(tolower(lookup$rank) == "species" & !is.na(have))
  if (!length(parents)) return(lookup)

  add <- list()
  for (i in parents) {
    rec <- tryCatch(fetch_fn(have[i]), error = function(e) NULL)
    kids <- if (is.null(rec)) NULL else rec$children
    if (!length(kids)) next
    for (k in kids) {
      if (!identical(tolower(k$rank %||% ""), "subspecies")) next
      kid <- suppressWarnings(as.integer(k$id))
      if (is.na(kid) || kid %in% have) next            # already in the lookup
      # the epithet is the LAST word of the trinomial ("Colletes hyalinus gaudialis")
      parts <- strsplit(trimws(as.character(k$name %||% "")), "\\s+")[[1]]
      epithet <- if (length(parts)) parts[length(parts)] else NA_character_
      if (is.na(epithet) || !nzchar(epithet)) next
      row <- lookup[i, , drop = FALSE]                 # inherit the parent's lineage
      row$taxon_id        <- kid
      row$rank            <- "subspecies"
      row$subspecies      <- epithet
      if ("scientific_name" %in% names(row)) row$scientific_name <- as.character(k$name)
      if ("common_name"     %in% names(row)) row$common_name     <- NA_character_
      add[[length(add) + 1L]] <- row
      have <- c(have, kid)                             # so a duplicate child is skipped
    }
  }
  if (!length(add)) return(lookup)
  rbind(lookup, do.call(rbind, add))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
