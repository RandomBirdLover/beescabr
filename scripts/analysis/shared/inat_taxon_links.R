# =============================================================
# analysis/shared/inat_taxon_links.R
# beescabr -- small iNaturalist logo links for the field guides.
#
# The guides never embed bee photos (licensing lives with iNaturalist); instead
# each bee gets the iNat logo linking to its taxon page, where the photo gallery
# is. Pure string builders, unit-tested in tests/testthat/test-inat-taxon-links.R.
# The logo is the official iNat badge EMBEDDED as a data URI (single source for
# every page and map popup; nothing external to load, so it also works offline).
# =============================================================

INAT_LOGO_URL <- "data:image/png;base64,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"

# Taxon page when the id is known (from sd_bee_taxonomy_lookup_generated.csv); otherwise a
# name search on iNat, so the link never dead-ends.
inat_taxon_url <- function(taxon_id, scientific_name) {
  if (length(taxon_id) && !is.na(taxon_id))
    sprintf("https://www.inaturalist.org/taxa/%d", as.integer(taxon_id))
  else
    paste0("https://www.inaturalist.org/taxa/search?q=",
           utils::URLencode(scientific_name, reserved = TRUE))
}

#' iNaturalist photo link, as an HTML anchor carrying the iNat logo
#'
#' @param taxon_id iNaturalist taxon id. Pass the id the record already carries;
#'   do not look it up again from a name (see CLAUDE.md's join rule).
#' @param scientific_name Used for the tooltip, and for the name-search fallback
#'   URL when `taxon_id` is missing -- 17 checklist bees have no iNat taxon.
#' @return A one-element HTML string, ready to paste into a table cell.
inat_photo_link <- function(taxon_id, scientific_name) {
  sprintf(paste0('<a class="inat" href="%s" target="_blank" rel="noopener" ',
                 'title="See photos of %s on iNaturalist">',
                 '<img src="%s" alt="iNaturalist" style="height:13px;width:auto;vertical-align:-2px;margin-left:6px"></a>'),
          inat_taxon_url(taxon_id, scientific_name), scientific_name, INAT_LOGO_URL)
}

# ---- carry the taxon_id, do not re-derive it ---------------------------------
# Every cleaned record already carries a taxon_id. Scripts used to drop it during
# aggregation and then recover it by matching an assembled display name against
# sd_bee_taxonomy_lookup_generated.csv -- which silently failed whenever the lookup spelled the
# name differently (e.g. "Anthophora urbana clementina", or genera absent from the
# checklist like Biastes and Trachusa), quietly downgrading a direct taxon link to a
# name search. Keep taxon_id in the grab/select step and pass it here instead.
#
# Picks ONE id for a group of records: the species-level id wins, because a species
# row pools its subspecies records and the row represents the species. Failing that,
# the most common id present. All-missing -> NA, which inat_photo_link turns into the
# name-search fallback (correct for a genuine data gap, e.g. a record with no id).
bee_taxon_id <- function(ids, ranks) {
  if (!length(ids)) return(NA_integer_)
  ids   <- suppressWarnings(as.integer(ids))
  ranks <- tolower(trimws(as.character(ranks)))
  pick <- function(v) { v <- v[!is.na(v)]
    if (!length(v)) return(NA_integer_)
    as.integer(names(sort(table(v), decreasing = TRUE))[1]) }
  sp <- pick(ids[ranks == "species"])
  if (!is.na(sp)) return(sp)
  pick(ids)
}
