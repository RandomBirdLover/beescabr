# =============================================================
# website/citations.R
# beescabr -- data-source credits for the public site.
#
# The two sources are NOT equivalent, and the pages should not pretend they are:
#
#   * IUCN REQUIRES a citation. Their API terms say "Full acknowledgement and citation
#     needs to be given for using the API", in a fixed form that includes the Red List
#     VERSION (e.g. "Version 2026-1"). The version is captured at refresh time by
#     refresh_iucn_status.R and read from disk here, so building a page never needs a live
#     API call and never has to guess. If the version is unknown we print NOTHING rather
#     than a citation that might be wrong: a wrong citation is worse than none.
#
#   * iNaturalist has no such requirement. It is a community platform, so it is CREDITED
#     as a data source with an access date, which is normal practice.
#
# Both are pure functions of their inputs so the wording is testable.
# =============================================================

# IUCN_CITE_URL / INAT_CITE_URL: the canonical public addresses to point a reader at.
IUCN_CITE_URL <- "https://www.iucnredlist.org"
INAT_CITE_URL <- "https://www.inaturalist.org"

# iucn_citation(): the required form, or "" when the version is unknown.
# The YEAR comes from the version string, not from today: a page rebuilt next year from
# this year's pull must still cite the edition it actually used.
iucn_citation <- function(version, accessed) {
  if (!length(version) || is.na(version) || !nzchar(version)) return("")
  year <- sub("^([0-9]{4}).*$", "\\1", version)
  sprintf("IUCN %s. IUCN Red List of Threatened Species. Version %s. %s. Accessed %s.",
          year, version, IUCN_CITE_URL, accessed)
}

# inat_citation(): a credit, not a formal citation (iNaturalist requires none).
inat_citation <- function(accessed) {
  sprintf("iNaturalist. Observations contributed by the iNaturalist community. %s. Accessed %s.",
          INAT_CITE_URL, accessed)
}

# The version is written beside the IUCN cache when the status is refreshed, so the site
# can cite the exact edition it used without another API call.
citation_save_version <- function(path, version) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(version), path)
  invisible(version)
}

#' Read a data source's version string, if it recorded one
#'
#' @param path The version file.
#' @return The version, or `""` when the file is absent or unreadable. A missing
#'   version is normal for a source that does not publish one.
citation_read_version <- function(path) {
  if (!length(path) || !file.exists(path)) return("")
  v <- tryCatch(trimws(paste(readLines(path, warn = FALSE), collapse = "")),
                error = function(e) "")
  if (is.na(v)) "" else v
}
