# =============================================================
# analysis/explorer_photo_helpers.R
# Representative-photo selection for the Bee Occurrence Explorer.
#
# Pure logic + injectable transport, so it is unit-testable without the network
# (tests fake request_fn; see tests/testthat/test-explorer-photos.R). Only
# OPENLY-LICENSED photos are ever kept, and every credit line carries the
# photographer attribution plus an explicit license label.
#
# The iNat /v1/taxa SEARCH response contains only the taxon's default_photo;
# the per-taxon photo list (taxon_photos) is only on the /v1/taxa/{id} DETAIL
# endpoint. So fetch_taxon_photo tries the default photo first and makes the
# second (detail) request only when that photo is missing or closed-license.
# =============================================================

OPEN_PHOTO_LICENSES <- c("cc0", "pd", "cc-by", "cc-by-nc", "cc-by-sa",
                         "cc-by-nd", "cc-by-nc-sa", "cc-by-nc-nd")

`%||%` <- function(a, b) if (is.null(a) || is.na(a) || a == "") b else a

# "cc-by-nc" -> "CC BY-NC", "cc0" -> "CC0", "pd" -> "Public Domain"; NA if missing.
license_label <- function(code) {
  if (is.null(code) || is.na(code) || code == "") return(NA_character_)
  code <- tolower(code)
  if (code == "cc0") return("CC0")
  if (code == "pd")  return("Public Domain")
  sub("^CC-", "CC ", toupper(code))
}

# Attribution + license label, without duplicating a label iNat already put in
# the attribution text ("... some rights reserved (CC BY-NC)").
photo_credit <- function(attribution, code) {
  attribution <- attribution %||% "iNaturalist"
  lab <- license_label(code)
  if (is.na(lab)) return(attribution)
  if (grepl(tolower(lab), tolower(attribution), fixed = TRUE)) return(attribution)
  paste0(attribution, " · ", lab)
}

.photo_is_open <- function(p, open_lic) {
  !is.null(p) && !is.null(p$license_code) && !is.na(p$license_code) &&
    p$license_code %in% open_lic && !is.null(p$medium_url)
}

# From one parsed taxon record: the first openly-licensed photo (default photo
# first, then taxon_photos in iNat's curated order) as list(u, c, l), or NULL.
# The exact-name check guards against the search returning a different taxon
# (e.g. a synonym now filed under another genus).
pick_open_photo <- function(t, name, open_lic = OPEN_PHOTO_LICENSES) {
  if (is.null(t) || is.null(t$name) || tolower(t$name) != tolower(name)) return(NULL)
  cands <- c(list(t$default_photo), lapply(t$taxon_photos, function(x) x$photo))
  for (p in cands) if (.photo_is_open(p, open_lic))
    return(list(u = p$medium_url,
                c = photo_credit(p$attribution, p$license_code),
                l = sprintf("https://www.inaturalist.org/taxa/%s", t$id)))
  NULL
}

.inat_request <- function(url)
  tryCatch(jsonlite::fromJSON(url, simplifyVector = FALSE), error = function(e) NULL)

# Search for the taxon; if its default photo is closed, fetch the detail record
# and pick from taxon_photos. -> list(u, c, l) or NULL.
fetch_taxon_photo <- function(name, rank, open_lic = OPEN_PHOTO_LICENSES,
                              request_fn = .inat_request,
                              sleep_fn = function() Sys.sleep(0.7)) {
  res <- request_fn(sprintf("https://api.inaturalist.org/v1/taxa?q=%s&rank=%s&per_page=1",
                            utils::URLencode(name, reserved = TRUE), rank))
  sleep_fn()
  if (is.null(res) || length(res$results) == 0) return(NULL)
  t <- res$results[[1]]
  if (is.null(t$name) || tolower(t$name) != tolower(name)) return(NULL)
  hit <- pick_open_photo(t, name, open_lic)
  if (!is.null(hit)) return(hit)
  det <- request_fn(sprintf("https://api.inaturalist.org/v1/taxa/%s", t$id))
  sleep_fn()
  if (is.null(det) || length(det$results) == 0) return(NULL)
  pick_open_photo(det$results[[1]], name, open_lic)
}
