# =============================================================
# utils/refresh_due.R
# beescabr -- "is this cached reference data stale?"
#
# IUCN Red List status and plant common names are cached so a normal run stays OFFLINE.
# They only refresh when someone remembers BEESCABR_REFRESH=1, which is easy to forget
# for an entire season. This module lets the pipeline check how old the cached values
# ACTUALLY are and say so, instead of relying on memory.
#
# Age comes from each cache's own retrieved_on column, never the file's mtime: these
# files get rewritten for unrelated reasons (a new taxon appended, a column added), and
# a rewrite must not make year-old values look freshly checked.
#
# Judged by the OLDEST entry, because one taxon rechecked yesterday says nothing about
# the other 200. Pure + injectable so it is unit-tested offline.
# =============================================================

REFRESH_MAX_AGE_DAYS <- 365L   # IUCN reassesses a handful of taxa per year; annual is plenty

refresh_due <- function(path, max_age_days = REFRESH_MAX_AGE_DAYS,
                        date_col = "retrieved_on", today = Sys.Date()) {
  none <- function(reason) list(due = TRUE, age_days = NA_integer_, oldest = NA,
                                reason = reason, path = path)
  if (!file.exists(path)) return(none("never fetched (no cache file yet)"))
  d <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(d) || !date_col %in% names(d)) return(none("cache has no retrieval dates"))
  dates <- suppressWarnings(as.Date(d[[date_col]]))
  dates <- dates[!is.na(dates)]
  if (!length(dates)) return(none("cache has no usable retrieval dates"))
  oldest <- min(dates)
  age    <- as.integer(today - oldest)
  list(due      = age > max_age_days,
       age_days = age,
       oldest   = oldest,
       reason   = sprintf("oldest entry checked %s (%d days ago)", oldest, age),
       path     = path)
}

# The reference caches a normal run depends on, in the order they matter.
REFRESH_CACHES <- list(
  list(key = "IUCN Red List status", path = "data/checklists/iucn/iucn_status_generated.csv",
       tool = "scripts/reference/refresh/refresh_iucn_status.R", needs = "internet + a free IUCN token"),
  list(key = "plant common names",   path = "data/checklists/plants/plant_genus_common_generated.csv",
       tool = "scripts/reference/refresh/refresh_plant_common_names.R", needs = "internet")
)

# Report any cache past its age limit. Returns a list (empty = all current) so the
# caller decides whether to warn, prompt, or ignore.
refresh_overdue <- function(caches = REFRESH_CACHES, max_age_days = REFRESH_MAX_AGE_DAYS,
                            today = Sys.Date()) {
  out <- lapply(caches, function(c) c(c, refresh_due(c$path, max_age_days, today = today)))
  Filter(function(x) isTRUE(x$due), out)
}

# Ask before spending a few minutes online re-checking a year-old cache. Defaults to
# NO: declining keeps the existing cache and the run carries on normally, still picking
# up NEW taxa incrementally the way every run does. Nothing is lost by saying no except
# re-checking taxa that were already looked up.
#
# Note on scope: this is NOT the name-judgement step. Both refresh tools query the APIs
# by scientific name and rewrite their cache with no prompts. Judging renames/synonyms
# is the FULL REBUILD (run-menu option 4), which carries its own expertise warning.
refresh_confirm <- function(overdue = refresh_overdue(),
                            read_fn = function(prompt) readline(prompt),
                            is_interactive = interactive(),
                            say = message) {
  if (!length(overdue)) return(FALSE)
  if (!is_interactive)  return(FALSE)   # scheduled runs never phone out unasked
  say("")
  say("  It has been over a year since this reference data was last checked:")
  for (o in overdue) say("    - ", o$key, " (", o$reason, ")")
  say("")
  say("  Refreshing re-queries every entry against the live APIs. It needs INTERNET")
  say("  and a free IUCN token, and takes a few minutes. It does NOT ask you to judge")
  say("  any bee names -- that is the full rebuild, which is a separate choice.")
  say("")
  say("  Say no and the run keeps the existing cache and continues normally; new taxa")
  say("  are still picked up as they always are.")
  ans <- tolower(trimws(read_fn("  Refresh the reference data now? [y/N]: ")))
  isTRUE(ans %in% c("y", "yes"))
}
