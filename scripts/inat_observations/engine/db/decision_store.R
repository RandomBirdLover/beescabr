# =============================================================
# inat_observations/engine/db/decision_store.R
# beescabr pipeline -- persisted manual disambiguation decisions (DuckDB)
# Created: 2026-07-13 (API + DuckDB rewrite)
#
# The Holway reference builder is interactive: when an iNat name search is
# ambiguous, a human picks the right taxon (or skips). Those decisions are
# recorded here keyed by the exact search term, so re-running the builder
# is reproducible and never re-prompts for an already-decided term. This is
# what makes "manual intervention" safe to bake into a batch pipeline.
#
# Depends on: DBI, duckdb.
# =============================================================

library(DBI)

# Return the recorded decision for a search term, or NULL if undecided.
# A decision is list(action, chosen_taxon_id). action is one of:
#   "pick"      -> resolved to chosen_taxon_id
#   "keep"      -> valid in ITIS but not on iNat (blank id, itis_valid TRUE)
#   "skip"      -> checked ITIS, not valid / unpublished (blank id, itis_valid FALSE)
#   "tentative" -> two-word name the user said is NOT a subspecies; provisional
#                  (blank id, itis_valid blank)
decision_get <- function(con, search_term) {
  res <- DBI::dbGetQuery(
    con,
    "SELECT action, chosen_taxon_id FROM holway_decisions WHERE search_term = ?",
    params = list(search_term)
  )
  if (nrow(res) == 0) return(NULL)
  list(action = res$action[1],
       chosen_taxon_id = if (is.na(res$chosen_taxon_id[1])) NA_integer_ else as.integer(res$chosen_taxon_id[1]))
}

decision_put <- function(con, search_term, action, chosen_taxon_id = NA_integer_) {
  stopifnot(action %in% c("pick", "skip", "keep", "tentative"))
  DBI::dbExecute(
    con,
    "INSERT OR REPLACE INTO holway_decisions (search_term, chosen_taxon_id, action, decided_at)
     VALUES (?, ?, ?, now())",
    params = list(search_term,
                  if (is.na(chosen_taxon_id)) NA_integer_ else as.integer(chosen_taxon_id),
                  action)
  )
  invisible(search_term)
}

decision_count <- function(con) {
  DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM holway_decisions")$n[1]
}
