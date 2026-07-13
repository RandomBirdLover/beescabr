# =============================================================
# checklists/holway_reference_build.R
# beescabr pipeline -- interactive Holway -> iNat taxon resolver
# Created: 2026-07-13 (ported from the Python process_holway_data)
#
# Resolves each row of Holway's v3 combined sheet to an iNat taxon and walks
# its ancestry to kingdom, producing a taxonomy checklist. This is the ONE
# place manual intervention is expected: when an iNat name search is
# ambiguous, a human picks the right taxon (or skips). Every decision is
# persisted in the DuckDB holway_decisions table (db/decision_store.R) so
# reruns are reproducible and never re-prompt an already-decided term.
#
# The candidate-selection RULE is a pure function (select_taxon_candidate)
# so the auto-pick / auto-skip / needs-prompt branching is unit-tested;
# the readline() prompting is the only impure part.
#
# Run: Rscript scripts/checklists/holway_reference_build.R
#   Set BEESCABR_NONINTERACTIVE=1 to auto-skip anything that would prompt
#   (for unattended reruns -- ambiguous terms are left for a human pass).
#
# Depends on: config.R, db/*, api/*.
# =============================================================

library(dplyr)
library(jsonlite)

if (sys.nframe() == 0L || !exists("select_taxon_candidate")) {
  # allow sourcing just the helpers for tests without pulling the world
}

# ------------------------------------------------------------
# select_taxon_candidate(): PURE decision rule for a name-search result set.
# Returns list(action, index):
#   action "pick"   -> use results[[index]]
#   action "skip"   -> no usable taxon (auto)
#   action "prompt" -> ambiguous, a human must choose
# Mirrors the Python auto-pick logic: single result -> pick; multiple ->
# for non-Described rows (genus-level searches) auto-skip; for Described
# rows pick the sole species-rank hit, or prompt if several species tie.
# ------------------------------------------------------------
select_taxon_candidate <- function(results, described) {
  n <- length(results)
  if (n == 0) return(list(action = "skip", index = NA_integer_))
  if (n == 1) return(list(action = "pick", index = 1L))

  species_idx <- which(vapply(results, function(t) identical(t$rank, "species"), logical(1)))
  if (!described) return(list(action = "skip", index = NA_integer_))
  if (length(species_idx) == 1) return(list(action = "pick", index = species_idx[[1]]))
  if (length(species_idx) == 0) return(list(action = "prompt", index = NA_integer_))
  list(action = "prompt", index = NA_integer_)
}

# ------------------------------------------------------------
# holway_search_term(): the string to search iNat for a Holway row.
# Described rows search "Genus species"; others search the genus alone.
# ------------------------------------------------------------
holway_search_term <- function(source_sheet, genus, species_raw) {
  if (identical(source_sheet, "Described")) paste(genus, species_raw) else genus
}

# ------------------------------------------------------------
# resolve_holway_row(): resolve one row to a taxon id, consulting/recording
# the decision cache and (only when needed and allowed) prompting. Returns
# list(taxon_id, action). Impure: may call the API via `con`/request_fn and
# may readline().
# ------------------------------------------------------------
resolve_holway_row <- function(con, source_sheet, genus, species_raw,
                               request_fn = inat_request,
                               interactive_ok = TRUE,
                               prompt_fn = readline) {
  term <- holway_search_term(source_sheet, genus, species_raw)

  decided <- decision_get(con, term)
  if (!is.null(decided)) {
    return(list(taxon_id = decided$chosen_taxon_id, action = decided$action, term = term))
  }

  results <- get_taxa_by_name(con, term, request_fn = request_fn)
  choice <- select_taxon_candidate(results, described = identical(source_sheet, "Described"))

  if (choice$action == "prompt") {
    if (!interactive_ok) {
      decision_put(con, term, "skip")
      return(list(taxon_id = NA_integer_, action = "skip", term = term))
    }
    message("\nAmbiguous: '", term, "' -- choose a taxon:")
    for (i in seq_along(results)) {
      t <- results[[i]]
      message(sprintf("  [%d] id=%s %s (%s)", i, t$id, t$name, t$rank %||% "?"))
    }
    raw <- prompt_fn("Pick number (or 'skip'): ")
    if (identical(trimws(raw), "skip")) {
      decision_put(con, term, "skip")
      return(list(taxon_id = NA_integer_, action = "skip", term = term))
    }
    idx <- suppressWarnings(as.integer(raw))
    if (is.na(idx) || idx < 1 || idx > length(results)) {
      decision_put(con, term, "skip")
      return(list(taxon_id = NA_integer_, action = "skip", term = term))
    }
    chosen <- results[[idx]]
    decision_put(con, term, "pick", chosen$id)
    return(list(taxon_id = as.integer(chosen$id), action = "pick", term = term))
  }

  if (choice$action == "skip") {
    decision_put(con, term, "skip")
    return(list(taxon_id = NA_integer_, action = "skip", term = term))
  }

  chosen <- results[[choice$index]]
  decision_put(con, term, "pick", chosen$id)
  list(taxon_id = as.integer(chosen$id), action = "pick", term = term)
}

# ------------------------------------------------------------
# build_holway_reference(): full interactive run over the Holway sheet.
# Produces a taxonomy tibble (one row per resolved Holway entry, full
# ancestry) and returns it; the runner writes it to disk.
# ------------------------------------------------------------
build_holway_reference <- function(con, holway_df,
                                   request_fn = inat_request,
                                   interactive_ok = TRUE) {
  rows <- vector("list", nrow(holway_df))
  for (i in seq_len(nrow(holway_df))) {
    r <- holway_df[i, ]
    res <- resolve_holway_row(con, r$source_sheet, r$genus, r$species_raw,
                              request_fn = request_fn, interactive_ok = interactive_ok)
    if (is.na(res$taxon_id)) {
      rows[[i]] <- tibble::tibble(
        source_sheet = r$source_sheet, genus = r$genus,
        species = r$species_raw, taxon_id = NA_integer_, resolved = FALSE
      )
    } else {
      taxon <- get_taxon_by_id(con, res$taxon_id, request_fn = request_fn)
      ranks <- parse_taxon_ranks(taxon)
      rows[[i]] <- ranks |>
        mutate(source_sheet = r$source_sheet, resolved = TRUE)
    }
    if (i %% 25 == 0) message(sprintf("  Holway resolve: %d / %d", i, nrow(holway_df)))
  }
  dplyr::bind_rows(rows)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ------------------------------------------------------------
# Runner (only when executed directly, not when sourced for tests).
# ------------------------------------------------------------
if (identical(environment(), globalenv()) &&
    !is.na(Sys.getenv("BEESCABR_RUN_HOLWAY", unset = NA))) {
  source("scripts/config.R")
  source("scripts/db/store_conn.R"); source("scripts/db/taxon_store.R")
  source("scripts/db/decision_store.R"); source("scripts/api/inat_http.R")
  source("scripts/api/inat_flatten.R"); source("scripts/api/inat_cache.R")
  source("scripts/checklists/holway.R")

  con <- store_connect()
  on.exit(store_disconnect(con), add = TRUE)
  holway_df <- load_holway(PATHS$holway_combined)
  interactive_ok <- Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"
  ref <- build_holway_reference(con, holway_df, interactive_ok = interactive_ok)
  write.csv(ref, "data/outputs/reference/holway_reference_checklist.csv", row.names = FALSE, na = "")
  message("Wrote ", sum(ref$resolved), " resolved of ", nrow(ref), " Holway rows.")
}
