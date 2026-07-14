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
# retry_empty_search(): PURE. When the initial iNat name search returns nothing
# and the run is interactive, let a human supply alternate search terms (Python
# parity -- rescues misspelled or since-renamed Holway names). Returns the first
# non-empty result set, or the (still-empty) results if the user skips or the
# run is non-interactive. fetch_fn(term) does the lookup; prompt_fn() reads a line.
# ------------------------------------------------------------
retry_empty_search <- function(results, term, fetch_fn,
                               prompt_fn = readline, interactive_ok = TRUE) {
  cur <- term
  while (length(results) == 0 && isTRUE(interactive_ok)) {
    message("No iNat match for '", cur,
            "'. Enter an alternate name to search, or 'skip' to move on.")
    raw <- trimws(prompt_fn("Alternate search (or 'skip'): "))
    if (raw == "" || identical(raw, "skip")) break
    cur <- raw
    results <- fetch_fn(cur)
  }
  results
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
  # zero-result retry: offer a human an alternate search term (Python parity).
  if (length(results) == 0 && interactive_ok) {
    results <- retry_empty_search(
      results, term,
      fetch_fn = function(t) get_taxa_by_name(con, t, request_fn = request_fn),
      prompt_fn = prompt_fn, interactive_ok = interactive_ok)
  }
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

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ------------------------------------------------------------
# Clean reference-table layout -- mirrors sd_bee_taxonomy_lookup.csv (minus the
# lookup-only computed columns verified/holway_status/in_*): taxon_id,
# scientific_name, common_name, rank, then the 14 taxonomic levels in order,
# complex_taxon_id, source_sheet, resolved.
# ------------------------------------------------------------
HOLWAY_REF_LEVELS <- c("kingdom", "phylum", "class", "order", "superfamily",
                       "family", "subfamily", "tribe", "subtribe", "genus",
                       "subgenus", "complex", "species", "subspecies")
HOLWAY_REF_COLUMNS <- c("taxon_id", "scientific_name", "common_name", "rank",
                        HOLWAY_REF_LEVELS, "complex_taxon_id",
                        "source_sheet", "resolved")

# .bare_epithet(): last token of a binomial/trinomial ("Andrena annectens" ->
# "annectens"), matching the lookup's bare-epithet species/subspecies columns.
.bare_epithet <- function(x) {
  if (length(x) == 0 || is.na(x) || x == "") return(NA_character_)
  stringr::word(x, -1)
}

# tidy_holway_ref_row(): PURE. Reshape one parse_taxon_ranks() result plus the
# taxon's API name/common name into the clean reference-table layout. species/
# subspecies are reduced to bare epithets; scientific_name is the AUTHORITATIVE
# iNat taxon name (taxon$name), not a derived string.
tidy_holway_ref_row <- function(ranks, scientific_name, common_name, source_sheet) {
  tibble::tibble(
    taxon_id        = as.integer(ranks$taxon_id),
    scientific_name = scientific_name %||% NA_character_,
    common_name     = common_name %||% NA_character_,
    rank            = ranks$rank %||% NA_character_,
    kingdom     = ranks$taxon_kingdom_name,     phylum = ranks$taxon_phylum_name,
    class       = ranks$taxon_class_name,       order  = ranks$taxon_order_name,
    superfamily = ranks$taxon_superfamily_name, family = ranks$taxon_family_name,
    subfamily   = ranks$taxon_subfamily_name,   tribe  = ranks$taxon_tribe_name,
    subtribe    = ranks$taxon_subtribe_name,    genus  = ranks$taxon_genus_name,
    subgenus    = ranks$subgenus,               complex = ranks$complex,
    species     = .bare_epithet(ranks$taxon_species_name),
    subspecies  = .bare_epithet(ranks$taxon_subspecies_name),
    complex_taxon_id = ranks$complex_taxon_id,
    source_sheet = source_sheet, resolved = TRUE
  )
}

# unresolved_holway_ref_row(): PURE. Same layout for a Holway row we could not
# resolve to an iNat taxon -- NA taxon fields, but keep the family/genus and the
# ORIGINAL species_raw (with CF/MSN) so nothing is lost.
unresolved_holway_ref_row <- function(r) {
  tibble::tibble(
    taxon_id = NA_integer_, scientific_name = NA_character_,
    common_name = NA_character_, rank = NA_character_,
    kingdom = NA_character_, phylum = NA_character_, class = NA_character_,
    order = NA_character_, superfamily = NA_character_,
    family = r$family %||% NA_character_, subfamily = r$subfamily %||% NA_character_,
    tribe = r$tribe %||% NA_character_, subtribe = NA_character_,
    genus = r$genus %||% NA_character_, subgenus = r$subgenus %||% NA_character_,
    complex = NA_character_, species = r$species_raw %||% NA_character_,
    subspecies = NA_character_, complex_taxon_id = NA_integer_,
    source_sheet = r$source_sheet %||% NA_character_, resolved = FALSE
  )
}

# ------------------------------------------------------------
# build_holway_reference(): full interactive run over the Holway sheet.
# Produces a taxonomy tibble in the clean reference-table layout (one row per
# Holway entry, full ancestry) and returns it; the runner writes it to disk.
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
      rows[[i]] <- unresolved_holway_ref_row(r)
    } else {
      taxon <- get_taxon_by_id(con, res$taxon_id, request_fn = request_fn)
      ranks <- parse_taxon_ranks(taxon)
      rows[[i]] <- tidy_holway_ref_row(
        ranks,
        scientific_name = .scalar(taxon$name, NA_character_),
        common_name     = .scalar(taxon$preferred_common_name, NA_character_),
        source_sheet    = r$source_sheet)
    }
    if (i %% 25 == 0) message(sprintf("  Holway resolve: %d / %d", i, nrow(holway_df)))
  }
  dplyr::bind_rows(rows) |> dplyr::select(dplyr::any_of(HOLWAY_REF_COLUMNS))
}

# ------------------------------------------------------------
# Runner (only when executed directly, not when sourced for tests).
# ------------------------------------------------------------
if (identical(environment(), globalenv()) &&
    !is.na(Sys.getenv("BEESCABR_RUN_HOLWAY", unset = NA))) {
  source("scripts/config.R")
  source("scripts/engine/db/store_conn.R"); source("scripts/engine/db/taxon_store.R")
  source("scripts/engine/db/decision_store.R"); source("scripts/engine/api/inat_http.R")
  source("scripts/engine/api/inat_flatten.R"); source("scripts/engine/api/inat_cache.R")
  source("scripts/checklists/holway.R")

  con <- store_connect()
  on.exit(store_disconnect(con), add = TRUE)
  holway_df <- load_holway(PATHS$holway_combined)
  interactive_ok <- Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"
  ref <- build_holway_reference(con, holway_df, interactive_ok = interactive_ok)
  write.csv(ref, PATHS$holway_reference, row.names = FALSE, na = "")
  message("Wrote ", sum(ref$resolved), " resolved of ", nrow(ref), " Holway rows.")
}
