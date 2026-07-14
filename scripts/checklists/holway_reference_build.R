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
# .norm_name(): normalize a taxon name for exact matching -- lowercased, with
# the subspecies markers ("ssp.", "subsp.", "var.", "f.") removed. iNat's API
# name field is the bare trinomial ("Ashmeadiella cactorum basalis") while we
# search WITH "ssp." ("Ashmeadiella cactorum ssp. basalis"); normalizing both
# lets them match. PURE.
.norm_name <- function(x) {
  x <- tolower(x %||% "")
  x <- gsub("\\b(ssp|subsp|var|f)\\.?(\\s|$)", " ", x)
  trimws(gsub("\\s+", " ", x))
}

select_taxon_candidate <- function(results, described, search_term = NULL,
                                   is_subspecies = FALSE) {
  n <- length(results)
  if (n == 0) return(list(action = "skip", index = NA_integer_))

  # Exact name match (ignoring ssp./subsp./var.) auto-picks. This is what grabs
  # the correct subspecies (e.g. 313836) when iNat has it, and disambiguates
  # near-spellings (Andrena nigra vs nigrae).
  if (!is.null(search_term)) {
    want <- .norm_name(search_term)
    got  <- vapply(results, function(t) .norm_name(t$name), character(1))
    exact <- which(got == want)
    if (length(exact) >= 1) return(list(action = "pick", index = exact[[1]]))
  }

  # A subspecies with no exact match: do NOT fall back to the parent species --
  # route it to the ITIS keep/skip question instead.
  if (isTRUE(is_subspecies)) return(list(action = "skip", index = NA_integer_))

  if (n == 1) return(list(action = "pick", index = 1L))
  species_idx <- which(vapply(results, function(t) identical(t$rank, "species"), logical(1)))
  if (!described) return(list(action = "skip", index = NA_integer_))
  if (length(species_idx) == 1) return(list(action = "pick", index = species_idx[[1]]))
  list(action = "prompt", index = NA_integer_)   # 0 or >1 species -> ask
}

# .yn(): true when the prompt answer is yes. PURE-ish (prompt_fn injected).
.yn <- function(prompt_fn, msg) tolower(trimws(prompt_fn(msg))) %in% c("y", "yes")

# itis_disposition(): after iNat yields no usable match, ask whether the name is
# valid per ITIS. "keep" (valid, retain w/o an iNat id) or "skip" (unpublished/
# invalid). Non-interactive defaults to "skip". Impure (prompts).
itis_disposition <- function(term, prompt_fn = readline, interactive_ok = TRUE) {
  if (!isTRUE(interactive_ok)) return("skip")
  message("'", term, "' not found on iNaturalist. Check ITIS for it.")
  if (.yn(prompt_fn, "Valid species/subspecies in ITIS? (y/n): ")) "keep" else "skip"
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

# holway_resolution_plan(): PURE. Decide the iNat search string and whether the
# row is a subspecies, given the source sheet, genus, and the split species_raw
# plus the user's answer to "is this a subspecies?" (needed only for two-word
# Described entries). Returns list(term, is_subspecies, ask_subspecies).
#   - non-Described        -> search the genus alone (provisional)
#   - one-word Described    -> search "genus species"
#   - two-word Described    -> ask_subspecies = TRUE; when confirmed, search
#                             "genus species ssp. subspecies"
holway_resolution_plan <- function(source_sheet, genus, sp) {
  if (!identical(source_sheet, "Described"))
    return(list(term = genus, is_subspecies = FALSE, ask_subspecies = FALSE))
  if (!is.na(sp$subspecies))
    # iNat's name search wants the PLAIN trinomial -- inserting "ssp." makes it
    # miss valid subspecies (e.g. Anthophora bomboides stanfordiana). The exact-
    # match normalizer still strips ssp./subsp. so matching is unaffected.
    return(list(term = paste(genus, sp$species, sp$subspecies),
                is_subspecies = TRUE, ask_subspecies = TRUE))
  list(term = paste(genus, sp$species), is_subspecies = FALSE, ask_subspecies = FALSE)
}

# ------------------------------------------------------------
# resolve_holway_row(): resolve one row to a taxon id, consulting/recording the
# decision cache and (only when needed and allowed) prompting. The decision is
# keyed on the stable "genus species_raw" identity (unchanged from before), so
# prior picks are reused. Returns list(taxon_id, action, term). Impure.
# ------------------------------------------------------------
resolve_holway_row <- function(con, source_sheet, genus, species_raw,
                               request_fn = inat_request,
                               interactive_ok = TRUE,
                               prompt_fn = readline) {
  key <- holway_search_term(source_sheet, genus, species_raw)  # stable cache key

  decided <- decision_get(con, key)
  if (!is.null(decided)) {
    return(list(taxon_id = decided$chosen_taxon_id, action = decided$action, term = key))
  }

  sp   <- split_holway_species(species_raw)
  plan <- holway_resolution_plan(source_sheet, genus, sp)

  # Two-word Described entry: confirm it's really a subspecies before adding ssp.
  if (plan$ask_subspecies && interactive_ok) {
    if (!.yn(prompt_fn, sprintf("Is '%s %s %s' a subspecies? (y/n): ",
                                genus, sp$species, sp$subspecies))) {
      decision_put(con, key, "tentative")   # not a subspecies -> provisional, blank
      return(list(taxon_id = NA_integer_, action = "tentative", term = key))
    }
  }

  results <- get_taxa_by_name(con, plan$term, request_fn = request_fn)
  if (length(results) == 0 && interactive_ok) {
    results <- retry_empty_search(
      results, plan$term,
      fetch_fn = function(t) get_taxa_by_name(con, t, request_fn = request_fn),
      prompt_fn = prompt_fn, interactive_ok = interactive_ok)
  }

  choice <- select_taxon_candidate(results, described = identical(source_sheet, "Described"),
                                   search_term = plan$term, is_subspecies = plan$is_subspecies)

  if (choice$action == "pick") {
    chosen <- results[[choice$index]]
    decision_put(con, key, "pick", chosen$id)
    return(list(taxon_id = as.integer(chosen$id), action = "pick", term = key))
  }

  if (choice$action == "prompt" && interactive_ok) {
    message("\nAmbiguous: '", plan$term, "' -- choose a taxon:")
    for (i in seq_along(results)) {
      t <- results[[i]]
      message(sprintf("  [%d] id=%s %s (%s)", i, t$id, t$name, t$rank %||% "?"))
    }
    raw <- trimws(prompt_fn("Pick number (or 'skip'): "))
    idx <- suppressWarnings(as.integer(raw))
    if (!identical(raw, "skip") && !is.na(idx) && idx >= 1 && idx <= length(results)) {
      chosen <- results[[idx]]
      decision_put(con, key, "pick", chosen$id)
      return(list(taxon_id = as.integer(chosen$id), action = "pick", term = key))
    }
    # declined the prompt -> fall through to the ITIS question
  }

  # Nothing usable on iNat (empty, subspecies w/o exact match, or declined
  # prompt): ask the ITIS keep/skip question.
  disp <- if (interactive_ok) itis_disposition(plan$term, prompt_fn, interactive_ok) else "skip"
  decision_put(con, key, disp)
  list(taxon_id = NA_integer_, action = disp, term = key)
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
                        "source_sheet", "resolved", "itis_valid")

# .strip_parens(): "(Hexosmia)" -> "Hexosmia"; NA/"" -> NA.
.strip_parens <- function(x) {
  if (length(x) == 0 || is.na(x) || x == "") return(NA_character_)
  out <- trimws(gsub("[()]", "", x))
  if (out == "") NA_character_ else out
}

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
tidy_holway_ref_row <- function(ranks, scientific_name, common_name, source_sheet,
                                itis_valid = NA) {
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
    source_sheet = source_sheet, resolved = TRUE, itis_valid = as.logical(itis_valid)
  )
}

# unresolved_holway_ref_row(): PURE. Clean-layout row for a Holway entry with no
# iNat taxon. species_raw is split into species + subspecies; when is_subspecies
# is TRUE the row is rank="subspecies" and scientific_name is the trinomial,
# otherwise rank="species" (species = the cleaned epithet, which may be two
# provisional words) and scientific_name the binomial. scientific_name is built
# from Holway's OWN names here -- the only place we construct it, because no iNat
# taxon exists to source it from. itis_valid: TRUE (valid, not on iNat),
# FALSE (checked ITIS, not valid), or NA (provisional / not asked).
unresolved_holway_ref_row <- function(r, itis_valid = NA, is_subspecies = FALSE) {
  sp      <- split_holway_species(r$species_raw %||% "")
  genus   <- r$genus %||% NA_character_
  if (isTRUE(is_subspecies)) {
    species_col    <- sp$species
    subspecies_col <- sp$subspecies
    rank           <- "subspecies"
  } else {
    species_col    <- clean_holway_species(r$species_raw %||% "")
    species_col    <- ifelse(is.na(species_col) | species_col == "", NA_character_, species_col)
    subspecies_col <- NA_character_
    rank           <- if (!is.na(species_col)) "species" else NA_character_
  }
  parts <- c(genus, species_col, if (isTRUE(is_subspecies)) subspecies_col else NULL)
  parts <- parts[!is.na(parts) & parts != ""]
  sci   <- if (length(parts) > 0) paste(parts, collapse = " ") else NA_character_

  tibble::tibble(
    taxon_id = NA_integer_, scientific_name = sci,
    common_name = NA_character_, rank = rank,
    kingdom = NA_character_, phylum = NA_character_, class = NA_character_,
    order = NA_character_, superfamily = NA_character_,
    family = r$family %||% NA_character_, subfamily = r$subfamily %||% NA_character_,
    tribe = r$tribe %||% NA_character_, subtribe = NA_character_,
    genus = genus, subgenus = .strip_parens(r$subgenus %||% NA_character_),
    complex = NA_character_, species = species_col, subspecies = subspecies_col,
    complex_taxon_id = NA_integer_,
    source_sheet = r$source_sheet %||% NA_character_, resolved = FALSE,
    itis_valid = as.logical(itis_valid)
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
    # a two-word entry that reached keep/skip was a confirmed subspecies
    is_ss <- !is.na(split_holway_species(r$species_raw %||% "")$subspecies)
    rows[[i]] <- if (!is.na(res$taxon_id)) {
      taxon <- get_taxon_by_id(con, res$taxon_id, request_fn = request_fn)
      ranks <- parse_taxon_ranks(taxon)
      tidy_holway_ref_row(
        ranks,
        scientific_name = .scalar(taxon$name, NA_character_),
        common_name     = .scalar(taxon$preferred_common_name, NA_character_),
        source_sheet    = r$source_sheet, itis_valid = NA)
    } else switch(res$action %||% "skip",
      keep      = unresolved_holway_ref_row(r, itis_valid = TRUE,  is_subspecies = is_ss),
      skip      = unresolved_holway_ref_row(r, itis_valid = FALSE, is_subspecies = is_ss),
      tentative = unresolved_holway_ref_row(r, itis_valid = NA,    is_subspecies = FALSE),
      unresolved_holway_ref_row(r, itis_valid = NA, is_subspecies = is_ss)
    )
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
