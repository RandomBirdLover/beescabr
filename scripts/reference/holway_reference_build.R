# =============================================================
# reference/holway_reference_build.R
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
# Run: Rscript scripts/reference/holway_reference_build.R
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

  rnk <- vapply(results, function(t) t$rank %||% "", character(1))
  # NEVER resolve to a complex. iNat files a species and its same-named complex
  # side by side -- "Andrena osmioides" the SPECIES, and "Osmioides Species Group",
  # a COMPLEX whose scientific name is also "Andrena osmioides". The species is the
  # target (closest true rank); its complex is reached later by roll-up (the species
  # row carries the parent complex via its ancestry). So only the non-complex
  # candidates are eligible; if every result is a complex, take nothing.
  ok <- which(rnk != "complex")
  if (length(ok) == 0) return(list(action = "skip", index = NA_integer_))

  # Exact name match (ignoring ssp./subsp./var.) auto-picks -- but ONLY at the
  # ranks Holway actually uses: species or subspecies. Matching on name alone was
  # the bug: a same-named aggregate (the "Andrena osmioides" COMPLEX, or a section)
  # equalled the search string and got picked. Restricting to species/subspecies
  # (preferring species) makes the aggregate ineligible. This still grabs the
  # correct subspecies (313836) and disambiguates near-spellings (nigra vs nigrae).
  if (!is.null(search_term)) {
    want <- .norm_name(search_term)
    got  <- vapply(results, function(t) .norm_name(t$name), character(1))
    exact <- which(got == want & rnk %in% c("species", "subspecies"))
    if (length(exact) >= 1) {
      sp <- exact[rnk[exact] == "species"]
      return(list(action = "pick", index = (if (length(sp)) sp else exact)[[1]]))
    }
  }

  # A subspecies with no exact match: do NOT fall back to the parent species --
  # route it to the ITIS keep/skip question instead.
  if (isTRUE(is_subspecies)) return(list(action = "skip", index = NA_integer_))

  if (length(ok) == 1) return(list(action = "pick", index = ok[[1]]))
  species_idx <- ok[rnk[ok] == "species"]
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
# holway_search_term(): the string to search iNat for a Holway row -- ALSO the
# stable decision-cache key.
#   Described    -> "Genus species_raw" (UNCHANGED, so already-made picks still hit
#                   the cache and are never re-prompted).
#   non-Described (Tentative/Unpublished) -> "Genus <cleaned epithet>". These carry
#                   real names once the CF/MSN/aff./sp. nov. marker is stripped
#                   (e.g. "Andrena annectens" -> 573509), so we resolve them like a
#                   species instead of collapsing every one to a bare genus row.
#                   Falls back to the genus alone when nothing is left after
#                   stripping (a bare "sp. nov.").
# ------------------------------------------------------------
holway_search_term <- function(source_sheet, genus, species_raw) {
  if (identical(source_sheet, "Described")) return(trimws(paste(genus, species_raw)))
  ep <- clean_holway_species(if (is.na(species_raw)) "" else species_raw)
  if (!is.na(ep) && ep != "") trimws(paste(genus, ep)) else trimws(genus)
}

# .needs_alt_search(): PURE. Should we ask the human for an alternate name? YES when
# the search found NOTHING, or when the only hits are a same-named COMPLEX with no
# species/subspecies (iNat elevated the name and renamed the species). A genus-only
# result (the bare "sp. nov." fallback) does NOT trigger a retry -- that genus hit
# is expected, not a miss.
.needs_alt_search <- function(results) {
  if (length(results) == 0) return(TRUE)
  rnk <- vapply(results, function(t) t$rank %||% "", character(1))
  any(rnk == "complex") && !any(rnk %in% c("species", "subspecies"))
}

# ------------------------------------------------------------
# retry_empty_search(): PURE. When the initial iNat name search returns no usable
# species/subspecies -- either NOTHING, or only a same-named complex -- and the run
# is interactive, let a human supply alternate search terms (rescues misspelled or
# since-renamed Holway names, and the "iNat only has the complex" case). Returns the
# first usable result set, or the last results if the user skips / non-interactive.
# fetch_fn(term) does the lookup; prompt_fn() reads a line.
# ------------------------------------------------------------
retry_empty_search <- function(results, term, fetch_fn,
                               prompt_fn = readline, interactive_ok = TRUE) {
  cur <- term
  while (.needs_alt_search(results) && isTRUE(interactive_ok)) {
    lead <- if (length(results) == 0) paste0("No iNat match for '", cur, "'.")
            else paste0("iNat has only a complex (no species/subspecies) for '", cur, "'.")
    message(lead, " Enter an alternate name to search, or 'none' if not found.")
    raw <- trimws(prompt_fn("Alternate search (or 'none'): "))
    if (raw == "" || raw %in% c("none", "skip")) break
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
  if (!identical(source_sheet, "Described")) {
    # Tentative/Unpublished: resolve the CLEANED "Genus epithet" (sp$species is
    # already stripped of CF/MSN/aff./sp. nov.) so it can match its real iNat
    # taxon; fall back to the genus alone when there's no epithet left.
    if (!is.na(sp$species) && sp$species != "")
      return(list(term = trimws(paste(genus, sp$species)),
                  is_subspecies = FALSE, ask_subspecies = FALSE))
    return(list(term = trimws(genus), is_subspecies = FALSE, ask_subspecies = FALSE))
  }
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
# parse_slash_options(): PURE. Holway writes "epithet A / epithet B" for a
# synonym or uncertain-identity pair. Split on "/" and build the candidate full
# names ("Bombus californicus", "Bombus fervidus").
parse_slash_options <- function(genus, species_raw) {
  parts <- trimws(strsplit(species_raw, "/", fixed = TRUE)[[1]])
  parts <- parts[parts != ""]
  vapply(parts, function(p) trimws(paste(genus, clean_holway_species(p))),
         character(1), USE.NAMES = FALSE)
}

# resolve_slash_answer(): PURE. Map the user's answer to a search term:
# a number -> that option; 'none' / '' -> NA (give up); anything else ->
# "genus <typed epithet>" so they can supply a current name not in the pair.
resolve_slash_answer <- function(raw, genus, opts) {
  raw <- trimws(raw)
  if (raw == "" || raw %in% c("none", "skip")) return(NA_character_)
  idx <- suppressWarnings(as.integer(raw))
  if (!is.na(idx) && idx >= 1 && idx <= length(opts)) opts[[idx]] else paste(genus, raw)
}

# .resolve_term(): shared search -> exact-match select -> keep/skip for a single
# search term. Impure. Returns list(taxon_id, action, term).
.resolve_term <- function(con, key, term, is_subspecies, described,
                          request_fn, interactive_ok, prompt_fn) {
  results <- get_taxa_by_name(con, term, request_fn = request_fn)
  if (.needs_alt_search(results) && interactive_ok) {
    results <- retry_empty_search(
      results, term,
      fetch_fn = function(t) get_taxa_by_name(con, t, request_fn = request_fn),
      prompt_fn = prompt_fn, interactive_ok = interactive_ok)
  }
  choice <- select_taxon_candidate(results, described = described,
                                   search_term = term, is_subspecies = is_subspecies)
  if (choice$action == "pick") {
    chosen <- results[[choice$index]]
    decision_put(con, key, "pick", chosen$id)
    return(list(taxon_id = as.integer(chosen$id), action = "pick", term = key))
  }
  if (choice$action == "prompt" && interactive_ok) {
    message("\nMore than one match for '", term, "' -- choose one:")
    for (i in seq_along(results)) {
      t <- results[[i]]
      message(sprintf("  [%d] id=%s %s (%s)", i, t$id, t$name, t$rank %||% "?"))
    }
    raw <- trimws(prompt_fn("Pick a number, or 'none' if none fit: "))
    idx <- suppressWarnings(as.integer(raw))
    if (!(raw %in% c("none", "skip")) && !is.na(idx) && idx >= 1 && idx <= length(results)) {
      chosen <- results[[idx]]
      decision_put(con, key, "pick", chosen$id)
      return(list(taxon_id = as.integer(chosen$id), action = "pick", term = key))
    }
  }
  # Nothing usable on iNat -- ask the ITIS keep/skip question.
  disp <- if (interactive_ok) itis_disposition(term, prompt_fn, interactive_ok) else "skip"
  decision_put(con, key, disp)
  list(taxon_id = NA_integer_, action = disp, term = key)
}

resolve_holway_row <- function(con, source_sheet, genus, species_raw,
                               request_fn = inat_request,
                               interactive_ok = TRUE,
                               prompt_fn = readline,
                               force = FALSE) {
  key <- holway_search_term(source_sheet, genus, species_raw)  # stable cache key

  # 'aff.' ("affinis") flags a species NEAR the named one, NOT that species, so it
  # must never resolve to that taxon. Check this BEFORE the decision cache: an aff.
  # name strips to the SAME search key as its Described sibling ("aff. miserabilis"
  # -> "Habropoda miserabilis"), so reading the cache here would inherit the
  # sibling's taxon_id. Short-circuit to unresolved and DON'T write the cache, so
  # the shared key stays owned by the real species.
  if (!identical(source_sheet, "Described")) {
    qual <- holway_qualifier(species_raw)
    if (!is.na(qual) && grepl("aff", qual, ignore.case = TRUE))
      return(list(taxon_id = NA_integer_, action = "tentative", term = key))
  }

  # force = TRUE bypasses the decision cache and re-resolves from scratch. Used to
  # auto-heal an old complex mis-pick (the cached decision would otherwise stick).
  decided <- if (force) NULL else decision_get(con, key)
  if (!is.null(decided)) {
    return(list(taxon_id = decided$chosen_taxon_id, action = decided$action, term = key))
  }

  # SLASH: "epithet A / epithet B" -- a synonym/uncertainty pair. Only the user
  # knows which name is current, so they pick which one to resolve.
  if (grepl("/", species_raw, fixed = TRUE)) {
    opts <- parse_slash_options(genus, species_raw)
    term <- NA_character_
    if (interactive_ok && length(opts) > 0) {
      message("'", trimws(paste(genus, species_raw)), "' lists ", length(opts), " names:")
      for (i in seq_along(opts)) message(sprintf("  [%d] %s", i, opts[[i]]))
      term <- resolve_slash_answer(prompt_fn("Which to use? (number, a name, or 'none'): "),
                                   genus, opts)
    }
    if (is.na(term)) {
      decision_put(con, key, "skip")
      return(list(taxon_id = NA_integer_, action = "skip", term = key))
    }
    return(.resolve_term(con, key, term, is_subspecies = FALSE, described = TRUE,
                         request_fn, interactive_ok, prompt_fn))
  }

  sp   <- split_holway_species(species_raw)
  plan <- holway_resolution_plan(source_sheet, genus, sp)

  # Two-word Described entry: confirm it's really a subspecies.
  if (plan$ask_subspecies && interactive_ok) {
    if (!.yn(prompt_fn, sprintf("Is '%s %s %s' a subspecies? (y/n): ",
                                genus, sp$species, sp$subspecies))) {
      decision_put(con, key, "tentative")   # not a subspecies -> provisional, blank
      return(list(taxon_id = NA_integer_, action = "tentative", term = key))
    }
  }

  .resolve_term(con, key, plan$term, plan$is_subspecies,
                identical(source_sheet, "Described"),
                request_fn, interactive_ok, prompt_fn)
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
# complex_taxon_id is intentionally omitted from the reference-table output
# (human-facing); the value still exists internally for the checklists.
HOLWAY_REF_COLUMNS <- c("taxon_id", "scientific_name", "common_name", "rank",
                        "source_sheet", "qualifier", "resolved", "itis_valid",
                        HOLWAY_REF_LEVELS)

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

# .ssp_scientific_name(): format a subspecies name with the "ssp." marker for
# display -- "Ashmeadiella cactorum ssp. basalis". Falls back to the binomial
# when there's no subspecies epithet.
.ssp_scientific_name <- function(genus, species, subspecies) {
  parts <- c(genus, species)
  parts <- parts[!is.na(parts) & parts != ""]
  base  <- if (length(parts) > 0) paste(parts, collapse = " ") else NA_character_
  if (!is.na(subspecies) && subspecies != "" && !is.na(base))
    paste0(base, " ssp. ", subspecies) else base
}

# tidy_holway_ref_row(): PURE. Reshape one parse_taxon_ranks() result plus the
# taxon's API name/common name into the clean reference-table layout. species/
# subspecies are reduced to bare epithets; scientific_name is the AUTHORITATIVE
# iNat taxon name (taxon$name), not a derived string.
tidy_holway_ref_row <- function(ranks, scientific_name, common_name, source_sheet,
                                itis_valid = NA, qualifier = NA_character_,
                                holway_subgenus = NA_character_) {
  sp_ep <- .bare_epithet(ranks$taxon_species_name)
  ss_ep <- .bare_epithet(ranks$taxon_subspecies_name)
  # subspecies get the "ssp." display form; everything else keeps the API name
  sci <- if (identical(ranks$rank, "subspecies") && !is.na(ss_ep))
    .ssp_scientific_name(ranks$taxon_genus_name, sp_ep, ss_ep)
  else scientific_name %||% NA_character_
  # prefer iNat's subgenus; fall back to Holway's own (e.g. a resolved tentative
  # species whose iNat ancestry omits the subgenus still keeps "(Micandrena)").
  subg <- if (is.na(ranks$subgenus)) .strip_parens(holway_subgenus) else ranks$subgenus
  tibble::tibble(
    taxon_id        = as.integer(ranks$taxon_id),
    scientific_name = sci,
    common_name     = common_name %||% NA_character_,
    rank            = ranks$rank %||% NA_character_,
    kingdom     = ranks$taxon_kingdom_name,     phylum = ranks$taxon_phylum_name,
    class       = ranks$taxon_class_name,       order  = ranks$taxon_order_name,
    superfamily = ranks$taxon_superfamily_name, family = ranks$taxon_family_name,
    subfamily   = ranks$taxon_subfamily_name,   tribe  = ranks$taxon_tribe_name,
    subtribe    = ranks$taxon_subtribe_name,    genus  = ranks$taxon_genus_name,
    subgenus    = subg,                         complex = ranks$complex,
    species     = sp_ep,
    subspecies  = ss_ep,
    complex_taxon_id = ranks$complex_taxon_id,
    source_sheet = source_sheet, qualifier = qualifier,
    resolved = TRUE, itis_valid = as.logical(itis_valid)
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
unresolved_holway_ref_row <- function(r, itis_valid = NA, is_subspecies = FALSE,
                                      qualifier = NA_character_) {
  sp      <- split_holway_species(r$species_raw %||% "")
  genus   <- r$genus %||% NA_character_
  if (isTRUE(is_subspecies)) {
    species_col    <- sp$species
    subspecies_col <- sp$subspecies
    rank           <- "subspecies"
    sci            <- .ssp_scientific_name(genus, species_col, subspecies_col)
  } else {
    species_col    <- clean_holway_species(r$species_raw %||% "")
    species_col    <- ifelse(is.na(species_col) | species_col == "", NA_character_, species_col)
    subspecies_col <- NA_character_
    rank           <- if (!is.na(species_col)) "species" else NA_character_
    parts <- c(genus, species_col)
    parts <- parts[!is.na(parts) & parts != ""]
    sci   <- if (length(parts) > 0) paste(parts, collapse = " ") else NA_character_
  }

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
    source_sheet = r$source_sheet %||% NA_character_, qualifier = qualifier,
    resolved = FALSE, itis_valid = as.logical(itis_valid)
  )
}

# ------------------------------------------------------------
# ROLL-UP: a Holway species NOT on iNat inherits the nearest PARENT taxon's id +
# full ancestry -- the same-named species-complex (prompted), else Holway's subgenus,
# else Holway's genus. Fixes the "unresolved rows have no taxon_id / no higher
# taxonomy" gap: every Holway bee ends up placed in the tree with an id.
# ------------------------------------------------------------
# .pick_rank(): PURE. First result whose rank is in `want`; with strict=TRUE ONLY an
# exact (ssp-normalized) name match on want_name qualifies. Returns the taxon or NULL.
.pick_rank <- function(results, want, want_name = NULL, strict = FALSE) {
  if (length(results) == 0) return(NULL)
  rnk  <- vapply(results, function(t) t$rank %||% "", character(1))
  elig <- which(rnk %in% want)
  if (!length(elig)) return(NULL)
  if (!is.null(want_name)) {
    nm <- vapply(results, function(t) .norm_name(t$name %||% ""), character(1))
    ex <- elig[nm[elig] == .norm_name(want_name)]
    if (length(ex)) return(results[[ex[[1]]]])
    if (strict) return(NULL)
  }
  results[[elig[[1]]]]
}

# rollup_parent(): nearest on-iNat parent for an unresolved Holway row. Impure
# (fetches + may prompt). complex (PROMPT; auto when non-interactive) -> subgenus ->
# genus, each requiring an EXACT name match. Returns a taxon or NULL.
rollup_parent <- function(con, r, request_fn = inat_request,
                          prompt_fn = readline, interactive_ok = TRUE) {
  fetch <- function(term) if (is.null(term) || is.na(term) || !nzchar(trimws(term))) list()
                          else get_taxa_by_name(con, term, request_fn = request_fn)
  sp_ep <- clean_holway_species(r$species_raw %||% "")
  binom <- trimws(paste(r$genus %||% "", if (is.na(sp_ep)) "" else sp_ep))
  cx <- .pick_rank(fetch(binom), "complex", want_name = binom, strict = TRUE)
  if (!is.null(cx)) {
    if (!isTRUE(interactive_ok)) return(cx)
    if (.yn(prompt_fn, sprintf("'%s' is not a species on iNat, but the complex '%s' (id %s) matches. Roll up to it? (y/n): ",
                               binom, cx$name %||% "?", cx$id))) return(cx)
  }
  subg <- .strip_parens(r$subgenus %||% NA_character_)
  if (!is.na(subg)) {
    sg <- .pick_rank(fetch(subg), "subgenus", want_name = subg, strict = TRUE)
    if (!is.null(sg)) return(sg)
  }
  g <- .pick_rank(fetch(r$genus %||% NA_character_), "genus", want_name = r$genus, strict = TRUE)
  if (!is.null(g)) return(g)
  NULL
}

# build_rollup_row(): PURE (given the parent taxon). Reshape the parent into a
# reference row, then stamp the Holway species/subspecies epithet + binomial name.
# resolved = FALSE marks it a roll-up (taxon_id present, not a direct species hit);
# `rank` stays the parent's rank (complex/subgenus/genus) so the level is visible.
build_rollup_row <- function(parent_taxon, r, is_subspecies = FALSE,
                             qualifier = NA_character_, itis_valid = NA) {
  ranks <- parse_taxon_ranks(parent_taxon)
  sp <- split_holway_species(r$species_raw %||% "")
  if (isTRUE(is_subspecies)) { sp_col <- sp$species; ss_col <- sp$subspecies }
  else {
    sp_col <- clean_holway_species(r$species_raw %||% "")
    sp_col <- if (is.na(sp_col) || sp_col == "") NA_character_ else sp_col
    ss_col <- NA_character_
  }
  gen <- r$genus %||% ranks$taxon_genus_name
  sci <- .ssp_scientific_name(gen, sp_col, ss_col)
  row <- tidy_holway_ref_row(ranks, scientific_name = sci, common_name = NA_character_,
                             source_sheet = r$source_sheet %||% NA_character_,
                             itis_valid = itis_valid, qualifier = qualifier,
                             holway_subgenus = r$subgenus %||% NA_character_)
  row$species         <- sp_col
  row$subspecies      <- ss_col
  row$scientific_name <- sci
  row$resolved        <- FALSE
  row
}

# ------------------------------------------------------------
# build_holway_reference(): full interactive run over the Holway sheet.
# Produces a taxonomy tibble in the clean reference-table layout (one row per
# Holway entry, full ancestry) and returns it; the runner writes it to disk.
# ------------------------------------------------------------
build_holway_reference <- function(con, holway_df,
                                   request_fn = inat_request,
                                   interactive_ok = TRUE) {
  # Batch-prefetch every already-decided taxon in full (with ancestors) so the
  # per-row get_taxon_by_id() calls below are cache hits -- turns hundreds of
  # single requests into a few throttled batch requests.
  if (exists("prefetch_taxa")) {
    decided_ids <- integer(0)
    for (i in seq_len(nrow(holway_df))) {
      d <- decision_get(con, holway_search_term(holway_df$source_sheet[i],
                                                holway_df$genus[i], holway_df$species_raw[i]))
      if (!is.null(d) && !is.na(d$chosen_taxon_id)) decided_ids <- c(decided_ids, d$chosen_taxon_id)
    }
    if (length(decided_ids) > 0)
      prefetch_taxa(con, unique(decided_ids), request_fn = request_fn)
  }

  rows <- vector("list", nrow(holway_df))
  for (i in seq_len(nrow(holway_df))) {
    r <- holway_df[i, ]
    res <- resolve_holway_row(con, r$source_sheet, r$genus, r$species_raw,
                              request_fn = request_fn, interactive_ok = interactive_ok)
    # Fetch the resolved taxon once. AUTO-HEAL an old complex mis-pick: if the
    # cached decision landed on a complex-rank taxon, re-resolve with the fixed
    # rule (which prefers the same-named species and never takes a complex solo).
    taxon <- if (!is.na(res$taxon_id))
      get_taxon_by_id(con, res$taxon_id, request_fn = request_fn) else NULL
    if (!is.null(taxon) && identical(parse_taxon_ranks(taxon)$rank, "complex")) {
      res   <- resolve_holway_row(con, r$source_sheet, r$genus, r$species_raw,
                                  request_fn = request_fn, interactive_ok = interactive_ok,
                                  force = TRUE)
      taxon <- if (!is.na(res$taxon_id))
        get_taxon_by_id(con, res$taxon_id, request_fn = request_fn) else NULL
    }
    # a two-word entry that reached keep/skip was a confirmed subspecies (but a
    # slash "A / B" pair is never a subspecies)
    is_ss <- !grepl("/", r$species_raw %||% "", fixed = TRUE) &&
             !is.na(split_holway_species(r$species_raw %||% "")$subspecies)
    qual  <- holway_qualifier(r$species_raw %||% "")   # "CF"/"MSN"/"aff."/"sp. nov." or NA
    rows[[i]] <- if (!is.null(taxon)) {
      ranks <- parse_taxon_ranks(taxon)
      tidy_holway_ref_row(
        ranks,
        scientific_name = .scalar(taxon$name, NA_character_),
        common_name     = .scalar(taxon$preferred_common_name, NA_character_),
        source_sheet    = r$source_sheet, itis_valid = NA,
        qualifier = qual, holway_subgenus = r$subgenus %||% NA_character_)
    } else {
      # No direct iNat species/subspecies -> ROLL UP to the nearest parent
      # (complex -> subgenus -> genus) so the row still gets a taxon_id + ancestry.
      itis   <- switch(res$action %||% "skip", keep = TRUE, skip = FALSE, NA)
      parent <- tryCatch(rollup_parent(con, r, request_fn = request_fn,
                                       interactive_ok = interactive_ok),
                         error = function(e) NULL)
      if (!is.null(parent))
        build_rollup_row(parent, r, is_subspecies = is_ss, qualifier = qual, itis_valid = itis)
      else
        unresolved_holway_ref_row(r, itis_valid = itis, is_subspecies = is_ss, qualifier = qual)
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
  source("scripts/observations/engine/db/store_conn.R"); source("scripts/observations/engine/db/taxon_store.R")
  source("scripts/observations/engine/db/decision_store.R"); source("scripts/observations/engine/api/inat_http.R")
  source("scripts/observations/engine/api/inat_flatten.R"); source("scripts/observations/engine/api/inat_cache.R")
  source("scripts/reference/holway.R")

  con <- store_connect()
  on.exit(store_disconnect(con), add = TRUE)
  holway_df <- load_holway(PATHS$holway_combined)
  interactive_ok <- Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"
  ref <- build_holway_reference(con, holway_df, interactive_ok = interactive_ok)
  write.csv(ref, PATHS$holway_reference, row.names = FALSE, na = "")
  message("Wrote ", sum(ref$resolved), " resolved of ", nrow(ref), " Holway rows.")
}
