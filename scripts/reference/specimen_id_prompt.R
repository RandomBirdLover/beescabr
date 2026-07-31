# =============================================================
# reference/specimen_id_prompt.R
# beescabr -- INTERACTIVE taxon_id resolver for specimen-only additions.
#
# The automatic resolver (resolve_missing_ids.R) is deliberately conservative: it
# fills a taxon_id ONLY on an unambiguous name+rank+parent match, so a spelling
# variant (daggetti vs daggettii) or a taxon never in the lookup (e.g. Melissodes
# microstictus, flagged but not yet an addition) is LEFT BLANK. Those blanks are
# real, resolvable iNat taxa a human can confirm in seconds -- so ASK: search
# iNaturalist, SUGGEST the best hit, and let the reviewer accept / paste an id /
# skip / stop. The confirmed id is written back to specimen_additions.csv.
#
# PURE by design: fetch_fn (iNat /taxa search) and prompt_fn (readline) are
# injected, so the whole loop is unit-tested with fakes (test-specimen-idprompt.R).
# Depends on: dplyr. Uses inat_fetch_taxa_by_name() for the live search.
# =============================================================
suppressWarnings(suppressMessages(library(dplyr)))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# .spid_norm(): lowercase, letters+spaces only, squished -- for loose name comparison.
.spid_norm <- function(x) {
  x <- tolower(as.character(x %||% "")); x[is.na(x)] <- ""
  trimws(gsub("\\s+", " ", gsub("[^a-z ]", " ", x)))
}

# suggest_taxon(): PURE. From raw iNat /taxa results, pick the best SUGGESTION for
# `term` at `want_rank`: prefer an exact normalized name + rank match, else the
# first same-rank hit, else the first result with a real id. Returns
# list(id, name, rank) or NULL. (A suggestion only -- the human still confirms.)
suggest_taxon <- function(cands, term, want_rank = "species") {
  if (is.null(cands) || !length(cands)) return(NULL)
  want_nm <- .spid_norm(term); want_rk <- tolower(trimws(as.character(want_rank %||% "")))
  info <- lapply(cands, function(c) list(
    id   = suppressWarnings(as.integer(c$id %||% NA)),
    name = as.character(c$name %||% ""),
    rank = tolower(trimws(as.character(c$rank %||% "")))))
  info <- Filter(function(c) !is.na(c$id), info)
  if (!length(info)) return(NULL)
  exact <- Filter(function(c) c$rank == want_rk && .spid_norm(c$name) == want_nm, info)
  if (length(exact)) return(exact[[1]])
  same  <- Filter(function(c) c$rank == want_rk, info)
  if (length(same)) return(same[[1]])
  info[[1]]
}

# .spid_parse(): PURE. Map one human answer to an action.
#   ""            -> accept the suggested id (or reask if there is no suggestion)
#   digits        -> use that id
#   s / skip / n  -> leave blank
#   x / stop      -> halt
#   anything else -> reask
.spid_parse <- function(ans, suggested_id) {
  a <- tolower(trimws(as.character(ans %||% "")))
  if (a == "")                              return(list(action = if (!is.na(suggested_id)) "accept" else "reask", id = suggested_id))
  if (a %in% c("s", "skip", "n", "no", "none")) return(list(action = "skip", id = NA_integer_))
  if (a %in% c("x", "stop", "halt"))            return(list(action = "stop", id = NA_integer_))
  if (grepl("^[0-9]+$", a))                     return(list(action = "id",   id = as.integer(a)))
  list(action = "reask", id = NA_integer_)
}

# flag_specimen_ids(): PURE. Map each taxon in a specimen table (the raw specimen RECORD, or the
# taxonomy-flags table -- any df with genus..subspecies + ucsd_id/sdnhm_id) to the ucsd_id /
# sdnhm_id of the specimens carrying it, keyed "rank|name" on the FINEST-rank determination
# (lowercased) -- the same key the resolver searches each addition under. Sourced from the RECORD
# so the prompt shows "which specimens is this?" every time -- even on a re-run where the taxon is
# pending in additions but no longer freshly flagged. Blank / 0 ids dropped. Named character vector.
flag_specimen_ids <- function(flags) {
  empty <- stats::setNames(character(0), character(0))
  if (is.null(flags) || !nrow(flags)) return(empty)
  gcol <- function(col) if (col %in% names(flags)) as.character(flags[[col]]) else rep(NA_character_, nrow(flags))
  b2 <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x }
  strip_cx <- function(x) trimws(sub("^\\s*\\([^)]*\\)\\s*", "", b2(x)))
  g  <- b2(gcol("genus")); sg <- b2(gcol("subgenus")); cx <- strip_cx(gcol("complex"))
  sp <- b2(gcol("species")); ss <- b2(gcol("subspecies"))
  ucsd <- b2(gcol("ucsd_id")); sdnhm <- b2(gcol("sdnhm_id"))
  # finest ID rank + its name -- key on BOTH so a complex and a species of the SAME name
  # (e.g. the "Colletes simulans" complex vs the species) never share a bucket.
  rk  <- ifelse(sp != "" & ss != "", "subspecies",
         ifelse(sp != "",            "species",
         ifelse(cx != "",            "complex",
         ifelse(sg != "",            "subgenus", "genus"))))
  sci <- ifelse(sp != "" & ss != "", paste(g, sp, ss),
         ifelse(sp != "",            paste(g, sp),
         ifelse(cx != "",            cx,
         ifelse(sg != "",            sg, g))))
  key <- ifelse(trimws(sci) != "", paste(rk, tolower(trimws(sci)), sep = "|"), "")
  ks  <- unique(key[key != ""])
  if (!length(ks)) return(empty)
  vals <- vapply(ks, function(k) {
    rows   <- which(key == k)
    keepid <- function(v) { v <- unique(v[rows]); v[v != "" & v != "0"] }
    u <- keepid(ucsd); s <- keepid(sdnhm)
    parts <- c(if (length(u)) paste0("ucsd_id ", paste(u, collapse = ", ")),
               if (length(s)) paste0("sdnhm_id ", paste(s, collapse = ", ")))
    paste(parts, collapse = " | ")
  }, character(1))
  stats::setNames(vals, ks)
}

# resolve_specimen_additions_interactive(): fill blank taxon_ids in the additions
# data.frame by asking the user one taxon at a time, each with an iNat suggestion.
# Returns list(additions = <updated df>, stopped = <TRUE if user chose stop>).
# Non-interactive (batch) -> unchanged, never blocks automation. id_map (from
# flag_specimen_ids) is optional: when present, each prompt also shows the specimen
# ucsd_id/sdnhm_id behind that taxon so the reviewer can look them up in the records.
resolve_specimen_additions_interactive <- function(add_df, fetch_fn = NULL, prompt_fn = readline,
                                                   interactive_ok = TRUE, verbose = TRUE, id_map = NULL) {
  if (!interactive_ok || is.null(add_df) || !nrow(add_df) || !"taxon_id" %in% names(add_df))
    return(list(additions = add_df, stopped = FALSE))
  if (is.null(fetch_fn)) fetch_fn <- function(nm) inat_fetch_taxa_by_name(nm)
  add_df$taxon_id <- suppressWarnings(as.integer(add_df$taxon_id))
  need <- which(is.na(add_df$taxon_id))
  if (!length(need)) return(list(additions = add_df, stopped = FALSE))
  namecol <- if ("scientific_name" %in% names(add_df)) "scientific_name" else NA_character_
  if (verbose) message(sprintf("  [taxon-id] %d specimen taxa need a taxon_id -- confirm each (Enter accepts the iNat suggestion):", length(need)))
  for (i in need) {
    term <- if (!is.na(namecol) && !is.na(add_df[[namecol]][i]) && trimws(add_df[[namecol]][i]) != "")
              as.character(add_df[[namecol]][i])
            else trimws(paste(add_df$genus[i] %||% "", add_df$species[i] %||% ""))
    rank <- tolower(as.character(add_df$rank[i] %||% "species"))
    cands <- tryCatch(fetch_fn(term), error = function(e) list())
    sug   <- suggest_taxon(cands, term, rank)
    sug_id <- if (is.null(sug)) NA_integer_ else sug$id
    .k <- paste(tolower(trimws(rank)), tolower(trimws(term)), sep = "|")   # rank+name: keep complex vs species apart
    ids_hint <- if (!is.null(id_map) && .k %in% names(id_map)) id_map[[.k]] else NULL
    if (!is.null(ids_hint) && nzchar(ids_hint))
      message(sprintf("      specimens: %s  (look these up in your records)", ids_hint))
    repeat {
      msg <- if (is.null(sug))
        sprintf("    %s -- no iNat match. Paste a taxon_id, or  s  skip /  x  stop: ", term)
      else
        sprintf("    %s  ->  iNat: %s (id %d, %s).  [Enter accept / paste id / s skip / x stop]: ",
                term, sug$name, sug$id, sug$rank)
      pr <- .spid_parse(prompt_fn(msg), sug_id)
      if (pr$action == "stop") return(list(additions = add_df, stopped = TRUE))
      if (pr$action == "skip") break
      if (pr$action %in% c("accept", "id")) { add_df$taxon_id[i] <- pr$id; break }
      message("       (Enter to accept, a number to override, s to skip, x to stop)")
    }
  }
  list(additions = add_df, stopped = FALSE)
}

# specimen_additions.csv column order (lookup-shaped rows with FULL parent lineage).
.SA_COLS <- c("rank", "scientific_name", "taxon_id", "in_holway", "in_inat", "in_cabr_specimens",
              "verified", "kingdom", "phylum", "class", "order", "superfamily", "family",
              "subfamily", "tribe", "subtribe", "genus", "subgenus", "complex", "species",
              "subspecies", "common_name")

.sa_empty <- function() {
  d <- as.data.frame(matrix(character(0), nrow = 0, ncol = length(.SA_COLS)), stringsAsFactors = FALSE)
  names(d) <- .SA_COLS; d$taxon_id <- integer(0); d
}

# seed_additions_from_flags(): PURE. Turn each NEW flagged specimen taxon into a lookup-
# shaped additions row -- taxon_id BLANK (the prompt/build fills it) with EVERY PARENT rank
# carried across from the raw specimen `record` and the fixed bee lineage on top
# (Animalia/Arthropoda/Insecta/Apoidea). Each addition is seeded AT THE SPECIMEN'S FINEST
# ID RANK: species/subspecies (a binomial), a complex-only ID (rank "complex", named by its
# bare binomial), or a subgenus-only ID (rank "subgenus"). A genus-only flag is NOT seeded
# here -- an unknown genus is a different problem than a new taxon under a known genus.
# Complex names are stored WITHOUT the "(Complex) " tag so the iNat search term is clean.
# Dedupes within the batch + against `existing` (rank-aware when the file carries a rank).
# Returns the new rows (0-row frame if none).
seed_additions_from_flags <- function(flags, record, existing = NULL) {
  if (is.null(flags) || !nrow(flags)) return(.sa_empty())
  gcol <- function(df, col) if (col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, nrow(df))
  b2 <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x }
  strip_cx <- function(x) trimws(sub("^\\s*\\([^)]*\\)\\s*", "", b2(x)))
  fl <- data.frame(genus      = b2(gcol(flags, "genus")),
                   subgenus   = b2(gcol(flags, "subgenus")),
                   complex    = strip_cx(gcol(flags, "complex")),
                   species    = b2(gcol(flags, "species")),
                   subspecies = b2(gcol(flags, "subspecies")), stringsAsFactors = FALSE)
  fl <- fl[fl$genus != "", , drop = FALSE]
  if (!nrow(fl)) return(.sa_empty())
  # finest ID rank + the scientific_name AT that rank (genus-only -> NA -> dropped)
  finest <- function(g, sg, cx, sp, ss) {
    if (sp != "" && ss != "") return(c(rank = "subspecies", sci = paste(g, sp, ss)))
    if (sp != "")             return(c(rank = "species",    sci = paste(g, sp)))
    if (cx != "")             return(c(rank = "complex",    sci = cx))
    if (sg != "")             return(c(rank = "subgenus",   sci = sg))
    c(rank = NA_character_, sci = NA_character_)
  }
  fin <- t(mapply(finest, fl$genus, fl$subgenus, fl$complex, fl$species, fl$subspecies, USE.NAMES = FALSE))
  fl$rank <- fin[, "rank"]; fl$sci <- fin[, "sci"]
  fl <- fl[!is.na(fl$rank), , drop = FALSE]
  if (!nrow(fl)) return(.sa_empty())
  fl <- fl[!duplicated(tolower(paste(fl$rank, fl$sci))), , drop = FALSE]
  if (!is.null(existing) && "scientific_name" %in% names(existing)) {
    if ("rank" %in% names(existing)) {
      have <- tolower(trimws(paste(existing$rank, existing$scientific_name)))
      fl   <- fl[!(tolower(paste(fl$rank, fl$sci)) %in% have), , drop = FALSE]
    } else {
      have <- tolower(trimws(existing$scientific_name))
      fl   <- fl[!(tolower(fl$sci) %in% have), , drop = FALSE]
    }
  }
  if (!nrow(fl)) return(.sa_empty())
  # parent lineage (order/family/subfamily/tribe/subgenus) from the record row that matches
  # the specimen's finest identity (species, else complex, else subgenus, else genus).
  rec_row <- function(r) {
    if (is.null(record) || !"genus" %in% names(record)) return(NA_integer_)
    hit <- tolower(trimws(record$genus)) == tolower(r$genus)
    if (r$species != "" && "species" %in% names(record))
      hit <- hit & tolower(trimws(record$species)) == tolower(r$species)
    else if (r$complex != "" && "complex" %in% names(record))
      hit <- hit & tolower(strip_cx(record$complex)) == tolower(r$complex)
    else if (r$subgenus != "" && "subgenus" %in% names(record))
      hit <- hit & tolower(trimws(record$subgenus)) == tolower(r$subgenus)
    w <- which(hit); if (length(w)) w[1] else NA_integer_
  }
  rec_at <- function(w, col) {
    if (is.na(w) || is.null(record) || !col %in% names(record)) return("")
    v <- as.character(record[[col]][w]); if (is.na(v)) "" else trimws(v)
  }
  rows <- lapply(seq_len(nrow(fl)), function(i) {
    r <- fl[i, ]; w <- rec_row(r)
    data.frame(rank = r$rank, scientific_name = r$sci, taxon_id = NA_integer_,
               in_holway = "FALSE", in_inat = "FALSE", in_cabr_specimens = "TRUE", verified = "FALSE",
               kingdom = "Animalia", phylum = "Arthropoda", class = "Insecta",
               order = rec_at(w, "order"), superfamily = "Apoidea",
               family = rec_at(w, "family"), subfamily = rec_at(w, "subfamily"),
               tribe = rec_at(w, "tribe"), subtribe = "",
               genus = r$genus,
               subgenus = if (r$subgenus != "") r$subgenus else rec_at(w, "subgenus"),
               complex  = if (r$complex  != "") r$complex  else strip_cx(rec_at(w, "complex")),
               species    = if (r$rank %in% c("species", "subspecies")) r$species    else "",
               subspecies = if (r$rank == "subspecies")                 r$subspecies else "",
               common_name = "", stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)[.SA_COLS]
}

# resolve_specimen_taxa(): the DRIVER. Reads the curated additions + the taxonomy flags, folds
# every new flagged taxon into the additions (full lineage from `record_df`), then walks the
# reviewer through each blank taxon_id (iNat suggestion -> confirm) and writes the file back.
# Effect: a confirmed taxon enters the lookup on the next build, so its flag stops recurring.
# I/O is isolated here; the pieces above are pure + unit-tested.
resolve_specimen_taxa <- function(record_df,
                                  additions_path = if (exists("PATHS")) PATHS$specimen_additions else "data/reference/curated/specimen_additions.csv",
                                  flags_path = "data/specimens/specimens_clean/review/cabr_specimen_bee_taxonomy_flags.csv",
                                  fetch_fn = NULL, prompt_fn = readline, interactive_ok = TRUE,
                                  write = TRUE, verbose = TRUE) {
  if (!interactive_ok) return(invisible(NULL))
  rd <- function(p) if (!is.null(p) && file.exists(p))
    tryCatch(suppressWarnings(utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)), error = function(e) NULL) else NULL
  additions <- rd(additions_path); if (is.null(additions)) additions <- .sa_empty()
  .flags    <- rd(flags_path)
  new_rows  <- seed_additions_from_flags(.flags, record_df, additions)
  # read.csv infers the TRUE/FALSE columns as <logical> while the seeded rows are <character>;
  # coerce both sides to character so bind_rows never hits a type clash.
  .chr <- function(d) { if (ncol(d)) d[] <- lapply(d, as.character); d }
  additions <- .chr(additions); new_rows <- .chr(new_rows)
  combined  <- if (nrow(new_rows)) dplyr::bind_rows(additions, new_rows) else additions
  if (!"taxon_id" %in% names(combined)) combined$taxon_id <- NA_integer_
  if (!sum(is.na(suppressWarnings(as.integer(combined$taxon_id))))) {
    if (verbose) message("  [taxon-id] every specimen addition already has an id -- nothing to resolve.")
    return(invisible(combined))
  }
  if (is.null(fetch_fn)) fetch_fn <- function(nm) inat_fetch_taxa_by_name(nm)
  # specimen-id hints come from the RECORD (every specimen), so they show even when a taxon is
  # pending in additions but no longer freshly flagged (flags empty on a re-run).
  res <- resolve_specimen_additions_interactive(combined, fetch_fn, prompt_fn, interactive_ok = TRUE,
                                                verbose = verbose, id_map = flag_specimen_ids(record_df))
  if (write) {
    dir.create(dirname(additions_path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(res$additions, additions_path, row.names = FALSE, na = "")
    if (verbose) message(sprintf("  [taxon-id] wrote %d specimen additions -> %s  (folds into the lookup on the next build)",
                                 nrow(res$additions), additions_path))
  }
  invisible(res$additions)
}
