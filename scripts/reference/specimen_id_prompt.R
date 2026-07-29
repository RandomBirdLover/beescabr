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

# resolve_specimen_additions_interactive(): fill blank taxon_ids in the additions
# data.frame by asking the user one taxon at a time, each with an iNat suggestion.
# Returns list(additions = <updated df>, stopped = <TRUE if user chose stop>).
# Non-interactive (batch) -> unchanged, never blocks automation.
resolve_specimen_additions_interactive <- function(add_df, fetch_fn = NULL, prompt_fn = readline,
                                                   interactive_ok = TRUE, verbose = TRUE) {
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

# seed_additions_from_flags(): PURE. Turn each NEW flagged specimen taxon (genus+species not
# already in `existing` additions) into a lookup-shaped additions row -- taxon_id BLANK (the
# prompt fills it), and EVERY PARENT rank carried across from the raw specimen `record`
# (order/family/subfamily/tribe/genus/subgenus/complex), with the fixed bee lineage on top
# (Animalia/Arthropoda/Insecta/Apoidea). Returns the new rows (0-row frame if none).
seed_additions_from_flags <- function(flags, record, existing = NULL) {
  if (is.null(flags) || !nrow(flags)) return(.sa_empty())
  gcol <- function(df, col) if (col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, nrow(df))
  fl <- data.frame(genus      = trimws(gcol(flags, "genus")),
                   species    = trimws(gcol(flags, "species")),
                   subspecies = trimws(gcol(flags, "subspecies")), stringsAsFactors = FALSE)
  fl <- fl[!is.na(fl$genus) & fl$genus != "" & !is.na(fl$species) & fl$species != "", , drop = FALSE]
  if (!nrow(fl)) return(.sa_empty())
  fl$sci <- paste(fl$genus, fl$species)
  fl <- fl[!duplicated(tolower(fl$sci)), , drop = FALSE]
  have <- if (!is.null(existing) && "scientific_name" %in% names(existing))
    tolower(trimws(existing$scientific_name)) else character(0)
  fl <- fl[!(tolower(fl$sci) %in% have), , drop = FALSE]
  if (!nrow(fl)) return(.sa_empty())
  rec_at <- function(gen, sp, col) {
    if (is.null(record) || !all(c("genus", "species") %in% names(record)) || !col %in% names(record)) return("")
    hit <- which(tolower(trimws(record$genus)) == tolower(gen) & tolower(trimws(record$species)) == tolower(sp))
    if (!length(hit)) return("")
    v <- as.character(record[[col]][hit[1]]); if (is.na(v)) "" else trimws(v)
  }
  rows <- lapply(seq_len(nrow(fl)), function(i) {
    gen <- fl$genus[i]; sp <- fl$species[i]
    data.frame(rank = "species", scientific_name = fl$sci[i], taxon_id = NA_integer_,
               in_holway = "FALSE", in_inat = "FALSE", in_cabr_specimens = "TRUE", verified = "FALSE",
               kingdom = "Animalia", phylum = "Arthropoda", class = "Insecta",
               order = rec_at(gen, sp, "order"), superfamily = "Apoidea",
               family = rec_at(gen, sp, "family"), subfamily = rec_at(gen, sp, "subfamily"),
               tribe = rec_at(gen, sp, "tribe"), subtribe = "",
               genus = gen, subgenus = rec_at(gen, sp, "subgenus"), complex = rec_at(gen, sp, "complex"),
               species = sp, subspecies = fl$subspecies[i], common_name = "", stringsAsFactors = FALSE)
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
  new_rows  <- seed_additions_from_flags(rd(flags_path), record_df, additions)
  combined  <- if (nrow(new_rows)) dplyr::bind_rows(additions, new_rows) else additions
  if (!"taxon_id" %in% names(combined)) combined$taxon_id <- NA_integer_
  if (!sum(is.na(suppressWarnings(as.integer(combined$taxon_id))))) {
    if (verbose) message("  [taxon-id] every specimen addition already has an id -- nothing to resolve.")
    return(invisible(combined))
  }
  if (is.null(fetch_fn)) fetch_fn <- function(nm) inat_fetch_taxa_by_name(nm)
  res <- resolve_specimen_additions_interactive(combined, fetch_fn, prompt_fn, interactive_ok = TRUE, verbose = verbose)
  if (write) {
    dir.create(dirname(additions_path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(res$additions, additions_path, row.names = FALSE, na = "")
    if (verbose) message(sprintf("  [taxon-id] wrote %d specimen additions -> %s  (folds into the lookup on the next build)",
                                 nrow(res$additions), additions_path))
  }
  invisible(res$additions)
}
