# =============================================================
# reference/verify_prompt.R
# beescabr -- INTERACTIVE pass-2 VERIFICATION.
#
# Pass 1 (specimen_id_prompt.R) answers "which iNaturalist species is this?" (taxon_id).
# Pass 2 -- here -- answers "is that ID actually right for San Diego?": every taxon that is
# NEW to the Holway SD baseline (lookup `verified == FALSE`) is shown to the reviewer, who
#   * y  = VERIFY       -> real ID / genuine SD record; appended to verified_taxa.csv (trusted).
#   * r  = REJECT-FOR-NOW -> reviewed, not accepted (e.g. wrong county, casual junk). Remembered
#                     in rejected_taxa.csv, but NOT deleted from the data and NOT hidden forever:
#                     next run it is shown AGAIN, flagged "you rejected this before -- verified
#                     now?", so a record that later gets confirmed is never locked out. Verifying
#                     a previously-rejected taxon moves it out of the rejected list automatically.
#   * Enter = SKIP      -> undecided, no memory; asked again next run.
#   * x  = STOP.
#
# PURE core (prompt_fn injected) + a thin driver. Depends on: dplyr.
# =============================================================
suppressWarnings(suppressMessages(library(dplyr)))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# .verify_parse(): y -> verify ; r -> reject ; ""/n/no/skip/s -> skip ; x/stop -> stop ; else -> reask
.verify_parse <- function(ans) {
  a <- tolower(trimws(as.character(ans %||% "")))
  if (a %in% c("y", "yes"))                 return("verify")
  if (a %in% c("r", "reject"))              return("reject")
  if (a %in% c("", "n", "no", "skip", "s")) return("skip")
  if (a %in% c("x", "stop", "halt"))        return("stop")
  "reask"
}

# unverified_rows(): PURE. lookup rows still needing a decision = `verified` is falsey AND a
# real taxon_id present (no id yet -> that's pass 1's job, not this one).
unverified_rows <- function(lookup) {
  if (is.null(lookup) || !nrow(lookup) || !"verified" %in% names(lookup)) return(lookup[0, , drop = FALSE])
  is_ver <- tolower(trimws(as.character(lookup$verified))) %in% c("true", "t", "yes", "y", "1")
  tid <- suppressWarnings(as.integer(lookup$taxon_id))
  lookup[!is_ver & !is.na(tid), , drop = FALSE]
}

# resolve_verification_interactive(): walk each taxon; return the taxon_ids VERIFIED and REJECTED
# this pass, plus a stopped flag. Taxa whose id is in `prev_rejected` are shown with a "you
# rejected this before -- verified now?" reminder (memory), so nothing is ever locked out.
resolve_verification_interactive <- function(needs, prev_rejected = integer(0),
                                             prompt_fn = readline, interactive_ok = TRUE, verbose = TRUE) {
  out <- list(verified_ids = integer(0), rejected_ids = integer(0), stopped = FALSE)
  if (!interactive_ok || is.null(needs) || !nrow(needs)) return(out)
  nm  <- if ("scientific_name" %in% names(needs)) as.character(needs$scientific_name) else rep("", nrow(needs))
  rk  <- if ("rank" %in% names(needs)) as.character(needs$rank) else rep("", nrow(needs))
  tid <- suppressWarnings(as.integer(needs$taxon_id))
  prev_rejected <- suppressWarnings(as.integer(prev_rejected))
  keep <- integer(0); rej <- integer(0)
  if (verbose) message(sprintf("  [verify] %d bee taxa to review -- y verify / r reject-for-now / Enter skip:",
                               sum(!is.na(tid))))
  for (i in seq_len(nrow(needs))) {
    if (is.na(tid[i])) next                          # no id yet -> pass 1's job
    tag <- if (tid[i] %in% prev_rejected) " -- you REJECTED this before. Verified now?" else "."
    repeat {
      msg <- sprintf("    %s (id %s%s)%s  [y verify / r reject / Enter skip / x stop]: ",
                     nm[i], tid[i], if (nzchar(rk[i])) paste0(", ", rk[i]) else "", tag)
      d <- .verify_parse(prompt_fn(msg))
      if (d == "stop")   { out$verified_ids <- unique(keep); out$rejected_ids <- unique(rej); out$stopped <- TRUE; return(out) }
      if (d == "skip")   break
      if (d == "verify") { keep <- c(keep, tid[i]); break }
      if (d == "reject") { rej  <- c(rej,  tid[i]); break }
      message("       (y = verify, r = reject-for-now, Enter = skip, x = stop)")
    }
  }
  out$verified_ids <- unique(keep); out$rejected_ids <- unique(rej)
  out
}

# read taxon_ids recorded in a decision file (verified or rejected); [] if none.
.pv_read_ids <- function(path) {
  if (is.null(path) || !file.exists(path)) return(integer(0))
  v <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(v) || !"taxon_id" %in% names(v) || !nrow(v)) return(integer(0))
  unique(suppressWarnings(as.integer(v$taxon_id)))
}

# Coerce every column to character before bind_rows. read.csv of a HEADER-ONLY file
# (e.g. right after a reset) infers all columns as logical(0), and
# bind_rows(<logical>, <character>) THROWS -- which silently lost a whole pass-2
# session (the run_pipeline tryCatch swallowed the error). Coercing sidesteps it.
.pv_chr <- function(d) { if (ncol(d)) d[] <- lapply(d, as.character); d }

# append (taxon_id, scientific_name, <col>=yes) rows to a decision file, keeping existing.
.pv_write <- function(path, ids, needs, statuscol) {
  if (!length(ids)) return(invisible())
  add <- data.frame(taxon_id = ids,
                    scientific_name = as.character(needs$scientific_name)[match(ids, suppressWarnings(as.integer(needs$taxon_id)))],
                    stringsAsFactors = FALSE)
  add[[statuscol]] <- "yes"
  existing <- if (file.exists(path)) tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL) else NULL
  keep_add <- add[!(add$taxon_id %in% suppressWarnings(as.integer(existing$taxon_id))), , drop = FALSE]
  merged <- if (!is.null(existing) && "taxon_id" %in% names(existing) && nrow(existing))
    dplyr::bind_rows(.pv_chr(existing), .pv_chr(keep_add))   # coerce: an empty/header-only file has logical cols
  else add
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(merged, path, row.names = FALSE, na = "")
}

# update rejected_taxa.csv: ADD newly-rejected, REMOVE any that were just verified (un-reject).
.pv_update_rejected <- function(path, add_ids, remove_ids, needs) {
  existing <- if (file.exists(path)) tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL) else NULL
  if (is.null(existing) || !"taxon_id" %in% names(existing))
    existing <- data.frame(taxon_id = integer(0), scientific_name = character(0), rejected = character(0), stringsAsFactors = FALSE)
  existing <- existing[!(suppressWarnings(as.integer(existing$taxon_id)) %in% remove_ids), , drop = FALSE]   # un-reject the now-verified
  to_add <- setdiff(add_ids, suppressWarnings(as.integer(existing$taxon_id)))
  if (length(to_add)) {
    addrows <- data.frame(taxon_id = to_add,
                          scientific_name = as.character(needs$scientific_name)[match(to_add, suppressWarnings(as.integer(needs$taxon_id)))],
                          rejected = "yes", stringsAsFactors = FALSE)
    existing <- dplyr::bind_rows(.pv_chr(existing), .pv_chr(addrows))   # coerce: header-only file has logical cols
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(existing, path, row.names = FALSE, na = "")
}

# .pv_fill_names(): PURE. Give a readable name to rows whose `scientific_name` is blank -- higher
# ranks store their name elsewhere: complex -> the `complex` column (minus its "(Complex) "
# prefix), subgenus -> "Genus (Subgenus)", genus -> `genus`. Species keep their scientific_name.
.pv_fill_names <- function(needs) {
  col <- function(n) if (n %in% names(needs)) as.character(needs[[n]]) else rep("", nrow(needs))
  sn <- trimws(col("scientific_name")); rk <- tolower(trimws(col("rank")))
  g  <- trimws(col("genus")); sg <- trimws(col("subgenus")); sp <- trimws(col("species"))
  cn <- trimws(col("common_name"))
  cx <- trimws(sub("^\\(Complex\\)\\s*", "", col("complex"), ignore.case = TRUE))
  out <- sn
  for (i in seq_along(out)) {
    if (nzchar(out[i])) next
    out[i] <-
      if      (rk[i] == "complex"  && nzchar(cx[i]))                 cx[i]
      else if (rk[i] == "subgenus" && nzchar(g[i]) && nzchar(sg[i])) paste0(g[i], " (", sg[i], ")")
      else if (nzchar(g[i]) && nzchar(sp[i]))                        paste(g[i], sp[i])
      else if (nzchar(g[i]))                                         g[i]
      else if (nzchar(cn[i]))                                        cn[i]
      else                                                          paste0("taxon ", needs$taxon_id[i])
  }
  out
}

# prompt_verify_taxa(): DRIVER. Prompt over ALL unverified rows (previously-rejected included, so
# a later-confirmed record is never locked out -- it is re-shown with a "rejected before" note).
# Verified ids -> verified_taxa.csv; rejected ids -> rejected_taxa.csv; a verify un-rejects.
prompt_verify_taxa <- function(lookup_df,
                               verified_path = if (exists("PATHS")) PATHS$verified_taxa else "data/reference/verified_taxa.csv",
                               rejected_path = if (exists("PATHS") && !is.null(PATHS$rejected_taxa)) PATHS$rejected_taxa else "data/reference/rejected_taxa.csv",
                               prompt_fn = readline, interactive_ok = TRUE, write = TRUE, verbose = TRUE) {
  if (!interactive_ok) return(invisible(NULL))
  needs <- unverified_rows(lookup_df)
  if (!nrow(needs)) { if (verbose) message("  [verify] nothing new to verify."); return(invisible(NULL)) }
  needs$scientific_name <- .pv_fill_names(needs)   # higher ranks (complex/subgenus/genus) show a readable name
  prev_rejected <- .pv_read_ids(rejected_path)
  res <- resolve_verification_interactive(needs, prev_rejected, prompt_fn, interactive_ok, verbose)
  if (write) {
    saved <- tryCatch({
      .pv_write(verified_path, res$verified_ids, needs, "verified")
      if (length(res$verified_ids) || length(res$rejected_ids))
        .pv_update_rejected(rejected_path, add_ids = res$rejected_ids, remove_ids = res$verified_ids, needs = needs)
      TRUE
    }, error = function(e) {   # never let a save failure be silent again
      message("\n  !!! [verify] COULD NOT SAVE -- ", conditionMessage(e))
      message("  !!! Your ", length(res$verified_ids), " verify + ", length(res$rejected_ids),
              " reject answers were NOT written to ", basename(verified_path),
              ". Re-run and report this message.\n")
      FALSE
    })
    if (saved && verbose) message(sprintf("  [verify] %d verified, %d reject-for-now. Rejected taxa are re-shown next run (memory) -- delete a row from %s to forget it.",
                                 length(res$verified_ids), length(res$rejected_ids), basename(rejected_path)))
  }
  invisible(res)
}
