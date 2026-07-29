# =============================================================
# reference/verify_prompt.R
# beescabr -- INTERACTIVE pass-2 VERIFICATION.
#
# Pass 1 (specimen_id_prompt.R) answers "which iNaturalist species is this?" (taxon_id).
# Pass 2 -- here -- answers "is that ID actually right for San Diego?": every taxon that is
# NEW to the Holway SD baseline (lookup `verified == FALSE`) is shown to the reviewer, who
# confirms it is a real ID / genuine new county record (not a misID). Each confirmed taxon_id
# is appended to verified_taxa.csv, so it stops being flagged on the next build.
#
# PURE core (prompt_fn injected) + a thin driver. Depends on: dplyr.
# =============================================================
suppressWarnings(suppressMessages(library(dplyr)))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# .verify_parse(): y/yes -> verify ; ""/n/no/skip/s -> skip ; x/stop -> stop ; else -> reask
.verify_parse <- function(ans) {
  a <- tolower(trimws(as.character(ans %||% "")))
  if (a %in% c("y", "yes"))                 return("verify")
  if (a %in% c("", "n", "no", "skip", "s")) return("skip")
  if (a %in% c("x", "stop", "halt"))        return("stop")
  "reask"
}

# unverified_rows(): PURE. lookup rows still needing verification = `verified` is falsey AND a
# real taxon_id present (no id yet -> that's pass 1's job, not this one).
unverified_rows <- function(lookup) {
  if (is.null(lookup) || !nrow(lookup) || !"verified" %in% names(lookup)) return(lookup[0, , drop = FALSE])
  is_ver <- tolower(trimws(as.character(lookup$verified))) %in% c("true", "t", "yes", "y", "1")
  tid <- suppressWarnings(as.integer(lookup$taxon_id))
  lookup[!is_ver & !is.na(tid), , drop = FALSE]
}

# resolve_verification_interactive(): walk each taxon; return the taxon_ids the reviewer
# confirmed + a stopped flag. `needs` = data.frame(taxon_id, scientific_name[, rank]).
resolve_verification_interactive <- function(needs, prompt_fn = readline, interactive_ok = TRUE, verbose = TRUE) {
  out <- list(verified_ids = integer(0), stopped = FALSE)
  if (!interactive_ok || is.null(needs) || !nrow(needs)) return(out)
  nm  <- if ("scientific_name" %in% names(needs)) as.character(needs$scientific_name) else rep("", nrow(needs))
  rk  <- if ("rank" %in% names(needs)) as.character(needs$rank) else rep("", nrow(needs))
  tid <- suppressWarnings(as.integer(needs$taxon_id))
  keep <- integer(0)
  if (verbose) message(sprintf("  [verify] %d bee taxa are new to the Holway SD baseline -- confirm each is a real ID (not a misID):",
                               sum(!is.na(tid))))
  for (i in seq_len(nrow(needs))) {
    if (is.na(tid[i])) next                          # no id yet -> pass 1's job
    repeat {
      msg <- sprintf("    %s (id %s%s).  Real ID / genuine SD record?  [y verify / Enter skip / x stop]: ",
                     nm[i], tid[i], if (nzchar(rk[i])) paste0(", ", rk[i]) else "")
      d <- .verify_parse(prompt_fn(msg))
      if (d == "stop")   { out$verified_ids <- unique(keep); out$stopped <- TRUE; return(out) }
      if (d == "skip")   break
      if (d == "verify") { keep <- c(keep, tid[i]); break }
      message("       (y = verify, Enter = skip, x = stop)")
    }
  }
  out$verified_ids <- unique(keep)
  out
}

# prompt_verify_taxa(): DRIVER. Prompt over the lookup's unverified rows and append every
# confirmed taxon_id to verified_taxa.csv (merged with what's already there, no duplicates).
prompt_verify_taxa <- function(lookup_df,
                               verified_path = if (exists("PATHS")) PATHS$verified_taxa else "data/reference/verified_taxa.csv",
                               prompt_fn = readline, interactive_ok = TRUE, write = TRUE, verbose = TRUE) {
  if (!interactive_ok) return(invisible(NULL))
  needs <- unverified_rows(lookup_df)
  if (!nrow(needs)) { if (verbose) message("  [verify] nothing new to verify."); return(invisible(NULL)) }
  res <- resolve_verification_interactive(needs, prompt_fn, interactive_ok, verbose)
  if (!length(res$verified_ids)) return(invisible(NULL))
  add <- data.frame(taxon_id = res$verified_ids,
                    scientific_name = as.character(needs$scientific_name)[match(res$verified_ids, suppressWarnings(as.integer(needs$taxon_id)))],
                    verified = "yes", stringsAsFactors = FALSE)
  existing <- if (file.exists(verified_path)) tryCatch(utils::read.csv(verified_path, stringsAsFactors = FALSE), error = function(e) NULL) else NULL
  merged <- if (!is.null(existing) && "taxon_id" %in% names(existing))
    dplyr::bind_rows(existing, add[!(add$taxon_id %in% suppressWarnings(as.integer(existing$taxon_id))), , drop = FALSE])
  else add
  if (write) {
    dir.create(dirname(verified_path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(merged, verified_path, row.names = FALSE, na = "")
    if (verbose) message(sprintf("  [verify] recorded %d verified taxa -> %s  (applies on the next build)",
                                 length(res$verified_ids), verified_path))
  }
  invisible(merged)
}
