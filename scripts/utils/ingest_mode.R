# =============================================================
# utils/ingest_mode.R
# beescabr -- the run-mode menu for the data cleaning pipeline.
#
# WHY: the pipeline's behaviour is controlled by env-var flags (BEESCABR_SKIP_INGEST,
# BEESCABR_FULL_INGEST, ...). They are precise but nobody remembers the names, and a
# flag set in one R session STAYS set -- a leftover BEESCABR_FULL_INGEST=1 silently
# turns every later run into a 40-minute rebuild. So an interactive run now ASKS, in
# plain language, and sets EVERY flag explicitly from the answer. That makes the choice
# visible each run and makes a stuck flag impossible.
#
# Scripted runs are untouched: if the session is non-interactive, or the operator has
# deliberately set a flag beforehand, the menu is skipped entirely.
#
# Pure + injectable (read_fn / is_interactive / preset) so it is unit-tested offline
# in tests/testthat/test-ingest-mode.R.
# =============================================================

INGEST_MODES <- list(
  "1" = list(label = "Normal run",
             blurb = "pull only NEW or edited observations since last time (seconds)",
             flags = c(BEESCABR_SKIP_INGEST = "0", BEESCABR_FULL_INGEST = "0")),
  "2" = list(label = "Offline run",
             blurb = "do not contact iNaturalist at all; reuse what is already cached",
             flags = c(BEESCABR_SKIP_INGEST = "1", BEESCABR_FULL_INGEST = "0")),
  "3" = list(label = "Full rebuild",
             blurb = "re-download EVERY observation from scratch (~40+ min; rarely needed)",
             flags = c(BEESCABR_SKIP_INGEST = "0", BEESCABR_FULL_INGEST = "1"))
)
INGEST_MODE_DEFAULT <- "1"
# Every flag the menu owns. All of them are written on every answer, so a value left
# over from an earlier session in the same R process cannot leak into this run.
INGEST_MODE_FLAGS <- c("BEESCABR_SKIP_INGEST", "BEESCABR_FULL_INGEST",
                       "BEESCABR_SKIP_PLANTS", "BEESCABR_REFRESH")

# Ask which kind of run this is; return a named character vector of flag values to
# apply, or NULL when the menu should not run at all (scripted, or flags preset).
ingest_mode_flags <- function(read_fn = function(prompt) readline(prompt),
                              is_interactive = interactive(),
                              preset = Sys.getenv(INGEST_MODE_FLAGS),
                              say = message) {
  if (!is_interactive) return(NULL)
  preset <- preset[nzchar(preset) & preset != "0"]
  if (length(preset)) {                       # operator was explicit -- respect it
    say("  note: run mode taken from flags you set: ",
        paste(names(preset), unname(preset), sep = "=", collapse = ", "))
    return(NULL)
  }
  say("")
  say("  How should this run pull iNaturalist data?")
  say("")
  for (k in names(INGEST_MODES))
    say(sprintf("    %s. %-14s %s", k, INGEST_MODES[[k]]$label, INGEST_MODES[[k]]$blurb))
  say("")

  repeat {
    ans <- trimws(read_fn(sprintf("  Choose 1-%d [%s]: ", length(INGEST_MODES), INGEST_MODE_DEFAULT)))
    if (!nzchar(ans)) ans <- INGEST_MODE_DEFAULT
    if (!is.null(INGEST_MODES[[ans]])) break
    say("  Not one of the choices. Type 1, 2 or 3 (or press Enter for ", INGEST_MODE_DEFAULT, ").")
  }
  mode <- INGEST_MODES[[ans]]
  say("  → ", mode$label, ": ", mode$blurb)

  out <- setNames(rep("0", length(INGEST_MODE_FLAGS)), INGEST_MODE_FLAGS)
  out[names(mode$flags)] <- mode$flags
  out
}

# Apply what the menu returned (no-op when NULL).
ingest_mode_apply <- function(flags) {
  if (is.null(flags)) return(invisible(FALSE))
  do.call(Sys.setenv, as.list(flags))
  invisible(TRUE)
}
