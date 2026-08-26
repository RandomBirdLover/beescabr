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
             blurb = "BEES + PLANTS: pull only what is new or edited since last time (seconds)",
             flags = c(BEESCABR_SKIP_INGEST = "0", BEESCABR_FULL_INGEST = "0")),
  "2" = list(label = "Bees only",
             blurb = "BEES ONLY: normal bee pull, plants SKIPPED (leaves plant data stale)",
             flags = c(BEESCABR_SKIP_INGEST = "0", BEESCABR_FULL_INGEST = "0",
                       BEESCABR_SKIP_PLANTS = "1")),
  "3" = list(label = "Offline run",
             blurb = "BEES + PLANTS: no iNaturalist calls at all, reuse what is cached",
             flags = c(BEESCABR_SKIP_INGEST = "1", BEESCABR_FULL_INGEST = "0")),
  "4" = list(label = "Full rebuild",
             blurb = "BEES + PLANTS: re-download everything from scratch (~40+ min, rare)",
             flags = c(BEESCABR_SKIP_INGEST = "0", BEESCABR_FULL_INGEST = "1"),
             # A rebuild re-resolves every name against iNaturalist's CURRENT taxonomy,
             # so taxa that were settled can come back for judgement. Deciding whether a
             # name is a genuine synonym/rename or a bad ID is not a mechanical call.
             warn = c(
               "This needs BEE EXPERTISE, not just patience.",
               "A rebuild re-resolves every name against iNaturalist's taxonomy as it",
               "stands TODAY. Names drift: a bee may have been renamed, split, lumped, or",
               "may simply not exist on iNaturalist under the name in our checklists or on",
               "a specimen label. You will be asked to judge those cases, and answering",
               "wrongly writes a wrong name into the checklists.",
               "If you are unsure, stop and ask someone who knows the bees before running."))
)
INGEST_MODE_DEFAULT <- "1"
INGEST_MODE_TRIES   <- 3    # bad answers before falling back to the default (never loop forever)
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
  say("  (a normal run covers BOTH bees and plants)")
  say("")
  for (k in names(INGEST_MODES))
    say(sprintf("    %s. %-14s %s", k, INGEST_MODES[[k]]$label, INGEST_MODES[[k]]$blurb))
  say("")

  # Bounded, never infinite: a console that keeps returning nonsense (or a caller
  # feeding a fixed bad answer) must not hang the pipeline -- fall back to the default.
  ans <- INGEST_MODE_DEFAULT
  for (attempt in seq_len(INGEST_MODE_TRIES)) {
    a <- trimws(read_fn(sprintf("  Choose 1-%d [%s]: ", length(INGEST_MODES), INGEST_MODE_DEFAULT)))
    if (!nzchar(a)) { ans <- INGEST_MODE_DEFAULT; break }
    if (!is.null(INGEST_MODES[[a]])) { ans <- a; break }
    if (attempt == INGEST_MODE_TRIES) {
      say("  Still not a valid choice -- using ", INGEST_MODE_DEFAULT, " (", INGEST_MODES[[INGEST_MODE_DEFAULT]]$label, ").")
    } else {
      say("  Not one of the choices. Type 1-", length(INGEST_MODES),
          " (or press Enter for ", INGEST_MODE_DEFAULT, ").")
    }
  }
  mode <- INGEST_MODES[[ans]]
  say("  → ", mode$label, ": ", mode$blurb)
  if (!is.null(mode$warn)) {
    say("")
    for (w in mode$warn) say("     ! ", w)
    say("")
  }

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
