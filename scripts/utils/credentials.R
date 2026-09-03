# =============================================================
# utils/credentials.R
# beescabr -- read an API credential that belongs to the PERSON running the pipeline.
#
# WHY THIS EXISTS: credentials used to be read silently from data/secrets/*.env, and a
# missing iNaturalist key just switched authentication OFF. The pull then returned
# OBSCURED coordinates for sensitive taxa (Bombus crotchii and the like) with no warning,
# so a run could quietly produce worse data than the operator thought. Worse, whoever
# inherited the folder inherited the previous person's keys.
#
# THE RULE: keys are personal. Nobody should run on somebody else's account. So a missing
# key is ASKED for, never assumed, and the answer is the operator's own.
#
# KEEPING IT SECRET, once typed:
#   * input is HIDDEN where the terminal allows it (askpass / getPass); if neither is
#     installed we say so plainly before echoing, rather than leaking it silently;
#   * the value is NEVER printed, logged, or echoed back -- confirmations show a mask;
#   * saving is optional and goes only to the gitignored data/secrets/*.env, written
#     owner-only (chmod 600). Nothing is ever written into a script;
#   * declining to save means the key lives only in that R session.
#
# Look-up order: environment variable -> secrets file -> ask.
# =============================================================

# cred_mask(): what a credential looks like when we have to mention it. PURE.
cred_mask <- function(x) {
  x <- as.character(x)
  if (!length(x) || is.na(x) || !nzchar(x)) return("(none)")
  if (nchar(x) <= 8) return("****")
  paste0("****", substr(x, nchar(x) - 3, nchar(x)))
}

# read one KEY=value line from an env-style file (quotes and spaces tolerated)
.cred_from_file <- function(key, file) {
  if (!file.exists(file)) return("")
  ln <- readLines(file, warn = FALSE)
  ln <- ln[grepl(paste0("^\\s*", key, "\\s*="), ln)]
  if (!length(ln)) return("")
  trimws(gsub('^["\']|["\']$', "", trimws(sub(paste0("^\\s*", key, "\\s*=\\s*"), "", ln[length(ln)]))))
}

# write/replace the key, then lock the file down to the owner
.cred_to_file <- function(key, value, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  ln <- if (file.exists(file)) readLines(file, warn = FALSE) else character(0)
  ln <- ln[!grepl(paste0("^\\s*", key, "\\s*="), ln)]        # replace, never duplicate
  writeLines(c(ln, paste0(key, "=", value)), file)
  try(Sys.chmod(file, "600"), silent = TRUE)                  # owner-only
  invisible(TRUE)
}

# hidden input where the terminal supports it; otherwise say so before echoing
.cred_ask <- function(msg) {
  if (requireNamespace("askpass", quietly = TRUE)) return(askpass::askpass(msg))
  if (requireNamespace("getPass", quietly = TRUE)) return(getPass::getPass(msg))
  message("  (typing will be VISIBLE: install the 'askpass' package to hide it)")
  readline(msg)
}

# cred_get(): the credential, or "" if the operator declines. Never throws.
# Injectable prompt_fn / confirm_fn / say / is_interactive so tests need no terminal.
cred_get <- function(key, file, ask = TRUE, what = key,
                     prompt_fn = .cred_ask, confirm_fn = readline,
                     say = message, is_interactive = interactive()) {
  v <- Sys.getenv(key)
  if (nzchar(v)) return(v)
  v <- .cred_from_file(key, file)
  if (nzchar(v)) return(v)
  if (!isTRUE(ask)) return("")
  if (!isTRUE(is_interactive)) {                 # a scheduled run must not block on input
    say("  ", key, " is not set. Set it in ", file, " or as an environment variable.")
    return("")
  }
  say("")
  say("  ", what, " is not set on this machine. (Enter to skip.)")
  v <- trimws(as.character(prompt_fn(paste0("  ", key, ": "))))
  if (!length(v) || is.na(v) || !nzchar(v)) { say("  Skipped. Continuing without ", key, "."); return("") }
  ans <- tolower(trimws(as.character(confirm_fn(paste0(
    "  Save it to ", file, " so you are not asked again? [y/N]: ")))))
  if (identical(ans, "y") || identical(ans, "yes")) {
    .cred_to_file(key, v, file)
    say("  Saved ", cred_mask(v), " to ", file, " (gitignored, readable only by you).")
  } else {
    say("  Not saved. ", cred_mask(v), " will be used for this session only.")
  }
  v
}
