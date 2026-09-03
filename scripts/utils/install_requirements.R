# =============================================================
# utils/install_requirements.R
# beescabr -- install everything the pipeline needs, once, on a new machine.
#
#   Rscript scripts/utils/install_requirements.R
#
# WHY THIS EXISTS: 23 scripts each installed their own packages and nothing listed the
# whole set, so a fresh machine discovered missing packages one failure at a time.
#
# WHERE THE PIECES LIVE, and why:
#   * the LIST is in config.R -- that is the constants file, and one list means one place
#     to change when a dependency is added;
#   * the INSTALLING is here -- config.R is sourced by every script, so it must not touch
#     the network at load. A setup step should be run deliberately, not as a side effect.
#
# The per-script auto-install blocks stay as a safety net and should rarely fire.
# =============================================================

if (!exists("BEESCABR_PACKAGES")) source("scripts/config.R")

CRAN <- "https://cloud.r-project.org"

# pkgs_missing(): PURE apart from the injected check, so it is testable offline.
pkgs_missing <- function(pkgs, have_fn = function(p) requireNamespace(p, quietly = TRUE)) {
  pkgs[!vapply(pkgs, have_fn, logical(1))]
}

# install_requirements(): install only what is absent, then REPORT what happened.
# Anything still missing afterwards is named rather than swallowed: a silent failure here
# turns into a confusing error much later, in a script that looks unrelated.
install_requirements <- function(pkgs = BEESCABR_PACKAGES,
                                 optional = BEESCABR_PACKAGES_OPTIONAL,
                                 have_fn = function(p) requireNamespace(p, quietly = TRUE),
                                 install_fn = function(p, ...) install.packages(p, repos = CRAN),
                                 say = message) {
  miss <- pkgs_missing(pkgs, have_fn)
  if (!length(miss)) say("  All ", length(pkgs), " required packages are already installed.")
  else {
    say("  Installing ", length(miss), " missing package(s): ", paste(miss, collapse = ", "))
    for (p in miss) try(install_fn(p), silent = TRUE)
  }
  failed <- pkgs_missing(miss, have_fn)          # still absent after the attempt
  if (length(failed)) {
    say("")
    say("  COULD NOT INSTALL: ", paste(failed, collapse = ", "))
    say("  Some of these need system libraries (sf and pdftools are the usual culprits).")
    say("  On macOS:  brew install gdal proj geos poppler")
  }
  opt_miss <- pkgs_missing(optional, have_fn)
  if (length(opt_miss)) {
    say("")
    say("  Optional, not installed: ", paste(opt_miss, collapse = ", "))
    say("  askpass/getPass hide an API key as you type it; ragg improves figure text.")
  }
  invisible(list(missing = miss, failed = failed, optional_missing = opt_miss))
}

if (sys.nframe() == 0) {
  message("Checking the ", length(BEESCABR_PACKAGES), " packages beescabr needs...")
  install_requirements()
}
