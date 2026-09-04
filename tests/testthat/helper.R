# Shared test helpers: locate and source pipeline modules regardless of the
# working directory testthat runs from.

.beescabr_root <- function() {
  env <- Sys.getenv("BEESCABR_ROOT", unset = NA)
  if (!is.na(env) && dir.exists(file.path(env, "scripts"))) return(env)
  for (cand in c(".", "..", "../..", "../../..")) {
    if (dir.exists(file.path(cand, "scripts"))) return(normalizePath(cand))
  }
  stop("cannot locate beescabr root (scripts/ dir)")
}

# Every pipeline module sources its dependencies with repo-root-relative paths
# (e.g. source("scripts/config.R")), because the pipeline is always run from the
# repo root. testthat runs from tests/testthat/, so we source WITH the working
# directory temporarily set to the root -- otherwise a module that pulls in a
# sibling fails with "cannot open the connection".
# If the exact path is gone, fall back to finding the file by NAME anywhere under
# scripts/. analysis/ is foldered by topic, and a test should not have to be
# edited because a script moved between topics -- the test cares which file it is
# testing, not which folder that file sits in. An ambiguous name still errors,
# because two files with one name is a problem worth stopping on.
src <- function(rel) {
  root <- .beescabr_root()
  old <- setwd(root); on.exit(setwd(old), add = TRUE)
  p <- file.path("scripts", rel)
  if (!file.exists(p)) {
    hits <- list.files("scripts", pattern = paste0("^", basename(rel), "$"),
                       recursive = TRUE, full.names = TRUE)
    if (length(hits) == 1) p <- hits
    else if (length(hits) > 1)
      stop("src(): '", basename(rel), "' matches ", length(hits), " files: ",
           paste(hits, collapse = ", "), call. = FALSE)
  }
  source(p)
}

fx <- function(name) file.path(.beescabr_root(), "tests", "testthat", "fixtures", name)

# TRUE when the duckdb R package is available (DB-backed tests run only then).
have_duckdb <- function() requireNamespace("duckdb", quietly = TRUE)

# console reporter (bx_phase / bx_kv / bx_cont / bx_out / bx_note / bx_need_*) so stage
# scripts that print through it can be sourced in tests without the pipeline present.
if (!exists("bx_kv")) source(file.path(.beescabr_root(), "scripts", "utils", "console.R"))
