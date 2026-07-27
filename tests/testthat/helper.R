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

src <- function(rel) source(file.path(.beescabr_root(), "scripts", rel))

fx <- function(name) file.path(.beescabr_root(), "tests", "testthat", "fixtures", name)

# TRUE when the duckdb R package is available (DB-backed tests run only then).
have_duckdb <- function() requireNamespace("duckdb", quietly = TRUE)

# console reporter (bx_phase / bx_kv / bx_cont / bx_out / bx_note / bx_need_*) so stage
# scripts that print through it can be sourced in tests without the pipeline present.
if (!exists("bx_kv")) source(file.path(.beescabr_root(), "scripts", "utils", "console.R"))
