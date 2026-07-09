# =============================================================
# Shared Utilities
# Created: June 21, 2026
# Author: Brandi Sanchez
# Description: Helper functions shared across the beescabr pipeline.
#              Source this at the top of any script that needs them:
#                source("scripts/utils/utils.R")
# =============================================================

library(stringr)

# ------------------------------------------------------------
# read_latest()
# Auto-detects the newest dated file in a folder, based on a
# YYYY-MM-DD pattern in the filename. Used everywhere instead of
# hardcoding filenames, so the pipeline always reads the current
# export/specimen version without manual path edits.
# ------------------------------------------------------------
read_latest <- function(folder, pattern) {
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop("No files matching '", pattern, "' found in ", folder)
  }
  dates <- as.Date(str_extract(basename(files), "\\d{4}-\\d{2}-\\d{2}"))
  files[which.max(dates)]
}

# ------------------------------------------------------------
# require_columns()
# Fails loudly with a clear, actionable message if expected
# columns are missing from a data frame — instead of letting a
# script break silently several steps later with a cryptic error.
# ------------------------------------------------------------
require_columns <- function(df, required, df_name = "data") {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      "'", df_name, "' is missing expected column(s): ", paste(missing, collapse = ", "),
      ". Run names(", df_name, ") to see actual columns and update this script."
    )
  }
}

# ------------------------------------------------------------
# write_fresh()
# Always overwrite on re-run. Clears whatever is sitting at `path`
# first -- an old file OR a stray folder that would block the write
# (write.csv cannot write a file over a folder) -- then writes fresh.
# Use in place of write.csv() for every output so re-running a script
# reliably replaces its old outputs.
# ------------------------------------------------------------
write_fresh <- function(x, path, row.names = FALSE, ...) {
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  else if (file.exists(path)) unlink(path, force = TRUE)
  write.csv(x, path, row.names = row.names, ...)
}
