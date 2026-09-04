# =============================================================
# utils/utils.R
# beescabr -- tiny shared helpers, sourced first by the pipeline runners
# =============================================================

library(stringr)

# Ensure pdftools is installed (stage 2d reads the beeple-calendar PDFs with it).
# utils.R is sourced first, so this runs before the calendar parser loads pdftools,
# keeping the pipeline from halting at load time on a fresh machine. try() so an
# offline install just warns and the calendar stage's own tryCatch skips gracefully.
# Dependencies are CHECKED here, not installed: see beescabr_require() in config.R.
if (!exists("beescabr_require")) source("scripts/config.R")
beescabr_require()

#' Read the most recent file matching a pattern
#'
#' @param folder Directory to look in.
#' @param pattern Regular expression the filename must match.
#' @return The parsed contents of the newest match. Stops when nothing matches,
#'   because a silent empty result would look like an empty dataset.
read_latest <- function(folder, pattern) {
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files matching '", pattern, "' found in ", folder)
  ds    <- str_extract(basename(files), "\\d{4}[-_]\\d{2}[-_]\\d{2}")  # accept _ or -
  dates <- as.Date(gsub("_", "-", ds))
  ver   <- suppressWarnings(as.integer(gsub("[^0-9]", "", str_extract(basename(files), "[Vv][0-9]+"))))  # version number if present
  ver[is.na(ver)] <- 0L
  files[order(dates, ver, decreasing = TRUE)][1]                        # newest date, then highest version number
}
#' Stop unless a data frame has the columns that follow it
#'
#' @param df The data frame to check.
#' @param required Column names that must be present.
#' @param df_name Name to use in the error, so the message says which frame.
#' @return Invisibly, nothing. Stops listing exactly what is missing.
require_columns <- function(df, required, df_name = "data") {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) stop("'", df_name, "' is missing expected column(s): ", paste(missing, collapse = ", "))
}
#' Write a CSV over anything already at the path
#'
#' Removes a stale file OR directory first, so a leftover folder from an earlier
#' layout cannot make the write fail.
#'
#' @param x The data frame to write.
#' @param path Output path.
#' @param row.names Passed to `write.csv()`; off by default.
#' @param ... Further arguments for `write.csv()`.
#' @return Invisibly, nothing.
write_fresh <- function(x, path, row.names = FALSE, ...) {
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  else if (file.exists(path)) unlink(path, force = TRUE)
  write.csv(x, path, row.names = row.names, ...)
}

# decorate_complex(): prefix "(Complex) " onto species-complex names for display,
# so a complex ("Bombus fervidus") isn't mistaken for the species of the same
# name. Idempotent -- won't double-prefix. Applied at OUTPUT time only, so
# internal joins/matching still use the bare complex name.
decorate_complex <- function(df) {
  if (!"complex" %in% names(df)) return(df)
  cx  <- as.character(df$complex)
  hit <- !is.na(cx) & cx != "" & !startsWith(cx, "(Complex)")
  cx[hit] <- paste0("(Complex) ", cx[hit])
  df$complex <- cx
  df
}

# decorate_complex_name(): mirror the "(Complex) <name>" tag into scientific_name for
# rank == "complex" ROWS, so a complex reads distinctly and never looks like a duplicate of
# its member species of the same name (e.g. the Diadasia australis complex vs the species).
# Keyed on rank == "complex" -- NOT on a populated `complex` column, since species rows also
# carry a parent complex and must keep their binomial. A blank complex scientific_name is
# filled from the `complex` column. Idempotent. OUTPUT-time only, like decorate_complex():
# internal joins/matching still use the bare name, so consumers that PARSE scientific_name
# into a binomial (not_on_holway, inat_misid_qc) strip this tag first.
decorate_complex_name <- function(df) {
  if (!all(c("rank", "complex", "scientific_name") %in% names(df))) return(df)
  is_cx  <- !is.na(df$rank) & df$rank == "complex"
  bare   <- sub("^\\s*\\(Complex\\)\\s*", "", ifelse(is.na(df$complex), "", as.character(df$complex)))
  tagged <- ifelse(bare == "", as.character(df$scientific_name), paste0("(Complex) ", bare))
  df$scientific_name[is_cx] <- tagged[is_cx]
  df
}
