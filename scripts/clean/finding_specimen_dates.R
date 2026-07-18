# =============================================================
# clean/finding_specimen_dates.R
# beescabr -- build the per-date lethal-net specimen table the brain reads.
# Created 2026-07-18.
#
# Reads the NEWEST specimen record .xlsx (data/cabr_bee_specimens_records/, same file
# specimen_bee_clean.R uses) and aggregates it to data/project_info/inputs/specimen_dates.csv
#   date, n_specimens, collectors
# n_specimens counts EVERY specimen row for that date (missing_specimen is IGNORED, not
# subtracted -- per Brandi). collectors = the collector(s) that day, most frequent first.
#
# The brain (finding_survey_dates.R) reads specimen_dates.csv to (a) stamp n_speci on lethal
# survey days and (b) add an intern lethal row for any specimen date with no intern-log entry.
# Run this whenever a new specimen .xlsx version lands; run_pipeline.R runs it before the brain.
#
#   source("scripts/clean/finding_specimen_dates.R"); finding_specimen_dates()
# =============================================================
suppressWarnings(suppressMessages({library(readxl); library(dplyr); library(tibble)}))
if (!exists("read_latest") && file.exists("scripts/utils/utils.R")) {
  suppressWarnings(suppressMessages(library(stringr))); source("scripts/utils/utils.R")
}

FSD_DIR     <- "data/cabr_bee_specimens_records"
FSD_PATTERN <- "^cabr_bee_specimens_record_V"
FSD_OUT     <- "data/project_info/inputs/specimen_dates.csv"

finding_specimen_dates <- function(dir = FSD_DIR, pattern = FSD_PATTERN, out_path = FSD_OUT) {
  path <- tryCatch(read_latest(dir, pattern), error = function(e) NA_character_)
  if (is.na(path) || !file.exists(path)) {
    message("  (no specimen .xlsx in ", dir, " -- specimen_dates.csv left as-is)"); return(invisible(NULL))
  }
  message("finding_specimen_dates: reading ", basename(path))
  raw <- readxl::read_excel(path)
  names(raw) <- tolower(trimws(names(raw)))
  if (!all(c("date", "collector") %in% names(raw))) {
    message("  (specimen xlsx missing date/collector columns -- skipped)"); return(invisible(NULL))
  }
  cnt <- if ("count" %in% names(raw)) suppressWarnings(as.integer(raw[["count"]])) else rep(1L, nrow(raw))
  df <- tibble(date      = as.Date(raw[["date"]]),
               n         = dplyr::coalesce(cnt, 1L),
               collector = trimws(as.character(raw[["collector"]]))) |>
    filter(!is.na(date))

  agg <- df |>
    group_by(date) |>
    summarise(
      n_specimens = sum(n),
      collectors  = { t <- sort(table(collector[nzchar(collector) & !is.na(collector)]), decreasing = TRUE)
                      paste(names(t), collapse = "; ") },
      .groups = "drop") |>
    arrange(date)

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(agg, out_path, row.names = FALSE, na = "")
  message(sprintf("  %d dates, %d specimens (missing included) -> %s",
                  nrow(agg), sum(agg$n_specimens), out_path))
  invisible(agg)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) finding_specimen_dates()
