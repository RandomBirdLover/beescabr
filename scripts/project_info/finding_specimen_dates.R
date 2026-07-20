# =============================================================
# project_info/finding_specimen_dates.R
# beescabr -- aggregate the lethal-net specimen record IN MEMORY for the brain.
# Created 2026-07-18.
#
# Reads the NEWEST specimen record .xlsx (data/specimens/records/, same file
# specimen_bee_clean.R uses) and RETURNS a per-date table:  date, n_specimens, collectors
# n_specimens counts EVERY specimen row for that date (missing_specimen is IGNORED, not
# subtracted -- per Brandi). collectors = the collector(s) that day, most frequent first.
#
# NO file is written. The brain (finding_survey_dates.R) calls finding_specimen_dates()
# directly to stamp n_speci on lethal survey days + add an intern row for any netting day
# with no intern-log entry. Re-parses the xlsx on every brain run (small; ~1.4k rows).
#
#   source("scripts/project_info/finding_specimen_dates.R"); finding_specimen_dates()
# =============================================================
suppressWarnings(suppressMessages({library(readxl); library(dplyr); library(tibble)}))
if (!exists("read_latest") && file.exists("scripts/utils/utils.R")) {
  suppressWarnings(suppressMessages(library(stringr))); source("scripts/utils/utils.R")
}

FSD_DIR     <- "data/specimens/records"
FSD_PATTERN <- "^cabr_bee_specimens_record_V"

# Returns tibble(date, n_specimens, collectors); an empty tibble if no usable .xlsx.
finding_specimen_dates <- function(dir = FSD_DIR, pattern = FSD_PATTERN) {
  empty <- tibble(date = as.Date(character()), n_specimens = integer(), collectors = character())
  path <- tryCatch(read_latest(dir, pattern), error = function(e) NA_character_)
  if (is.na(path) || !file.exists(path)) {
    message("  (no specimen .xlsx in ", dir, " -- no specimen counts)"); return(empty)
  }
  message("finding_specimen_dates: reading ", basename(path))
  raw <- readxl::read_excel(path)
  names(raw) <- tolower(trimws(names(raw)))
  if (!all(c("date", "collector") %in% names(raw))) {
    message("  (specimen xlsx missing date/collector columns -- skipped)"); return(empty)
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
  message(sprintf("  %d specimen dates, %d specimens (missing included)", nrow(agg), sum(agg$n_specimens)))
  agg
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) print(finding_specimen_dates())
