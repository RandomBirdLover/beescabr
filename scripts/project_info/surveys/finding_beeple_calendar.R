# =============================================================
# project_info/surveys/finding_beeple_calendar.R
# beescabr pipeline -- parse annual Cabrillo Bee Survey Calendar PDFs
# Ported from the retired parse_beeple_calendars.py (pdfplumber) to pure R
# using pdftools::pdf_data (positional tokens).
#
# Each PDF is a set of monthly tables: Date | [OT] | TP1 | TP2 | UPMON | BST.
# Each data row is a ~4-day survey window with a beeple FIRST NAME in each
# transect column. This writes one row per (window x transect x name):
#   year, window_start, window_end, first_name, transect
#
# Column layout is read from each month's header row (so the extra OT column
# that appears mid-season is handled automatically -- names are assigned to
# whichever transect header sits closest in x).
#
# In the pipeline as stage 2d (rebuilds the windows every run). Standalone:
#   source("scripts/project_info/surveys/finding_beeple_calendar.R"); finding_beeple_calendar()
# =============================================================

library(pdftools)
library(dplyr)
library(stringr)
library(purrr)

if (!exists("bx_kv") && file.exists("scripts/utils/console.R")) source("scripts/utils/console.R")

CALENDAR_DIR <- "data/project_info/surveys/survey_date_sources/beeple_calendar_windows"
WINDOWS_OUT  <- "data/project_info/surveys/survey_date_sources/beeple_calendar_windows/beeple_calendar_windows_generated.csv"

MONTHS <- c(January=1, February=2, March=3, April=4, May=5, June=6,
            July=7, August=8, September=9, October=10, November=11, December=12)
MONTH_ABBR <- setNames(MONTHS, substr(names(MONTHS), 1, 3))
TRANSECTS  <- c("OT", "TP1", "TP2", "UPMON", "BST")

.month_num <- function(name) {
  name <- trimws(name)
  out <- MONTHS[match(name, names(MONTHS))]
  if (is.na(out)) out <- MONTH_ABBR[match(substr(name, 1, 3), names(MONTH_ABBR))]
  as.integer(out)
}

# Parse "3-6", "Mar 30-2", "28-Oct 1" -> c(start_date, end_date) as Date.
# current_month/current_year come from the enclosing month table.
parse_date_range <- function(raw, current_month, current_year) {
  raw <- trimws(raw)
  mk <- function(y, m, d) tryCatch(as.Date(sprintf("%04d-%02d-%02d", y, m, d)),
                                   error = function(e) as.Date(NA))

  # "MonthAbbr D-D"  e.g. "Mar 30-2"
  m <- str_match(raw, "^([A-Za-z]+)\\s+(\\d+)-(\\d+)$")
  if (!is.na(m[1])) {
    sm <- .month_num(m[2]); if (is.na(sm)) return(c(NA, NA))
    sd <- as.integer(m[3]); ed <- as.integer(m[4])
    sy <- if (sm > current_month) current_year - 1 else current_year
    return(c(mk(sy, sm, sd), mk(current_year, current_month, ed)))
  }
  # "D-MonthAbbr D"  e.g. "28-Oct 1"
  m <- str_match(raw, "^(\\d+)-([A-Za-z]+)\\s+(\\d+)$")
  if (!is.na(m[1])) {
    em <- .month_num(m[3]); if (is.na(em)) return(c(NA, NA))
    sd <- as.integer(m[2]); ed <- as.integer(m[4])
    ey <- if (em < current_month) current_year + 1 else current_year
    return(c(mk(current_year, current_month, sd), mk(ey, em, ed)))
  }
  # Simple "D-D"
  m <- str_match(raw, "^(\\d+)-(\\d+)$")
  if (!is.na(m[1]))
    return(c(mk(current_year, current_month, as.integer(m[2])),
             mk(current_year, current_month, as.integer(m[3]))))
  c(NA, NA)
}

# Split a cell that may hold multiple names: "Patricia/Juliet" or "MarkJorge".
split_names <- function(cell) {
  cell <- str_replace_all(trimws(cell), "\\n", "")
  if (cell == "") return(character(0))
  if (str_detect(cell, "/")) return(str_trim(str_split(cell, "/")[[1]]) |> (\(x) x[x != ""])())
  parts <- str_extract_all(cell, "[A-Z][a-z]+")[[1]]
  if (length(parts) > 1) return(parts)
  cell
}

# Cluster a page's tokens into visual rows by y (tolerance in points).
.cluster_rows <- function(page, tol = 6) {
  page <- page[order(page$y, page$x), ]
  row_id <- integer(nrow(page)); cur <- 1L; row_id[1] <- 1L
  if (nrow(page) > 1) for (i in 2:nrow(page)) {
    if (page$y[i] - page$y[i - 1] > tol) cur <- cur + 1L
    row_id[i] <- cur
  }
  split(page, row_id)
}

parse_calendar <- function(pdf_path, year) {
  pages <- pdf_data(pdf_path)
  out <- list()
  current_month <- NA_integer_
  col_centers <- NULL   # named numeric: "DATE","OT","TP1",... -> x

  for (pg in pages) {
    for (row in .cluster_rows(pg)) {
      row <- row[order(row$x), ]
      texts <- trimws(row$text)
      # ---- month header row? (first token is a month name) ----
      first <- texts[1]
      if (!is.na(.month_num(first)) && first %in% names(MONTHS)) {
        current_month <- MONTHS[[first]]
        centers <- c(DATE = row$x[1])
        for (i in seq_along(texts)) {
          if (texts[i] %in% TRANSECTS) centers[texts[i]] <- row$x[i]
        }
        col_centers <- centers
        next
      }
      if (is.null(col_centers) || is.na(current_month)) next

      # ---- assign every token to its nearest column center ----
      assign_col <- names(col_centers)[max.col(-abs(outer(row$x, col_centers, "-")),
                                               ties.method = "first")]
      date_str <- paste(texts[assign_col == "DATE"], collapse = " ")
      if (!str_detect(date_str, "\\d")) next
      rng <- parse_date_range(date_str, current_month, year)
      if (is.na(rng[1])) next

      for (tr in setdiff(names(col_centers), "DATE")) {
        cell <- paste(texts[assign_col == tr], collapse = " ")
        for (nm in split_names(cell)) {
          out[[length(out) + 1]] <- tibble(
            year = year, window_start = as.Date(rng[1], origin = "1970-01-01"),
            window_end = as.Date(rng[2], origin = "1970-01-01"),
            first_name = nm, transect = tr
          )
        }
      }
    }
  }
  bind_rows(out)
}

finding_beeple_calendar <- function(dir = CALENDAR_DIR, out = WINDOWS_OUT, write = TRUE) {
  pdfs <- list.files(dir, pattern = "\\.pdf$", full.names = TRUE)
  per_year <- character(0)
  res <- map_dfr(pdfs, function(p) {
    m <- str_match(basename(p), "^(\\d{4})\\s+Cabrillo Bee Survey Calendar\\.pdf$")
    if (is.na(m[1])) { bx_note("skip (bad name): ", basename(p)); return(tibble()) }
    r <- parse_calendar(p, as.integer(m[2]))
    per_year <<- c(per_year, sprintf("%s: %d", m[2], nrow(r)))
    r
  }) |> arrange(year, window_start, transect, first_name) |> distinct()

  bx_kv("Calendars", format(nrow(res), big.mark = ","), " beeple survey windows")
  if (length(per_year)) bx_cont(paste(per_year, collapse = " · "))

  if (write) {
    dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
    write.csv(res, out, row.names = FALSE, na = "")
    bx_out(basename(out))
  }
  res
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) finding_beeple_calendar()
