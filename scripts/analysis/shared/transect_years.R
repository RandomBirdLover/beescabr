# =============================================================
# analysis/shared/transect_years.R
# beescabr -- which transects existed in a given year.
#
# A published report is a claim about a season, and the transects have changed:
# OT was first surveyed in 2024. Draw a 2023 report map from today's shapefile and
# it shows four transects, making it look as though nobody walked OT that year,
# when there was nothing to walk. Worse, the map on an already-published 2023
# report would silently change the next time the pipeline runs.
#
# So there are two maps:
#   data/spatial/ ................ the OVERALL map. What exists now. Redrawn freely.
#   nps_report_YYYY_generated/ ... that season's transects only. Frozen with the report.
#
# The years are DECLARED in data/spatial/shapefiles/transects/transect_years_manual.csv,
# never inferred from the survey record. "First surveyed" is not "first established":
# a transect cut in November with no surveys until spring would be dated a year late.
# =============================================================

TRANSECT_YEARS_FILE <- "data/spatial/shapefiles/transects/transect_years_manual.csv"

read_transect_years <- function(path = TRANSECT_YEARS_FILE) {
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

# transects_in_year(): PURE. Codes active in `year`, sorted. NULL year = all of them
# (the overall map). A transect with a blank last_year is still in use.
transects_in_year <- function(year, years) {
  if (is.null(years) || !nrow(years) || !"transect" %in% names(years)) return(character(0))
  code <- toupper(trimws(as.character(years$transect)))
  if (is.null(year)) return(sort(unique(code)))
  y  <- as.integer(year)
  fy <- suppressWarnings(as.integer(years$first_year))
  ly <- suppressWarnings(as.integer(years$last_year))
  keep <- (is.na(fy) | fy <= y) & (is.na(ly) | ly >= y)   # blank at either end = open-ended
  sort(unique(code[keep]))
}
