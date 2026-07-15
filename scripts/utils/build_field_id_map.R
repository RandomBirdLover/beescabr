# =============================================================
# utils/build_field_id_map.R
# One-time helper: the flattened export names obs-fields by TEXT name and drops
# the numeric iNat field_id. This pulls the name -> field_id map back out of the
# raw observation cache so the crosswalk can carry real IDs.
#
# Writes data/project_info/inat_field_id_map.csv  (field_name, inat_field_id).
# Run in RStudio (needs the DuckDB cache):
#   source("scripts/utils/build_field_id_map.R")
# =============================================================

local({
  need <- function(sym, file) if (!exists(sym)) source(file.path("scripts", file))
  need("store_connect",         "engine/db/store_conn.R")
  need("read_observations_raw", "engine/db/observations_store.R")
})
suppressMessages({library(DBI); library(dplyr); library(readr)})

build_field_id_map <- function(out = "data/project_info/inat_field_id_map.csv") {
  con <- store_connect(); on.exit(store_disconnect(con), add = TRUE)

  # Fast path: unnest the ofvs array in DuckDB and collect distinct (name, id).
  q <- "
    SELECT j->>'$.name' AS field_name,
           string_agg(DISTINCT j->>'$.field_id', '; ') AS inat_field_id
    FROM inat_observations,
         UNNEST(CAST(CAST(raw_data AS JSON)->'$.ofvs' AS JSON[])) AS t(j)
    WHERE j->>'$.field_id' IS NOT NULL AND j->>'$.name' IS NOT NULL
    GROUP BY j->>'$.name'
  "
  map <- tryCatch(DBI::dbGetQuery(con, q), error = function(e) {
    message("DuckDB JSON path failed (", conditionMessage(e), ") -- falling back to R parse.")
    NULL
  })

  if (is.null(map)) {           # reliable (slower) fallback in R
    suppressMessages(library(jsonlite))
    raw <- read_observations_raw(con)
    seen <- new.env()
    for (rd in raw$raw_data) {
      o <- tryCatch(fromJSON(rd, simplifyVector = FALSE), error = function(e) NULL)
      for (ofv in o$ofvs %||% list()) {
        if (!is.list(ofv) || is.null(ofv$field_id) || is.null(ofv$name)) next
        k <- as.character(ofv$name)
        seen[[k]] <- unique(c(get0(k, envir = seen, ifnotfound = NULL), as.character(ofv$field_id)))
      }
    }
    map <- tibble(field_name = ls(seen),
                  inat_field_id = vapply(ls(seen),
                    function(k) paste(sort(seen[[k]]), collapse = "; "), character(1)))
  }

  map <- map |> filter(!is.na(field_name), field_name != "") |> arrange(field_name)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  write.csv(map, out, row.names = FALSE, na = "")
  message("Wrote ", nrow(map), " field name -> id rows -> ", out)
  invisible(map)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) build_field_id_map()
