# =============================================================
# project_info/rosters/build_identification_counts.R
# beescabr -- how many identifications each person made ON CABRILLO RECORDS.
#   the two observation caches + cabr_inat_raw_generated.csv (in_cabr)
#     -> data/project_info/rosters/identification_counts_generated.csv
#
# GENERATED. This replaced a hand-typed `id_count` column in people_manual.csv: a
# number that was true the day it was entered and drifted every time anybody
# identified a bee. It orders the identifier list on the acknowledgements page, and
# the survey team beside it is already ranked by a counted figure, so this makes the
# two consistent -- declared identity, derived activity, the same rule as
# participation_generated.csv.
#
# SCOPE IS THE WHOLE POINT. The caches are county-wide (119,928 records). Counting
# all of them credited John Ascher with 47,596 identifications -- true, but not about
# Cabrillo. Counting only observations inside the park boundary (in_cabr, 37,679 of
# them) gives 6,256, within a hundred of the 6,153 someone once typed by hand. That
# closeness is the evidence the typed figures were always meant to be park-scoped.
# =============================================================
if (!exists("PATHS")) source("scripts/config.R")

ID_COUNTS_COLUMNS <- c("person_id", "n_bee", "n_plant", "total")

# identification_counts(): PURE. ids = data.frame(login, kind ("bee"/"plant"), n).
# Returns one row per person in `people`, zero included -- a person with no
# identifications still belongs on the page, just last.
identification_counts <- function(ids, people) {
  lc  <- function(x) tolower(trimws(ifelse(is.na(x), "", as.character(x))))
  out <- data.frame(person_id = as.character(people$person_id),
                    n_bee = 0L, n_plant = 0L, total = 0L, stringsAsFactors = FALSE)
  if (!nrow(people)) return(out[0, ])
  key <- lc(people$inaturalist_username)
  if (nrow(ids)) {
    l <- lc(ids$login); k <- lc(ids$kind); n <- as.integer(ids$n)
    for (i in seq_len(nrow(out))) {
      if (!nzchar(key[i])) next
      hit <- l == key[i]
      out$n_bee[i]   <- sum(n[hit & k == "bee"],   na.rm = TRUE)
      out$n_plant[i] <- sum(n[hit & k == "plant"], na.rm = TRUE)
    }
  }
  out$total <- out$n_bee + out$n_plant
  out[, ID_COUNTS_COLUMNS, drop = FALSE]
}

# Pull per-login identification tallies from one cache, restricted to CABR records.
.idc_from_cache <- function(db, cabr_ids, kind) {
  empty <- data.frame(login = character(0), kind = character(0), n = integer(0), stringsAsFactors = FALSE)
  if (!requireNamespace("duckdb", quietly = TRUE) || !file.exists(db) || !length(cabr_ids)) return(empty)
  con <- tryCatch(DBI::dbConnect(duckdb::duckdb(), db, read_only = TRUE), error = function(e) NULL)
  if (is.null(con)) return(empty)
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  duckdb::duckdb_register(con, "cabr_keep", data.frame(id = as.character(cabr_ids), stringsAsFactors = FALSE))
  r <- tryCatch(DBI::dbGetQuery(con, "
    with ids as (
      select unnest(from_json(json_extract(o.raw_data,'$.identifications'), '[\"JSON\"]')) AS d
      from inat_observations o join cabr_keep k on CAST(o.id AS VARCHAR) = k.id)
    select lower(trim(both '\"' from CAST(json_extract(d,'$.user.login') AS VARCHAR))) AS login,
           count(*) AS n
    from ids where json_extract(d,'$.user.login') is not null group by 1"),
    error = function(e) NULL)
  if (is.null(r) || !nrow(r)) return(empty)
  data.frame(login = r$login, kind = kind, n = as.integer(r$n), stringsAsFactors = FALSE)
}

build_identification_counts <- function(out = NULL) {
  rd <- function(p) if (!is.null(p) && file.exists(p))
    read.csv(p, stringsAsFactors = FALSE, check.names = FALSE) else data.frame()
  ppl <- rd(PATHS$people)
  if (!nrow(ppl)) { message("identification counts: no people.csv -- skipped"); return(invisible(NULL)) }
  raw <- rd(PATHS$inat_raw_membership)
  cabr <- if (nrow(raw) && "in_cabr" %in% names(raw))
            unique(as.character(raw$obs_id[as.logical(raw$in_cabr) %in% TRUE])) else character(0)
  ids <- rbind(.idc_from_cache(DB_CACHE_PATH,       cabr, "bee"),
               .idc_from_cache(DB_CACHE_PATH_PLANT, cabr, "plant"))
  cnt <- identification_counts(ids, ppl)
  out <- if (is.null(out)) PATHS$identification_counts else out
  write.csv(cnt, out, row.names = FALSE, na = "")
  invisible(cnt)
}
