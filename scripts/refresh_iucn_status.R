# =============================================================
# scripts/refresh_iucn_status.R
# beescabr -- pull the current IUCN Red List category for every bee species in the
# park and cache it, so the field guide can show a real "IUCN" column that stays up
# to date instead of a hand-maintained list.
#
# This is a DATA-REFRESH step, NOT part of run_all_analysis.R (which stays offline).
# It uses the ropensci `rredlist` package against the official IUCN Red List API v4,
# so it needs internet AND a free token:
#   1. Request a token (free) at  https://api.iucnredlist.org  -- approval can take a
#      day or two. You agree to the Red List Terms of Use.
#   2. Paste the token into  data/secrets/iucn_redlist.env  (on the IUCN_REDLIST_KEY= line).
#      That file is gitignored, so the token is never committed. (You can instead set the
#      IUCN_REDLIST_KEY environment variable, or run rredlist::rl_use_iucn("your-token").)
#   3. From the repo root:  Rscript scripts/refresh_iucn_status.R
#
# Writes data/checklists/iucn/iucn_status.csv (one row per species). bee_field_guide.R
# reads that file automatically and shows the IUCN column. Re-run whenever you want to
# refresh (IUCN publishes a couple of updates a year). Species IUCN has never assessed
# come back as "NE" (Not Evaluated) -- true for most solitary bees; the bumble bees are
# the assessed ones.
#
# Depends on: rredlist, dplyr, stringr (+ config.R).
# =============================================================

if (!requireNamespace("rredlist", quietly = TRUE))
  try(install.packages("rredlist", repos = "https://cloud.r-project.org"), silent = TRUE)
suppressPackageStartupMessages({ library(rredlist); library(dplyr); library(stringr) })
if (!exists("PATHS")) source("scripts/config.R")

OUT_DIR     <- "data/checklists/iucn"; dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
SECRET_FILE <- "data/secrets/iucn_redlist.env"    # gitignored -- paste your token in here

# token: environment variable first, then the gitignored secrets file
read_secret_key <- function(path) {
  if (!file.exists(path)) return("")
  ln <- readLines(path, warn = FALSE)
  ln <- ln[grepl("^\\s*IUCN_REDLIST_KEY\\s*=", ln)]                 # skip comments / blank lines
  if (!length(ln)) return("")
  v  <- sub("^\\s*IUCN_REDLIST_KEY\\s*=\\s*", "", ln[length(ln)])   # last definition wins
  trimws(gsub('^["\']|["\']$', "", trimws(v)))                      # strip optional quotes
}
KEY <- Sys.getenv("IUCN_REDLIST_KEY")
if (!nzchar(KEY)) KEY <- read_secret_key(SECRET_FILE)
if (!nzchar(KEY))
  stop("No IUCN token found. Paste your token into ", SECRET_FILE,
       " (on the IUCN_REDLIST_KEY= line), or set the IUCN_REDLIST_KEY environment variable. ",
       "Free token: https://api.iucnredlist.org.")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a)) ||
                            (is.character(a) && length(a) == 1 && !nzchar(a))) b else a
CODE_NAME <- c(EX = "Extinct", EW = "Extinct in the Wild", RE = "Regionally Extinct",
               CR = "Critically Endangered", EN = "Endangered", VU = "Vulnerable",
               NT = "Near Threatened", LC = "Least Concern", DD = "Data Deficient",
               NE = "Not Evaluated", LR = "Lower Risk")
name_of <- function(code) { n <- unname(CODE_NAME[toupper(code)]); if (is.na(n)) code else n }

# ---- species universe: every species-level bee in the two cleaned tables ----
grab <- function(df) data.frame(
  rank    = tolower(str_squish(df$taxon_rank)),
  genus   = str_squish(df$genus),
  epithet = tolower(word(str_squish(df$species), -1)),
  stringsAsFactors = FALSE)
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
sp <- bind_rows(grab(spec), grab(inat)) %>%
  filter(rank %in% c("species", "subspecies"), genus != "", epithet != "") %>%
  transmute(genus, epithet, scientific_name = paste(genus, epithet)) %>%
  distinct() %>% arrange(scientific_name)
message(sprintf("Querying IUCN Red List (v4, via rredlist) for %d bee species...", nrow(sp)))

# ---- latest global category for one species (NE if not assessed / not found) --
get_code <- function(g, e) {
  res <- tryCatch(rredlist::rl_species_latest(genus = g, species = e, key = KEY, parse = TRUE),
                  error = function(err) NULL)                 # 404 = not in the Red List -> NE
  if (is.null(res)) return(list(code = "NE", year = NA))
  code <- tryCatch(res$red_list_category$code, error = function(e2) NULL)
  year <- tryCatch(res$year_published %||% res$assessment_date, error = function(e2) NA)
  list(code = toupper(code %||% "NE"), year = year %||% NA)
}

rows <- lapply(seq_len(nrow(sp)), function(i) {
  cat <- tryCatch(get_code(sp$genus[i], sp$epithet[i]),
                  error = function(e) { message("  ! ", sp$scientific_name[i], ": ", conditionMessage(e)); NULL })
  if (is.null(cat)) cat <- list(code = "NE", year = NA)
  if (i %% 10 == 0) message(sprintf("  ...%d/%d", i, nrow(sp)))
  Sys.sleep(0.34)                                             # ~3 req/sec, courteous to the API
  data.frame(scientific_name = sp$scientific_name[i],
             iucn_code = cat$code, iucn_category = name_of(cat$code),
             assessment_year = as.character(cat$year %||% ""), stringsAsFactors = FALSE)
})
out <- do.call(rbind, rows)
out$source       <- "IUCN Red List API v4 (rredlist)"
out$retrieved_on <- as.character(Sys.Date())
write.csv(out, file.path(OUT_DIR, "iucn_status.csv"), row.names = FALSE)

thr <- out$scientific_name[out$iucn_code %in% c("CR", "EN", "VU", "NT")]
message(sprintf("Wrote %s  (%d species; %d threatened/near-threatened)",
                file.path(OUT_DIR, "iucn_status.csv"), nrow(out), length(thr)))
if (length(thr)) message("  Flagged: ", paste(thr, collapse = ", "))
message("Now re-run the field guide (or run_all_analysis.R) and it will show the IUCN column.")
