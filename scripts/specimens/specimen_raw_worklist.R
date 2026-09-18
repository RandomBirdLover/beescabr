# =============================================================
# specimens/specimen_raw_worklist.R   (formerly tidy_raw_specimens.R)
# beescabr -- RAW specimen record hygiene worklist (run BY HAND).
#
# ┌───────────────────────────────────────────────────────────────────────────┐
# │ TODO -- the raw specimen .xlsx still needs manual cleanup:                  │
# │   1. ADD IDENTIFICATIONS  -- fill genus/species on the non-ID'd rows        │
# │                              (or delete them if they'll never be ID'd).     │
# │   2. REMOVE DUPLICATES     -- collapse the duplicate ucsd_id / sdnhm_id rows.│
# │   3. DROP MISSING SPECIMENS-- eventually delete the missing_specimen == "Y" │
# │                              rows once they're confirmed gone.              │
# └───────────────────────────────────────────────────────────────────────────┘
#
# This script does NOT edit the raw .xlsx (that stays the hand-curated source of
# truth). It reads the newest record and writes ONE consolidated WORKLIST of the
# cluttered rows -- each tagged needs_id / missing / duplicate -- so you can walk
# the raw sheet and either ID, dedupe, or delete them. Re-run it any time to see
# what's left.
#
# Run: source("scripts/specimens/specimen_raw_worklist.R")
#      (or) source("scripts/specimens/specimen_raw_worklist.R"); tidy_raw_specimens()
# =============================================================
suppressWarnings(suppressMessages({ library(dplyr); library(readxl) }))
if (!exists("bx_kv") && file.exists("scripts/utils/console.R")) source("scripts/utils/console.R")

local({
  need <- function(sym, file) if (!exists(sym)) source(file.path("scripts", file))
  need("read_latest",         "utils/utils.R")
  need("write_fresh",         "utils/utils.R")
  need("flag_raw_clutter",    "specimens/specimen_clean_helpers.R")
})

TRS_RECORDS_DIR     <- "data/specimens/records"
TRS_RECORDS_PATTERN <- "^cabr_bee_specimens_record_V"
# The newest specimen workbook, by V number -- "the raw .xlsx" names nineteen files.
# The three reasons, counted off the worklist the step just wrote. clean_specimens()
# reads it back rather than having the count threaded through two scripts.
.raw_worklist_counts <- function(path) {
  z <- c(needs_id = 0L, missing = 0L, duplicate = 0L)
  if (is.null(path) || !file.exists(path)) return(z)
  d <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(d) || !nrow(d) || !"reason" %in% names(d)) return(z)
  r <- as.character(d$reason)
  c(needs_id  = sum(grepl("needs_id", r), na.rm = TRUE),
    missing   = sum(grepl("missing_specimen", r), na.rm = TRUE),
    duplicate = sum(grepl("duplicate", r), na.rm = TRUE))
}

# The REVIEW NEEDED row for specimens nobody has named yet. It sits in the box
# because that is where the operator decides whether to stop, but it is the one
# item there that a re-run can never clear, so the text says so.
.needs_id_review_row <- function(n, file = basename(TRS_WORKLIST_OUT)) {
  if (!length(n) || is.na(n) || n <= 0L) return(NULL)
  data.frame(
    label = "not identified yet",
    count = as.integer(n),
    file  = file,
    what  = paste("These need a bee expert at a microscope, so this one is not a",
                  "now job -- it is here so you can see the whole picture before",
                  "deciding. It stays on the list until the determinations come back."),
    stringsAsFactors = FALSE)
}

.trs_workbook <- function(dir = "data/specimens/records") {
  f <- list.files(dir, pattern = "^cabr_bee_specimens_record_V[0-9]+_.*[.]xlsx$")
  if (!length(f)) return(file.path(dir, "the specimen workbook"))
  v <- suppressWarnings(as.integer(sub(".*_V([0-9]+)_.*", "\\1", f)))
  file.path(dir, f[which.max(v)])
}

TRS_WORKLIST_OUT    <- "data/specimens/specimens_clean/review/qc_review_specimen_cleanup_worklist_generated.csv"

# tidy_raw_specimens(): read the newest raw record, flag the cluttered rows
# (non-ID'd, missing, duplicate ids), and write the worklist. Returns it invisibly.
tidy_raw_specimens <- function(write = TRUE) {
  path <- read_latest(TRS_RECORDS_DIR, TRS_RECORDS_PATTERN)
  bx_kv("Raw specimens", "reading ", basename(path))
  raw <- suppressMessages(readxl::read_excel(path))

  id_cols <- intersect(c("ucsd_id", "sdnhm_id", "date", "plot", "collector",
                         "genus", "subgenus", "species", "subspecies", "missing_specimen"),
                       names(raw))
  pick <- function(df, reason_col) {
    if (nrow(df) == 0) return(tibble())
    df |> transmute(across(any_of(id_cols)), reason = .data[[reason_col]])
  }
  clutter <- flag_raw_clutter(raw)        # needs_id / missing
  dups    <- detect_duplicate_ids(raw)    # duplicate ucsd_id / sdnhm_id

  worklist <- bind_rows(pick(clutter, "clutter_reason"), pick(dups, "duplicate_reason"))
  if (nrow(worklist) && "ucsd_id" %in% names(worklist)) {
    worklist <- worklist |>
      group_by(across(any_of(setdiff(id_cols, character(0))))) |>
      summarise(reason = paste(sort(unique(reason)), collapse = "; "), .groups = "drop") |>
      arrange(reason)
  }

  n_id   <- sum(grepl("needs_id",  worklist$reason))
  n_miss <- sum(grepl("missing",   worklist$reason))
  n_dup  <- sum(grepl("duplicate", worklist$reason))
  # "the raw .xlsx" is one of nineteen versioned workbooks, and "see TODO in this
  # script's header" sends the operator into a code comment. Name the workbook and
  # say what each count means.
  if (n_id)   bx_cont(n_id, " specimen", if (n_id == 1L) "" else "s",
                      " have no identification yet")
  if (n_miss) bx_cont(n_miss, " row", if (n_miss == 1L) "" else "s",
                      " name a specimen that is not in the collection")
  if (n_dup)  bx_cont(n_dup, " museum number", if (n_dup == 1L) " is" else "s are",
                      " used twice")
  # No fix instructions here on purpose. The REVIEW NEEDED box a moment later prints
  # them via .specimen_fix_hint(); saying them twice made the box look like the
  # complete version of this block, so this block -- the only place needs_id appeared
  # -- was the one people skipped.

  if (write) {
    dir.create(dirname(TRS_WORKLIST_OUT), recursive = TRUE, showWarnings = FALSE)
    write_fresh(worklist, TRS_WORKLIST_OUT, row.names = FALSE)
    bx_out(TRS_WORKLIST_OUT)
  }
  invisible(worklist)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) tidy_raw_specimens()
