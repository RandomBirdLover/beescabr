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
  bx_note("Fix these in the current workbook. Find each row by its ucsd_id / sdnhm_id.")
  bx_cont(.trs_workbook())
  bx_cont("Save a NEW version when done (next number, today's date), never edit an old one.")

  if (write) {
    dir.create(dirname(TRS_WORKLIST_OUT), recursive = TRUE, showWarnings = FALSE)
    write_fresh(worklist, TRS_WORKLIST_OUT, row.names = FALSE)
    bx_out(TRS_WORKLIST_OUT)
  }
  invisible(worklist)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) tidy_raw_specimens()
