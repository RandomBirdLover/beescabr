# =============================================================
# specimens/tidy_raw_specimens.R
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
# Run: Rscript scripts/specimens/tidy_raw_specimens.R
#      (or) source("scripts/specimens/tidy_raw_specimens.R"); tidy_raw_specimens()
# =============================================================
suppressWarnings(suppressMessages({ library(dplyr); library(readxl) }))
if (!exists("bx_kv") && file.exists("scripts/utils/console.R")) source("scripts/utils/console.R")

local({
  need <- function(sym, file) if (!exists(sym)) source(file.path("scripts", file))
  need("read_latest",         "utils/utils.R")
  need("write_fresh",         "utils/utils.R")
  need("flag_raw_clutter",    "specimens/specimen_clean.R")
})

TRS_RECORDS_DIR     <- "data/specimens/records"
TRS_RECORDS_PATTERN <- "^cabr_bee_specimens_record_V"
TRS_WORKLIST_OUT    <- "data/specimens/specimens_clean/review/cabr_specimen_raw_cleanup_worklist.csv"

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
  bx_cont("RAW cleanup worklist: ", nrow(worklist), " row(s) need attention")
  bx_cont("needs_id (no genus): ", n_id, " | missing_specimen: ", n_miss, " | duplicate id: ", n_dup)
  bx_note("ID them, dedupe them, or delete them in the raw .xlsx (see TODO in this script's header).")

  if (write) {
    dir.create(dirname(TRS_WORKLIST_OUT), recursive = TRUE, showWarnings = FALSE)
    write_fresh(worklist, TRS_WORKLIST_OUT, row.names = FALSE)
    bx_out(basename(TRS_WORKLIST_OUT))
  }
  invisible(worklist)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) tidy_raw_specimens()
