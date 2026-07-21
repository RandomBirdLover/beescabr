library(testthat)
library(dplyr)
library(readr)

# Phase 3: collect every CABR plant name (specimen labels + plant obs), flag the
# ones master_crosswalk doesn't recognize, and (interactively) file them. The
# prompt is injectable so this runs offline.

src("project_info/collect_plant_names.R")

# a seeded crosswalk with one plant canonical + a transect concept row
.mk_cw <- function(path) {
  cw <- tibble(
    name = c("tp", "Isocoma menziesii"),
    what_for = c("transect", "plant_taxon"),
    applies_to_plant_bee_or_both = c("both", "plant"),
    specimen_label_variants = c("Tide Pool Trail", "Isocoma menziesii; Isocoma menzeisii"),
    inat_tag_variants = c("TP; #TP1", "")
  )
  write.csv(cw, path, row.names = FALSE, na = "")
  path
}
.mk_spec <- function(path, labels) {
  write.csv(tibble(flower_visited_raw = labels), path, row.names = FALSE, na = ""); path
}
.mk_plant <- function(path, sci) {
  write.csv(tibble(scientific_name = sci), path, row.names = FALSE, na = ""); path
}

test_that("collect_plant_names unions specimen labels + obs names with sources", {
  sp <- .mk_spec(tempfile(fileext=".csv"), c("Isocoma menziesii", "Salvia apiana"))
  pl <- .mk_plant(tempfile(fileext=".csv"), c("Isocoma menziesii", "Encelia californica"))
  got <- collect_plant_names(sp, pl)
  expect_setequal(got$name, c("Isocoma menziesii", "Salvia apiana", "Encelia californica"))
  # Isocoma menziesii seen in BOTH sources
  expect_equal(got$source[got$name == "Isocoma menziesii"], "observation+specimen")
})

test_that("unknown_plant_names: obs names are truth; only unrecognized specimen labels flag", {
  cwp <- .mk_cw(tempfile(fileext=".csv"))
  cw  <- read_csv(cwp, show_col_types = FALSE, col_types = cols(.default="c"))
  collected <- tibble(
    name   = c("Isocoma menzeisii", "Salvia apiana", "Encelia californica", "Obsonly rareplant"),
    source = c("specimen",          "specimen",      "observation+specimen", "observation"))
  unk <- unknown_plant_names(collected, cw)
  # menzeisii known via crosswalk; Encelia recognized via obs; Obsonly is obs-only (never flagged)
  expect_setequal(unk$name, "Salvia apiana")
})

test_that("plant_suggest catches a near-miss typo but not a distant name", {
  canon <- c("Isocoma menziesii", "Encelia californica")
  expect_equal(plant_suggest("Isacoma menziesii", canon), "Isocoma menziesii")  # 1-char typo
  expect_true(is.na(plant_suggest("Salvia apiana", canon)))                      # not close to anything
})

test_that("non-interactive review writes a worklist instead of prompting", {
  cwp <- .mk_cw(tempfile(fileext=".csv"))
  sp  <- .mk_spec(tempfile(fileext=".csv"), c("Salvia apiana"))
  pl  <- .mk_plant(tempfile(fileext=".csv"), character(0))
  wl  <- tempfile(fileext=".csv")
  review_plant_names(cw_path = cwp, specimen_clean_path = sp, plant_clean_path = pl,
                     interactive_ok = FALSE, worklist_path = wl, write = FALSE)
  expect_true(file.exists(wl))
  w <- read_csv(wl, show_col_types = FALSE)
  expect_true("Salvia apiana" %in% w$name)
})

test_that("interactive review files a typo under its canonical and adds a new plant", {
  cwp <- .mk_cw(tempfile(fileext=".csv"))
  # unknowns (sorted): "Isacoma menziesii" (typo), then "Salvia apiana" (new)
  sp  <- .mk_spec(tempfile(fileext=".csv"), c("Isacoma menziesii", "Salvia apiana", "Isocoma menziesii"))
  pl  <- .mk_plant(tempfile(fileext=".csv"), character(0))
  answers <- c("", "a"); i <- 0
  mock <- function(prompt) { i <<- i + 1; answers[i] }
  out <- review_plant_names(cw_path = cwp, specimen_clean_path = sp, plant_clean_path = pl,
                            prompt_fn = mock, interactive_ok = TRUE, write = TRUE)
  cw2 <- read_csv(cwp, show_col_types = FALSE, col_types = cols(.default="c"))
  # new canonical added
  expect_true(any(cw2$name == "Salvia apiana" & tolower(cw2$what_for) == "plant_taxon"))
  # typo filed as a variant under Isocoma menziesii
  iso <- cw2$specimen_label_variants[cw2$name == "Isocoma menziesii"]
  expect_true(grepl("isacoma menziesii", tolower(iso)))
})
