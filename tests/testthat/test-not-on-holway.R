library(testthat)
library(dplyr)

# not_on_holway.R: CABR checklist taxa with holway == FALSE, annotated with current specimen/iNat
# record counts + a group label, and the grouped "N new bees not in Holway's Checklist" prompt.

src("analysis/not_on_holway.R")

make_checklist <- function() {
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    taxon_id        = c(100, 200, 300, 400, 500),
    taxon_rank      = c("complex", "complex", "species", "species", "complex"),
    scientific_name = c("Diadasia australis", "Lasioglossum gemmatum", "Colletes phaceliae",
                        "Bombus onholway", "Nomada vegana"),
    genus           = c("Diadasia", "Lasioglossum", "Colletes", "Bombus", "Nomada"),
    holway          = c(FALSE, FALSE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE), f, row.names = FALSE)
  f
}
make_inat <- function() tibble::tibble(
  taxon_id        = c(100, 100, 200, 500),
  scientific_name = c("Diadasia australis", "Diadasia australis", "Lasioglossum gemmatum", "Nomada vegana"),
  quality_grade   = c("research", "needs_id", "needs_id", "needs_id"))
make_spec <- function() tibble::tibble(
  taxon_id        = c(200, 300),
  scientific_name = c("Lasioglossum gemmatum", "Colletes phaceliae"))

test_that("not_on_holway_bees keeps only holway==FALSE taxa and groups them by evidence", {
  noh <- not_on_holway_bees(make_checklist(), make_spec(), make_inat())
  expect_setequal(noh$taxon_id, c("100", "200", "300", "500"))          # Bombus (on Holway) excluded
  g <- setNames(noh$group, noh$taxon_id)
  expect_equal(unname(g["100"]), "inat_only")                            # iNat, no specimen
  expect_equal(unname(g["200"]), "inat_and_collected")                   # iNat + specimen
  expect_equal(unname(g["300"]), "specimen_only")                        # specimen, no iNat
  expect_equal(unname(g["500"]), "inat_only")
  expect_equal(noh$n_inat_research_grade[noh$taxon_id == "100"], 1L)     # 1 of 2 research-grade
})

test_that("format_new_bees lists iNat-evidenced taxa only, grouped + numbered continuously", {
  noh <- not_on_holway_bees(make_checklist(), make_spec(), make_inat())
  out <- format_new_bees(noh, mode = "review")
  txt <- paste(out, collapse = "\n")
  expect_match(out[1], "^3 new bees that are not in Holway's Checklist$")  # 3 iNat-evidenced (not 4)
  expect_match(txt, "Seen only on iNat \\(never collected\\):")
  expect_match(txt, "Seen on iNat and collected:")
  expect_match(txt, "  1\\. \\(Complex\\) Diadasia australis")            # most-observed iNat-only first
  expect_match(txt, "  2\\. \\(Complex\\) Nomada vegana")
  expect_match(txt, "  3\\. \\(Complex\\) Lasioglossum gemmatum")         # collected group continues numbering
  expect_false(grepl("Colletes phaceliae", txt))                         # specimen-only NOT listed
  expect_match(txt, "Double-check these in review")                      # review framing
})

test_that("format_new_bees report mode uses park-facing framing, same taxa", {
  noh <- not_on_holway_bees(make_checklist(), make_spec(), make_inat())
  out <- format_new_bees(noh, mode = "report")
  expect_match(out[1], "^3 new bees that are not in Holway's Checklist$")
  expect_match(paste(out, collapse = "\n"), "park/county additions")
})

test_that("format_new_bees derives group from counts when absent (coverage summary_tbl shape)", {
  summ <- data.frame(scientific_name = "Diadasia australis", taxon_rank = "complex",
                     taxon_id = "100", n_inat_records = 5, n_specimen_records = 0,
                     stringsAsFactors = FALSE)
  out <- format_new_bees(summ, mode = "review")
  expect_match(paste(out, collapse = "\n"), "1\\. \\(Complex\\) Diadasia australis")
})

test_that("format_new_bees returns nothing when everything is on Holway / no iNat evidence", {
  expect_length(format_new_bees(.noh_empty()), 0)
  spec_only <- data.frame(scientific_name = "Colletes phaceliae", taxon_rank = "species",
                          taxon_id = "300", n_inat_records = 0, n_specimen_records = 4,
                          stringsAsFactors = FALSE)
  expect_length(format_new_bees(spec_only), 0)                           # specimen-only -> nothing to verify
})

make_holway <- function() {
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    scientific_name = c("Diadasia australis", "Nomada formula"),
    rank            = c("species", "species"),
    complex         = c("(Complex) Diadasia australis", "(Complex) Nomada vegana"),  # real Holway format: "(Complex) " tag
    stringsAsFactors = FALSE), f, row.names = FALSE)
  f
}

test_that("holway_name_set collects binomials from scientific_name AND the complex column", {
  s <- holway_name_set(make_holway())
  expect_true("diadasia australis" %in% s)   # from scientific_name
  expect_true("nomada vegana"      %in% s)   # from complex column (its members are on Holway)
  expect_true("nomada formula"     %in% s)
  expect_length(holway_name_set(NULL), 0)    # missing path -> empty, no correction applied
})

test_that("not_on_holway_bees drops rank/complex mismatches Holway actually lists, keeps true new names", {
  ck <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    taxon_id = c(1, 2, 3), taxon_rank = "complex",
    scientific_name = c("Diadasia australis", "Nomada vegana", "Perdita nova"),  # #1 species, #2 complex, #3 truly new
    genus = c("Diadasia", "Nomada", "Perdita"), holway = FALSE, stringsAsFactors = FALSE), ck, row.names = FALSE)
  inat <- tibble::tibble(taxon_id = c(1, 2, 3),
                         scientific_name = c("Diadasia australis", "Nomada vegana", "Perdita nova"),
                         quality_grade = "needs_id")
  spec <- tibble::tibble(taxon_id = integer(), scientific_name = character())
  noh <- not_on_holway_bees(ck, spec, inat, holway_path = make_holway())
  expect_setequal(noh$scientific_name, "Perdita nova")   # only the name Holway never lists survives
})

test_that("not_on_holway_bees skips the name correction when no Holway path (backward compatible)", {
  ck <- tempfile(fileext = ".csv")
  write.csv(data.frame(taxon_id = 1, taxon_rank = "complex", scientific_name = "Diadasia australis",
                       genus = "Diadasia", holway = FALSE, stringsAsFactors = FALSE), ck, row.names = FALSE)
  inat <- tibble::tibble(taxon_id = 1, scientific_name = "Diadasia australis", quality_grade = "needs_id")
  noh <- not_on_holway_bees(ck, tibble::tibble(taxon_id = integer(), scientific_name = character()), inat)
  expect_equal(nrow(noh), 1)                              # no holway_path -> flag trusted as-is
})
