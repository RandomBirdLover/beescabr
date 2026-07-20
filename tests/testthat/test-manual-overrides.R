library(testthat)
library(dplyr)

# manual_overrides.R: user-curated answers for name-changed / synonym taxa the automated iNat
# search can't bridge. apply_manual_overrides fills the id (and corrects the name) at BOTH the
# Holway reference and taxonomy lookup levels; write_review_worklist emits the "look these up" prompt.

test_that("apply_manual_overrides fills id and corrects the name for a renamed species", {
  src("reference/manual_overrides.R")
  df <- tibble(
    rank = c("species", "species"),
    genus = c("Holcopasites", "Andrena"),
    species = c("minima", "baeriae"),
    subspecies = NA_character_,
    scientific_name = c("Holcopasites minima", "Andrena baeriae"),
    taxon_id = c(NA_integer_, 100L))
  ov <- tibble(rank = "species", name = "Holcopasites minima", taxon_id = 999L,
               correct_name = "Holcopasites minimus", note = "minima->minimus")
  out <- apply_manual_overrides(df, ov)
  expect_equal(out$taxon_id[1], 999L)
  expect_equal(out$scientific_name[1], "Holcopasites minimus")
  expect_equal(out$species[1], "minimus")            # epithet corrected to iNat's name
  expect_equal(out$genus[1], "Holcopasites")
  expect_equal(out$taxon_id[2], 100L)                # non-matching row untouched
  expect_equal(attr(out, "n_applied"), 1L)
})

test_that("apply_manual_overrides marks resolved and matches diacritics/case (id only)", {
  src("reference/manual_overrides.R")
  df <- tibble(rank = "subgenus", genus = "Xylocopa", subgenus = "Schönnherria",
               scientific_name = NA_character_, taxon_id = NA_integer_, resolved = FALSE)
  ov <- tibble(rank = "subgenus", name = "schonnherria", taxon_id = 571255L,
               correct_name = NA_character_, note = NA_character_)
  out <- apply_manual_overrides(df, ov)
  expect_equal(out$taxon_id[1], 571255L)
  expect_true(out$resolved[1])                       # override resolves the Holway row
})

test_that("apply_manual_overrides only matches the exact rank + name", {
  src("reference/manual_overrides.R")
  df <- tibble(rank = c("species", "genus"), genus = c("Holcopasites", "Holcopasites"),
               species = c("minima", NA_character_), taxon_id = NA_integer_,
               scientific_name = NA_character_)
  ov <- tibble(rank = "genus", name = "Holcopasites", taxon_id = 252959L,
               correct_name = NA_character_, note = NA_character_)
  out <- apply_manual_overrides(df, ov)
  expect_true(is.na(out$taxon_id[1]))    # species row NOT matched by a genus-rank override
  expect_equal(out$taxon_id[2], 252959L) # genus row matched
})

test_that("load_manual_overrides drops rows with a blank taxon_id", {
  src("reference/manual_overrides.R")
  p <- tempfile(fileext = ".csv")
  readr::write_csv(tibble(
    rank = c("species", "species"), name = c("Holcopasites minima", "Foo bar"),
    taxon_id = c(NA, 5L), correct_name = c("Holcopasites minimus", NA), note = NA_character_), p)
  ov <- load_manual_overrides(p)
  expect_equal(nrow(ov), 1L)
  expect_equal(ov$taxon_id, 5L)
})

test_that("write_review_worklist surfaces the resolver's not_found taxa, dropping answered ones", {
  src("reference/manual_overrides.R")
  # a resolver cache like resolve_missing_ids.R writes: key = "rank|name|parent"
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(tibble(
    key = c("species|holcopasites minima|252959",
            "genus|megandrena|47222",
            "species|melissodes lupinus|52781"),
    taxon_id = c(NA, NA, 709268L),
    status = c("not_found_or_ambiguous", "not_found_or_ambiguous", "filled")), cache)
  out <- tempfile(fileext = ".csv")
  # Megandrena already answered in the overrides -> must NOT appear
  ov <- tibble(rank = "genus", name = "Megandrena", taxon_id = 55L,
               correct_name = NA_character_, note = NA_character_)
  wl <- write_review_worklist(cache_path = cache, overrides = ov, path = out)
  expect_equal(wl$name, "Holcopasites minima")            # only the open not_found, title-cased
  expect_false("Melissodes lupinus" %in% wl$name)         # 'filled' status excluded
  expect_false("Megandrena" %in% wl$name)                 # already answered -> dropped
  expect_true(all(grepl("inaturalist.org/taxa/search", wl$inat_search_url)))
  expect_true(all(is.na(wl$taxon_id)))                    # blank column to fill in
})
