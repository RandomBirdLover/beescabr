# test-specimen-idprompt.R -- interactive specimen taxon_id resolver
# Verifies: iNat SUGGESTION picking, answer parsing, and the accept/paste/skip/stop
# prompt loop that fills blank taxon_ids in the specimen-additions table.
suppressWarnings(suppressMessages(library(testthat)))
.impl <- function() src("reference/specimen_id_prompt.R")   # src() resolves from repo root (helper.R)

fake_fetch  <- function(results) function(term) results          # iNat search stub
queue_prompt <- function(answers) { i <- 0; function(...) { i <<- i + 1; answers[min(i, length(answers))] } }

test_that("suggest_taxon prefers an exact name+rank match", {
  .impl()
  cands <- list(
    list(id = 1,      name = "Melissodes",             rank = "genus"),
    list(id = 747170, name = "Melissodes microstictus", rank = "species"),
    list(id = 2,      name = "Melissodes microsticta",  rank = "species"))
  expect_equal(suggest_taxon(cands, "Melissodes microstictus", "species")$id, 747170L)
})

test_that("suggest_taxon falls back to first same-rank hit, else NULL", {
  .impl()
  expect_equal(suggest_taxon(list(list(id = 5, name = "Colletes x", rank = "species")),
                             "Colletes phaceliae", "species")$id, 5L)
  expect_null(suggest_taxon(list(), "x", "species"))
})

test_that(".spid_parse maps answers to actions", {
  .impl()
  expect_equal(.spid_parse("",       99L)$action, "accept")
  expect_equal(.spid_parse("",       99L)$id,     99L)
  expect_equal(.spid_parse("747170", NA)$action,  "id")
  expect_equal(.spid_parse("747170", NA)$id,      747170L)
  expect_equal(.spid_parse("s",      1L)$action,  "skip")
  expect_equal(.spid_parse("x",      1L)$action,  "stop")
  expect_equal(.spid_parse("huh",    1L)$action,  "reask")
  expect_equal(.spid_parse("",       NA)$action,  "reask")   # no suggestion + Enter -> reask
})

test_that("resolver: Enter accepts the iNat suggestion", {
  .impl()
  add <- data.frame(rank = "species", scientific_name = "Melissodes microstictus",
                    taxon_id = NA_integer_, genus = "Melissodes", species = "microstictus",
                    stringsAsFactors = FALSE)
  fetch <- fake_fetch(list(list(id = 747170, name = "Melissodes microstictus", rank = "species")))
  out <- resolve_specimen_additions_interactive(add, fetch, queue_prompt(c("")), interactive_ok = TRUE)
  expect_equal(out$additions$taxon_id, 747170L)
  expect_false(out$stopped)
})

test_that("resolver: pasted id overrides, skip leaves blank, stop halts", {
  .impl()
  base <- data.frame(rank = "species", scientific_name = "Colletes phaceliae",
                     taxon_id = NA_integer_, genus = "Colletes", species = "phaceliae",
                     stringsAsFactors = FALSE)
  fetch <- fake_fetch(list(list(id = 1, name = "Colletes wrong", rank = "species")))
  expect_equal(resolve_specimen_additions_interactive(base, fetch, queue_prompt(c("179703")), TRUE)$additions$taxon_id, 179703L)
  expect_true(is.na(resolve_specimen_additions_interactive(base, fetch, queue_prompt(c("s")), TRUE)$additions$taxon_id))
  st <- resolve_specimen_additions_interactive(base, fetch, queue_prompt(c("x")), TRUE)
  expect_true(st$stopped)
})

test_that("resolver: non-interactive is a no-op", {
  .impl()
  add <- data.frame(rank = "species", scientific_name = "X y", taxon_id = NA_integer_, stringsAsFactors = FALSE)
  out <- resolve_specimen_additions_interactive(add, fake_fetch(list()), queue_prompt(c("")), interactive_ok = FALSE)
  expect_true(is.na(out$additions$taxon_id))
})

test_that("seed_additions_from_flags builds a full-lineage row for a NEW flagged taxon", {
  .impl()
  flags  <- data.frame(genus = "Melissodes", species = "microstictus", subspecies = "",
                       flag_reason = "genus+species combo not in taxonomy lookup", stringsAsFactors = FALSE)
  record <- data.frame(order = "Hymenoptera", family = "Apidae", subfamily = "Apinae",
                       tribe = "Eucerini", genus = "Melissodes", subgenus = "Eumelissodes",
                       complex = "", species = "microstictus", stringsAsFactors = FALSE)
  out <- seed_additions_from_flags(flags, record, existing = NULL)
  expect_equal(nrow(out), 1L)
  expect_equal(out$scientific_name, "Melissodes microstictus")
  expect_true(is.na(out$taxon_id))                       # blank -> the prompt fills it
  expect_equal(out$in_cabr_specimens, "TRUE")
  expect_equal(out$family, "Apidae");   expect_equal(out$subfamily, "Apinae")
  expect_equal(out$tribe, "Eucerini");  expect_equal(out$subgenus, "Eumelissodes")
  expect_equal(out$kingdom, "Animalia"); expect_equal(out$superfamily, "Apoidea")   # full parents carried
})

test_that("seed_additions_from_flags skips taxa already in additions, and blank species", {
  .impl()
  flags <- data.frame(genus = c("Colletes", "Lasioglossum"), species = c("phaceliae", ""),
                      subspecies = "", stringsAsFactors = FALSE)
  record <- data.frame(genus = "Colletes", species = "phaceliae", family = "Colletidae", stringsAsFactors = FALSE)
  existing <- data.frame(scientific_name = "Colletes phaceliae", stringsAsFactors = FALSE)
  expect_equal(nrow(seed_additions_from_flags(flags, record, existing)), 0L)
})
