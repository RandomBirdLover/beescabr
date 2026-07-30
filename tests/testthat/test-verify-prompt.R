# test-verify-prompt.R -- interactive pass-2 verification (verify / reject-for-now / skip),
# with memory: a rejected taxon is remembered, re-shown next run, and un-rejected if verified.
suppressWarnings(suppressMessages(library(testthat)))
.impl <- function() src("reference/verify_prompt.R")
queue_prompt <- function(answers) { i <- 0; function(...) { i <<- i + 1; answers[min(i, length(answers))] } }

test_that(".verify_parse maps answers", {
  .impl()
  expect_equal(.verify_parse("y"),      "verify")
  expect_equal(.verify_parse("r"),      "reject")
  expect_equal(.verify_parse("reject"), "reject")
  expect_equal(.verify_parse(""),       "skip")
  expect_equal(.verify_parse("n"),      "skip")
  expect_equal(.verify_parse("x"),      "stop")
  expect_equal(.verify_parse("huh"),    "reask")
})

test_that("unverified_rows keeps only falsey-verified rows with a real id", {
  .impl()
  lk <- data.frame(taxon_id = c(1, 2, 3, NA), scientific_name = c("a", "b", "c", "d"),
                   verified = c("FALSE", "TRUE", "false", ""), stringsAsFactors = FALSE)
  expect_equal(sort(unverified_rows(lk)$taxon_id), c(1, 3))   # 2 verified; NA-id dropped
})

test_that(".pv_fill_names gives blank scientific_names a readable label by rank", {
  .impl()
  needs <- data.frame(
    taxon_id        = c(747170, 1266534, 12345, 67890, 99999),
    rank            = c("species", "complex", "subgenus", "genus", "genus"),
    scientific_name = c("Melissodes microstictus", "", "", "", ""),
    genus           = c("Melissodes", "", "Andrena", "Megandrena", ""),
    subgenus        = c("", "", "Callandrena", "", ""),
    complex         = c("", "(Complex) Bombus fervidus", "", "", ""),
    species         = c("microstictus", "", "", "", ""),
    common_name     = c("", "", "", "", "a mining bee"),
    stringsAsFactors = FALSE)
  out <- .pv_fill_names(needs)
  expect_equal(out[1], "Melissodes microstictus")   # already named -> unchanged
  expect_equal(out[2], "Bombus fervidus")           # complex -> strip "(Complex) " prefix
  expect_equal(out[3], "Andrena (Callandrena)")     # subgenus -> Genus (Subgenus)
  expect_equal(out[4], "Megandrena")                # genus -> genus
  expect_equal(out[5], "a mining bee")              # nothing else -> common_name fallback
})

test_that("resolver: verify / reject / skip captured; prev-rejected still processed", {
  .impl()
  needs <- data.frame(taxon_id = c(747170, 309284, 179703),
                      scientific_name = c("Melissodes microstictus", "Perdita larreae", "Colletes phaceliae"),
                      rank = "species", stringsAsFactors = FALSE)
  r <- resolve_verification_interactive(needs, prompt_fn = queue_prompt(c("y", "r", "")), interactive_ok = TRUE)
  expect_equal(r$verified_ids, 747170L)
  expect_equal(r$rejected_ids, 309284L)
  expect_false(r$stopped)
  # a previously-rejected taxon is STILL shown next run -> here the reviewer upgrades it
  r2 <- resolve_verification_interactive(needs[2, ], prev_rejected = 309284,
                                         prompt_fn = queue_prompt(c("y")), interactive_ok = TRUE)
  expect_equal(r2$verified_ids, 309284L)
})

test_that("prompt_verify_taxa saves when verified_taxa.csv is HEADER-ONLY (post-reset)", {
  .impl()
  # This is the exact bug that silently lost a whole pass-2 session: a reset leaves the
  # file as a bare header, read.csv gives logical(0) columns, and bind_rows(<logical>,
  # <character>) threw -- swallowed by run_pipeline's tryCatch.
  lk <- data.frame(taxon_id = c(747170, 1266534),
                   scientific_name = c("Melissodes microstictus", ""),
                   rank = c("species", "complex"), genus = c("Melissodes", ""),
                   complex = c("", "(Complex) Bombus fervidus"),
                   verified = c("FALSE", "FALSE"), stringsAsFactors = FALSE)
  vf <- tempfile(fileext = ".csv"); writeLines('"taxon_id","scientific_name","verified"', vf)  # header only
  rf <- tempfile(fileext = ".csv")
  prompt_verify_taxa(lk, verified_path = vf, rejected_path = rf, prompt_fn = queue_prompt(c("y", "y")),
                     interactive_ok = TRUE, write = TRUE, verbose = FALSE)
  got <- read.csv(vf)
  expect_equal(nrow(got), 2)                    # both saved despite the header-only start
  expect_true(all(c(747170, 1266534) %in% got$taxon_id))
})

test_that("prompt_verify_taxa: reject is remembered + re-asked; verifying un-rejects", {
  .impl()
  lk <- data.frame(taxon_id = c(747170, 309284),
                   scientific_name = c("Melissodes microstictus", "Perdita larreae"),
                   rank = "species", verified = c("FALSE", "FALSE"), stringsAsFactors = FALSE)
  vf <- tempfile(fileext = ".csv"); rf <- tempfile(fileext = ".csv")
  prompt_verify_taxa(lk, verified_path = vf, rejected_path = rf,
                     prompt_fn = queue_prompt(c("y", "r")), interactive_ok = TRUE, write = TRUE, verbose = FALSE)
  expect_true(747170 %in% read.csv(vf)$taxon_id)     # verified
  expect_true(309284 %in% .pv_read_ids(rf))          # reject-for-now remembered
  # NEXT run: the rejected taxon is RE-ASKED (not hidden). This time verify it -> un-rejected.
  lk2 <- lk[lk$taxon_id == 309284, , drop = FALSE]
  vf2 <- tempfile(fileext = ".csv")
  res <- prompt_verify_taxa(lk2, verified_path = vf2, rejected_path = rf,
                            prompt_fn = queue_prompt(c("y")), interactive_ok = TRUE, write = TRUE, verbose = FALSE)
  expect_equal(res$verified_ids, 309284L)            # it was re-asked, not silently skipped
  expect_true(309284 %in% read.csv(vf2)$taxon_id)
  expect_false(309284 %in% .pv_read_ids(rf))         # removed from rejected (un-rejected)
})
