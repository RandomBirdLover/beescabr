# test-verify-prompt.R -- interactive pass-2 verification prompt
suppressWarnings(suppressMessages(library(testthat)))
.impl <- function() src("reference/verify_prompt.R")
queue_prompt <- function(answers) { i <- 0; function(...) { i <<- i + 1; answers[min(i, length(answers))] } }

test_that(".verify_parse maps answers", {
  .impl()
  expect_equal(.verify_parse("y"),    "verify")
  expect_equal(.verify_parse("yes"),  "verify")
  expect_equal(.verify_parse(""),     "skip")
  expect_equal(.verify_parse("n"),    "skip")
  expect_equal(.verify_parse("skip"), "skip")
  expect_equal(.verify_parse("x"),    "stop")
  expect_equal(.verify_parse("huh"),  "reask")
})

test_that("unverified_rows keeps only falsey-verified rows with a real id", {
  .impl()
  lk <- data.frame(taxon_id = c(1, 2, 3, NA), scientific_name = c("a", "b", "c", "d"),
                   verified = c("FALSE", "TRUE", "false", ""), stringsAsFactors = FALSE)
  expect_equal(sort(unverified_rows(lk)$taxon_id), c(1, 3))   # 2 is verified; NA-id dropped
})

test_that("resolver: y verifies, Enter skips, stop halts, non-interactive no-op", {
  .impl()
  needs <- data.frame(taxon_id = c(747170, 179703),
                      scientific_name = c("Melissodes microstictus", "Colletes phaceliae"),
                      rank = "species", stringsAsFactors = FALSE)
  r1 <- resolve_verification_interactive(needs, queue_prompt(c("y", "")), interactive_ok = TRUE)  # verify #1, skip #2
  expect_equal(r1$verified_ids, 747170L); expect_false(r1$stopped)
  r2 <- resolve_verification_interactive(needs, queue_prompt(c("x")), interactive_ok = TRUE)       # stop on #1
  expect_true(r2$stopped); expect_length(r2$verified_ids, 0)
  r3 <- resolve_verification_interactive(needs, queue_prompt(c("y")), interactive_ok = FALSE)      # batch: no-op
  expect_length(r3$verified_ids, 0)
})

test_that("prompt_verify_taxa appends confirmed ids to verified_taxa.csv", {
  .impl()
  lk <- data.frame(taxon_id = c(747170, 179703),
                   scientific_name = c("Melissodes microstictus", "Colletes phaceliae"),
                   rank = "species", verified = c("FALSE", "FALSE"), stringsAsFactors = FALSE)
  tf <- tempfile(fileext = ".csv")
  prompt_verify_taxa(lk, verified_path = tf, prompt_fn = queue_prompt(c("y", "y")),
                     interactive_ok = TRUE, write = TRUE, verbose = FALSE)
  v <- read.csv(tf, stringsAsFactors = FALSE)
  expect_true(all(c(747170, 179703) %in% v$taxon_id))
  expect_true(all(tolower(v$verified) == "yes"))
})
