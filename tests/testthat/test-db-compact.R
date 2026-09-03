library(testthat)

# Compacting the observation cache.
#
# DuckDB never returns space to the filesystem on its own: an updated row leaves
# the old copy behind as a dead block, and an incremental ingest does that every
# run. The bee cache had reached 31 GB holding 78,578 observations. Copying it
# into a fresh file brought it to 7.9 GB with every row intact -- most of the
# saving is re-compression, not just the dead blocks.
#
# Nothing here may ever lose a row, so the copy is verified against the original
# BEFORE it replaces it, and the original is only removed once that passes.

skip_if_no_store <- function() {
  if (!have_duckdb()) skip("duckdb R package not installed")
}
src("inat_observations/engine/db/compact_store.R")

test_that("it compacts only when the dead space is worth reclaiming", {
  # pure decision, no database needed: this is what runs after every ingest.
  # 128087/28679 are the real numbers the bee cache carried at 31 GB (22% dead).
  expect_true(db_should_compact(total_blocks = 128087, free_blocks = 28679, threshold = 0.2))
  expect_false(db_should_compact(total_blocks = 128087, free_blocks = 10000, threshold = 0.2))
  expect_false(db_should_compact(total_blocks = 0,      free_blocks = 0,     threshold = 0.2))
})

test_that("a freshly compacted database is not compacted again", {
  # after a rewrite DuckDB reports zero free blocks; the next run must skip
  expect_false(db_should_compact(total_blocks = 32406, free_blocks = 0, threshold = 0.2))
})

test_that("a tiny database is left alone whatever its ratio", {
  # a fresh cache is mostly empty by ratio; rewriting it would be pure cost
  expect_false(db_should_compact(total_blocks = 5, free_blocks = 4, threshold = 0.2, min_blocks = 100))
})

test_that("compacting keeps every row and every value", {
  skip_if_no_store()
  suppressMessages(library(duckdb))
  p <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb(), p)
  dbExecute(con, "CREATE TABLE t AS SELECT i AS id, ('row ' || i) AS label FROM range(5000) s(i)")
  dbExecute(con, "DELETE FROM t WHERE id % 2 = 0")          # leave dead space behind
  before <- dbGetQuery(con, "SELECT count(*) n, sum(id) s FROM t")
  dbDisconnect(con, shutdown = TRUE)

  res <- db_compact(p)
  expect_true(res$ok)
  expect_true(file.exists(p))

  con <- dbConnect(duckdb(), p, read_only = TRUE)
  after <- dbGetQuery(con, "SELECT count(*) n, sum(id) s FROM t")
  dbDisconnect(con, shutdown = TRUE)
  expect_equal(after$n, before$n)
  expect_equal(after$s, before$s)
})

test_that("a failed verification leaves the original untouched", {
  skip_if_no_store()
  suppressMessages(library(duckdb))
  p <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb(), p)
  dbExecute(con, "CREATE TABLE t AS SELECT 1 AS id")
  dbDisconnect(con, shutdown = TRUE)
  size_before <- file.info(p)$size

  # a verifier that always disagrees must abort the swap, not destroy the cache
  res <- db_compact(p, verify_fn = function(...) FALSE)
  expect_false(res$ok)
  expect_true(file.exists(p))
  expect_equal(file.info(p)$size, size_before)
  con <- dbConnect(duckdb(), p, read_only = TRUE)
  expect_equal(dbGetQuery(con, "SELECT count(*) n FROM t")$n, 1)
  dbDisconnect(con, shutdown = TRUE)
})

test_that("a missing database is a no-op, not an error", {
  expect_false(db_compact(file.path(tempdir(), "no-such.duckdb"))$ok)
})
