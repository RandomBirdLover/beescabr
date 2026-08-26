# IUCN status and plant common names are cached and only refreshed when someone
# remembers BEESCABR_REFRESH=1 -- which is easy to forget for a whole year. The
# pipeline now checks how old the cached values actually are and says so.
#
# Age comes from each cache's own retrieved_on column, not the file's mtime: a file
# rewritten for an unrelated reason must not look freshly checked.

src("utils/refresh_due.R")

mk <- function(dates) {
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(genus = "X", retrieved_on = dates), f, row.names = FALSE)
  f
}

test_that("a freshly retrieved cache is not due", {
  r <- refresh_due(mk(Sys.Date() - 10), max_age_days = 365)
  expect_false(r$due)
  expect_equal(r$age_days, 10L)
})

test_that("a cache older than the limit IS due", {
  r <- refresh_due(mk(Sys.Date() - 400), max_age_days = 365)
  expect_true(r$due)
  expect_equal(r$age_days, 400L)
})

test_that("age is judged by the OLDEST entry, not the newest", {
  # one taxon rechecked yesterday must not make a years-old table look current
  r <- refresh_due(mk(c(Sys.Date() - 1, Sys.Date() - 500)), max_age_days = 365)
  expect_true(r$due)
  expect_equal(r$age_days, 500L)
})

test_that("a missing cache is due, and says so rather than erroring", {
  r <- refresh_due(file.path(tempdir(), "nope.csv"), max_age_days = 365)
  expect_true(r$due)
  expect_true(is.na(r$age_days))
  expect_match(r$reason, "never|missing", ignore.case = TRUE)
})

test_that("a cache with no usable dates is due", {
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(genus = "X", retrieved_on = NA), f, row.names = FALSE)
  r <- refresh_due(f, max_age_days = 365)
  expect_true(r$due)
})

test_that("the file's mtime is ignored -- only retrieved_on counts", {
  f <- mk(Sys.Date() - 400)
  Sys.setFileTime(f, Sys.time())        # touched just now, but the DATA is old
  expect_true(refresh_due(f, max_age_days = 365)$due)
})

# ---- the pipeline-facing roll-up --------------------------------------------

test_that("refresh_overdue lists only the caches past their limit", {
  fresh <- mk(Sys.Date() - 5); stale <- mk(Sys.Date() - 500)
  caches <- list(list(key = "fresh one", path = fresh, tool = "a.R", needs = "internet"),
                 list(key = "stale one", path = stale, tool = "b.R", needs = "internet"))
  out <- refresh_overdue(caches, max_age_days = 365)
  expect_equal(length(out), 1L)
  expect_equal(out[[1]]$key, "stale one")
  expect_true(grepl("500 days ago", out[[1]]$reason))
})

test_that("refresh_overdue is empty when everything is current", {
  caches <- list(list(key = "a", path = mk(Sys.Date() - 5), tool = "a.R", needs = "internet"))
  expect_equal(length(refresh_overdue(caches, max_age_days = 365)), 0L)
})

test_that("the REAL caches are both checkable (paths exist and carry dates)", {
  old <- setwd(.beescabr_root()); on.exit(setwd(old), add = TRUE)
  for (c in REFRESH_CACHES) {
    r <- refresh_due(c$path)
    expect_false(is.na(r$age_days), info = paste(c$key, "has no usable retrieved_on"))
  }
})

# ---- the yearly confirm ------------------------------------------------------
# Nothing is actually overdue today and testthat runs non-interactively, so these
# supply a fake overdue cache and say so explicitly.
fake_overdue <- list(list(key = "IUCN Red List status", reason = "oldest entry checked 2024-01-01 (600 days ago)"))
confirm <- function(read_fn, say = function(...) invisible())
  refresh_confirm(overdue = fake_overdue, read_fn = read_fn, is_interactive = TRUE, say = say)
# A refresh is online, takes minutes, and needs an IUCN token. It is not a name-
# judgement task (that is the full rebuild), so the prompt says what it really does.

test_that("Y runs the refresh", {
  expect_true(confirm(function(...) "y"))
  expect_true(confirm(function(...) "Y"))
})

test_that("N keeps the cache and the run continues normally", {
  expect_false(confirm(function(...) "n"))
})

test_that("Enter defaults to NOT refreshing -- the safe, offline-ish choice", {
  expect_false(confirm(function(...) ""))
})

test_that("anything unrecognised is treated as no", {
  expect_false(confirm(function(...) "banana"))
})

test_that("a non-interactive run never prompts and never refreshes on its own", {
  called <- FALSE
  r <- refresh_confirm(overdue = fake_overdue, read_fn = function(...) { called <<- TRUE; "y" },
                       is_interactive = FALSE, say = function(...) invisible())
  expect_false(called)
  expect_false(r)
})

test_that("the prompt states what the refresh needs and what it does NOT need", {
  said <- character(0)
  confirm(function(...) "n", say = function(...) said <<- c(said, paste0(...)))
  blob <- paste(said, collapse = " ")
  expect_true(grepl("internet", blob, ignore.case = TRUE))
  expect_true(grepl("token",    blob, ignore.case = TRUE))
  expect_true(grepl("keep|cache", blob, ignore.case = TRUE))
})
