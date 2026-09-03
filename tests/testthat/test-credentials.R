# Credentials belong to the PERSON running the pipeline, never to whoever wrote it.
# Before this existed, a missing iNaturalist key silently switched authentication off and
# the pull returned obscured coordinates for sensitive taxa with no warning at all. The
# reader now asks, so an unauthenticated run is a decision rather than an accident.

src("utils/credentials.R")

tmp_env <- function(lines = character(0)) {
  f <- tempfile(fileext = ".env"); writeLines(lines, f); f
}

test_that("a value already in the environment wins over the file", {
  f <- tmp_env("MY_KEY=from_file")
  withr::with_envvar(c(MY_KEY = "from_env"), {
    expect_equal(cred_get("MY_KEY", f, ask = FALSE), "from_env")
  })
})

test_that("otherwise it reads the secrets file", {
  f <- tmp_env(c("# a comment", 'MY_KEY = "from_file"'))
  withr::with_envvar(c(MY_KEY = ""), expect_equal(cred_get("MY_KEY", f, ask = FALSE), "from_file"))
})

test_that("a missing key returns empty when asking is off", {
  f <- tmp_env("OTHER=x")
  withr::with_envvar(c(MY_KEY = ""), expect_equal(cred_get("MY_KEY", f, ask = FALSE), ""))
})

test_that("a missing key PROMPTS, and what is typed is returned", {
  f <- tmp_env(character(0))
  withr::with_envvar(c(MY_KEY = ""), {
    got <- cred_get("MY_KEY", f, ask = TRUE, is_interactive = TRUE, prompt_fn = function(...) "typed_value",
                    confirm_fn = function(...) "n", say = function(...) invisible())
    expect_equal(got, "typed_value")
  })
})

test_that("saying yes writes it to the secrets file for next time", {
  f <- tmp_env(character(0))
  withr::with_envvar(c(MY_KEY = ""), {
    cred_get("MY_KEY", f, ask = TRUE, is_interactive = TRUE, prompt_fn = function(...) "typed_value",
             confirm_fn = function(...) "y", say = function(...) invisible())
  })
  expect_true(any(grepl("^MY_KEY=typed_value$", readLines(f))))
})

test_that("saying no leaves the file untouched, so nothing is stored", {
  f <- tmp_env(character(0))
  withr::with_envvar(c(MY_KEY = ""), {
    cred_get("MY_KEY", f, ask = TRUE, is_interactive = TRUE, prompt_fn = function(...) "typed_value",
             confirm_fn = function(...) "n", say = function(...) invisible())
  })
  expect_false(any(grepl("MY_KEY", readLines(f))))
})

test_that("an empty answer means 'skip', not an empty key saved to disk", {
  f <- tmp_env(character(0))
  withr::with_envvar(c(MY_KEY = ""), {
    got <- cred_get("MY_KEY", f, ask = TRUE, is_interactive = TRUE, prompt_fn = function(...) "",
                    confirm_fn = function(...) "y", say = function(...) invisible())
    expect_equal(got, "")
  })
  expect_false(any(grepl("MY_KEY", readLines(f))))
})

test_that("writing a key REPLACES its line rather than appending a duplicate", {
  # cred_get never reaches this with a filled file (a present key is used, not re-asked),
  # so the writer is where replace-vs-append actually matters.
  f <- tmp_env(c("OTHER=keep_me", "MY_KEY=old_value"))
  .cred_to_file("MY_KEY", "new_value", f)
  ln <- readLines(f)
  expect_length(grep("^MY_KEY=", ln), 1)
  expect_true("MY_KEY=new_value" %in% ln)
  expect_true("OTHER=keep_me" %in% ln)   # other keys survive
})

test_that("a non-interactive run never blocks on a prompt", {
  f <- tmp_env(character(0))
  withr::with_envvar(c(MY_KEY = ""), {
    expect_equal(cred_get("MY_KEY", f, ask = TRUE, is_interactive = FALSE,
                          say = function(...) invisible()), "")
  })
})

# ---- the value must stay secret once typed --------------------------------------
test_that("a key is never shown in full, only masked", {
  expect_equal(cred_mask("abcdefghijklmnop"), "****mnop")
  expect_equal(cred_mask("abc"), "****")          # too short to show any of it
  expect_equal(cred_mask(""), "(none)")
})

test_that("nothing printed while asking contains the typed value", {
  f <- tempfile(fileext = ".env"); writeLines(character(0), f)
  said <- character(0)
  withr::with_envvar(c(MY_KEY = ""), {
    cred_get("MY_KEY", f, ask = TRUE, is_interactive = TRUE, prompt_fn = function(...) "SUPERSECRETVALUE",
             confirm_fn = function(...) "y", say = function(...) said <<- c(said, paste0(...)))
  })
  expect_false(any(grepl("SUPERSECRETVALUE", said)))
})

test_that("the secrets file is written owner-only", {
  f <- tempfile(fileext = ".env"); writeLines(character(0), f)
  withr::with_envvar(c(MY_KEY = ""), {
    cred_get("MY_KEY", f, ask = TRUE, is_interactive = TRUE, prompt_fn = function(...) "typed_value",
             confirm_fn = function(...) "y", say = function(...) invisible())
  })
  expect_equal(substr(as.character(file.info(f)$mode), 1, 3), "600")
})
