# A generated REFERENCES_CITED.txt for the whole analysis folder. The folder notes
# carry a short citation; this is where the full reference lives, plus the software
# versions the numbers were actually produced with.
source(file.path("..", "..", "scripts", "analysis", "shared", "folder_readmes.R"))

test_that("every method source carries a full reference, not just a short form", {
  for (x in STAT_SOURCES) {
    expect_true(!is.null(x$full) && nzchar(x$full), info = x$src)
    expect_gt(nchar(x$full), nchar(x$src))
  }
})

test_that("a reference says whether it came from the package or from a person", {
  # anything marked verified was read out of the installed package's own Rd or
  # citation(); everything else is typed by hand and should be spot-checked
  for (x in STAT_SOURCES) expect_true(is.logical(x$verified), info = x$src)
  expect_true(any(vapply(STAT_SOURCES, function(x) isTRUE(x$verified), logical(1))))
  expect_true(any(vapply(STAT_SOURCES, function(x) !isTRUE(x$verified), logical(1))))
})

test_that("the file lists the software versions the numbers came from", {
  txt <- references_text()
  expect_match(txt, "vegan", fixed = TRUE)
  expect_match(txt, "iNEXT", fixed = TRUE)
  expect_match(txt, as.character(getRversion()), fixed = TRUE)
  expect_match(txt, as.character(utils::packageVersion("vegan")), fixed = TRUE)
})

test_that("the verified ones are marked, and so is the reason it matters", {
  txt <- references_text()
  expect_match(txt, "Almeida-Neto", fixed = TRUE)
  expect_match(txt, "Oikos", fixed = TRUE)             # a full journal reference
  expect_match(txt, "checked against", ignore.case = TRUE)
})

test_that("references are sorted by author, the way a reference list is read", {
  txt <- references_text()
  lines <- strsplit(txt, "\n")[[1]]
  # an entry starts at the margin, optionally behind the [pkg] marker; its
  # continuation lines are indented, which is what makes the list readable
  ent <- grep("^(\\[pkg\\] )?[A-Z][a-zA-Z-]+, ", lines, value = TRUE)
  who <- sub("^(\\[pkg\\] )?", "", ent)
  expect_gt(length(who), 8)
  expect_equal(who, sort(who))
})

test_that("no line wraps past a terminal width", {
  expect_lt(max(nchar(strsplit(references_text(), "\n")[[1]])), 90)
})
