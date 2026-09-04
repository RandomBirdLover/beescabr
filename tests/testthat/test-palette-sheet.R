# BEE_THEMES_PALLETE.png is how somebody picks a color without reading the theme
# file. A palette that never reaches the sheet is invisible, so the next person
# invents a hex instead -- which is the drift theme_beescabr.R exists to stop.
# beeple-vs-intern was added and the sheet was never updated, and nothing noticed.
#
# So: every color palette in the theme is either DRAWN on the sheet or named in
# PALETTE_NOT_CHARTED with a reason. Adding a palette and forgetting the sheet
# now fails here.
source(file.path("..", "..", "scripts", "analysis", "shared", "theme_beescabr.R"))

.theme_palettes <- function() {
  e <- new.env()
  sys.source(file.path("..", "..", "scripts", "analysis", "shared", "theme_beescabr.R"), e)
  nm <- ls(e)
  Filter(function(n) {
    v <- get(n, envir = e)
    is.character(v) && length(v) && all(grepl("^#[0-9A-Fa-f]{6}$", v))
  }, nm)
}

# Read just the PALETTE_NOT_CHARTED literal out of the file. render_palette.R
# sources its dependencies with repo-root-relative paths and cannot be sourced
# from here, and the declaration is a plain c(...) literal either way.
.not_charted <- function() {
  txt <- paste(readLines(file.path("..", "..", "scripts", "analysis", "render_palette.R"),
                         warn = FALSE), collapse = "\n")
  m <- regmatches(txt, regexpr("PALETTE_NOT_CHARTED\\s*<-\\s*c\\((?:[^()]|\\([^()]*\\))*\\)",
                               txt, perl = TRUE))
  if (!length(m)) return(character(0))
  eval(parse(text = sub("^PALETTE_NOT_CHARTED\\s*<-\\s*", "", m)))
}

test_that("every theme palette is on the sheet or declared not-charted", {
  rp <- readLines(file.path("..", "..", "scripts", "analysis", "render_palette.R"),
                  warn = FALSE)
  body <- paste(rp, collapse = "\n")

  charted <- vapply(.theme_palettes(), function(n)
    grepl(paste0("(?<![A-Z_])", n, "(?![A-Z_])"), body, perl = TRUE), logical(1))

  skipped <- .not_charted()

  missing <- setdiff(names(charted)[!charted], names(skipped))
  expect_equal(sort(missing), character(0),
               info = paste("not on the palette sheet:", paste(sort(missing), collapse = ", ")))
})

test_that("beeple vs interns is on the sheet", {
  body <- paste(readLines(file.path("..", "..", "scripts", "analysis", "render_palette.R"),
                          warn = FALSE), collapse = "\n")
  expect_match(body, "BEE_OBSERVER_COL", fixed = TRUE)
})

test_that("a not-charted palette has to say why", {
  skipped <- .not_charted()
  if (!length(skipped)) succeed()
  else {
    expect_true(!is.null(names(skipped)) && all(nzchar(names(skipped))))
    expect_true(all(nzchar(skipped)))
  }
})

# The landing rows are parsed from publish_pages.R rather than typed here, so the
# sheet tracks the real site. When that CSS moved to __W_BG__ tokens the regex
# stopped matching, and the sheet quietly rendered two EMPTY rows for a release.
# An empty row is worse than a missing one: it says "these colors are nothing".
test_that("the landing rows actually resolve to colors", {
  e <- new.env()
  old <- setwd(file.path("..", "..")); on.exit(setwd(old), add = TRUE)
  sys.source("scripts/analysis/render_palette.R", e)
  for (v in c("LAND_LIGHT", "LAND_DARK")) {
    got <- get(v, envir = e)
    expect_gt(length(got), 0)
    expect_true(all(grepl("^#[0-9A-Fa-f]{3,8}$", got)), info = v)
  }
})
