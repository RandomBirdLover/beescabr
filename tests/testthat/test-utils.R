# Tests for scripts/utils/utils.R helpers.

src("utils/utils.R")

test_that("decorate_complex prefixes only populated complex names", {
  df <- data.frame(
    genus   = c("Bombus", "Andrena", "Osmia"),
    complex = c("Bombus fervidus", "", NA_character_),
    stringsAsFactors = FALSE
  )
  out <- decorate_complex(df)
  expect_equal(out$complex, c("(Complex) Bombus fervidus", "", NA_character_))
})

test_that("decorate_complex is idempotent (no double prefix)", {
  df <- data.frame(complex = "(Complex) Bombus fervidus", stringsAsFactors = FALSE)
  expect_equal(decorate_complex(df)$complex, "(Complex) Bombus fervidus")
  # Applying twice yields the same result.
  expect_equal(decorate_complex(decorate_complex(df))$complex,
               "(Complex) Bombus fervidus")
})

test_that("decorate_complex is a no-op when there is no complex column", {
  df <- data.frame(genus = "Bombus", species = "vosnesenskii",
                   stringsAsFactors = FALSE)
  expect_identical(decorate_complex(df), df)
})

test_that("decorate_complex leaves other columns untouched", {
  df <- data.frame(
    taxon_id = c(1L, 2L),
    complex  = c("Bombus fervidus", ""),
    species  = c("californicus", "texanus"),
    stringsAsFactors = FALSE
  )
  out <- decorate_complex(df)
  expect_equal(out$taxon_id, c(1L, 2L))
  expect_equal(out$species, c("californicus", "texanus"))
})
