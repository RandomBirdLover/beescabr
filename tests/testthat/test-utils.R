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

test_that("decorate_complex_name tags the scientific_name of complex ROWS only", {
  df <- data.frame(
    rank            = c("complex",            "species",           "complex",                       "genus"),
    scientific_name = c("Diadasia australis", "Diadasia australis", "",                             "Colletes"),
    complex         = c("(Complex) Diadasia australis", "(Complex) Diadasia australis",
                        "(Complex) Colletes americanus", ""),
    stringsAsFactors = FALSE)
  out <- decorate_complex_name(df)
  expect_equal(out$scientific_name[1], "(Complex) Diadasia australis")   # complex row -> tagged
  expect_equal(out$scientific_name[2], "Diadasia australis")             # species row keeps its binomial
  expect_equal(out$scientific_name[3], "(Complex) Colletes americanus")  # blank complex sci filled from the complex col
  expect_equal(out$scientific_name[4], "Colletes")                       # non-complex row untouched
})

test_that("decorate_complex_name is idempotent and a no-op without rank/complex/scientific_name", {
  df <- data.frame(rank = "complex", scientific_name = "(Complex) Bombus fervidus",
                   complex = "(Complex) Bombus fervidus", stringsAsFactors = FALSE)
  expect_equal(decorate_complex_name(df)$scientific_name, "(Complex) Bombus fervidus")
  expect_equal(decorate_complex_name(decorate_complex_name(df))$scientific_name, "(Complex) Bombus fervidus")
  bare <- data.frame(genus = "Bombus", species = "vosnesenskii", stringsAsFactors = FALSE)
  expect_identical(decorate_complex_name(bare), bare)
})
