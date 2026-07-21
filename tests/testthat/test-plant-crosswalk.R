library(testthat)
library(dplyr)

# Plant-name variants live in master_crosswalk (what_for == "plant_taxon").
# specimen_bee_clean folds each raw flower label to the canonical name via that
# map so flower_visited is uniform with the plant obs + plant lookup. The brain
# must NOT treat a plant name as a survey tag.

src("specimens/specimen_clean.R")

.cw <- function() tibble(
  name = c("tp", "Isocoma menziesii", "Leptosyne maritima", "Madia"),
  what_for = c("transect", "plant_taxon", "plant_taxon", "plant_taxon"),
  applies_to_plant_bee_or_both = c("both", "plant", "plant", "plant"),
  specimen_label_variants = c("Tide Pool Trail; TP1",
                              "Isocoma menziesii; Isocoma menzeisii; Isocomona menziesii",
                              "Leptosyne maritima; Coreopsis maritima",
                              "Madia sp."),
  inat_tag_variants = c("TP; #TP1", "", "", "")
)

test_that("plant_variant_map maps label spellings to canonical, ignoring non-plant rows", {
  m <- plant_variant_map(.cw())
  expect_false(any(m$canonical == "tp"))                                   # transect row excluded
  expect_equal(m$canonical[m$variant == "isocoma menzeisii"], "Isocoma menziesii")
  expect_equal(m$canonical[m$variant == "isocomona menziesii"], "Isocoma menziesii")
  expect_equal(m$canonical[m$variant == "coreopsis maritima"], "Leptosyne maritima")
  expect_equal(m$canonical[m$variant == "madia sp."], "Madia")
})

test_that("plant_variant_map is empty on NULL / missing columns", {
  expect_equal(nrow(plant_variant_map(NULL)), 0L)
  expect_equal(nrow(plant_variant_map(tibble(x = 1))), 0L)
})

test_that("normalize_flower_name folds raw labels to canonical, passes unmapped through", {
  m <- plant_variant_map(.cw())
  x <- c("Isocomona menziesii", "Coreopsis maritima", "Madia sp.", "Salvia mellifera", NA)
  out <- normalize_flower_name(x, m)
  expect_equal(out[1], "Isocoma menziesii")
  expect_equal(out[2], "Leptosyne maritima")
  expect_equal(out[3], "Madia")
  expect_equal(out[4], "Salvia mellifera")   # unmapped -> unchanged
  expect_true(is.na(out[5]))
})

test_that("normalize_flower_name is a no-op with an empty map", {
  x <- c("Whatever", "Else")
  expect_equal(normalize_flower_name(x, plant_variant_map(NULL)), x)
})

test_that("fpi_build_tagmap excludes plant_taxon rows (plant names are not survey tags)", {
  src("project_info/finding_project_info.R")
  tm <- fpi_build_tagmap(.cw())
  expect_false(any(grepl("isocoma|leptosyne|madia", tm$key)))   # no plant name leaks into the tag map
  expect_true(any(tm$concept == "tp"))                           # real transect tag still there
})
