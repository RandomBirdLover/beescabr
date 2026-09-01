# iNaturalist taxon-page links for the field guides: a small logo per species
# linking to the bee's photo gallery on iNaturalist.

src("analysis/inat_taxon_links.R")

test_that("inat_taxon_url links straight to the taxon page when the id is known", {
  expect_equal(inat_taxon_url(335697, "Habropoda depressa"),
               "https://www.inaturalist.org/taxa/335697")
})

test_that("inat_taxon_url falls back to a name search when the id is missing", {
  u <- inat_taxon_url(NA, "Habropoda depressa")
  expect_equal(u, "https://www.inaturalist.org/taxa/search?q=Habropoda%20depressa")
  expect_equal(inat_taxon_url(NULL, "Bombus"),
               "https://www.inaturalist.org/taxa/search?q=Bombus")
})

test_that("inat_photo_link renders the logo inside a new-tab link with a helpful title", {
  h <- inat_photo_link(335697, "Habropoda depressa")
  expect_true(grepl('href="https://www.inaturalist.org/taxa/335697"', h, fixed = TRUE))
  expect_true(grepl('class="inat"', h, fixed = TRUE))
  expect_true(grepl('target="_blank"', h, fixed = TRUE))
  expect_true(grepl('rel="noopener"', h, fixed = TRUE))
  expect_true(grepl('alt="iNaturalist"', h, fixed = TRUE))
  expect_true(grepl("See photos of Habropoda depressa on iNaturalist", h, fixed = TRUE))
  expect_true(grepl(INAT_LOGO_URL, h, fixed = TRUE))
})

test_that("inat_photo_link works without an id via the search fallback", {
  h <- inat_photo_link(NA, "Perdita rhois")
  expect_true(grepl("taxa/search?q=Perdita%20rhois", h, fixed = TRUE))
})

test_that("the logo is self-contained and popup-safe (data URI, inline size)", {
  h <- inat_photo_link(335697, "Habropoda depressa")
  expect_true(grepl('src="data:image/png;base64,', h, fixed = TRUE))   # no hotlink: works offline, nothing external
  expect_true(grepl("height:13px", h, fixed = TRUE))                    # inline size: renders in Leaflet popups with no page CSS
  expect_false(grepl("static.inaturalist.org", h, fixed = TRUE))
})

# ---- picking the taxon_id to carry through ----------------------------------
# Records already carry taxon_id. Scripts used to throw it away and later re-derive
# it by matching an assembled display name against the lookup, which failed for names
# the lookup spells differently (Anthophora urbana clementina, Biastes, Trachusa).
# bee_taxon_id() picks the right id from the records themselves.

test_that("the species-level id wins over a subspecies id in the same group", {
  # a species row pools its subspecies records; the row IS the species, so 57681
  expect_equal(bee_taxon_id(c(1127014L, 57681L, 1127014L),
                            c("subspecies", "species", "subspecies")), 57681L)
})

test_that("with only subspecies records it still returns one rather than nothing", {
  expect_equal(bee_taxon_id(c(1127014L, 1127014L), c("subspecies", "subspecies")), 1127014L)
})

test_that("the most common id wins when several share a rank", {
  expect_equal(bee_taxon_id(c(5L, 9L, 9L), c("species", "species", "species")), 9L)
})

test_that("missing ids are ignored, and all-missing gives NA", {
  expect_equal(bee_taxon_id(c(NA, 7L, NA), c("species", "species", "species")), 7L)
  expect_true(is.na(bee_taxon_id(c(NA, NA), c("species", "species"))))
  expect_true(is.na(bee_taxon_id(integer(0), character(0))))
})

test_that("a carried id produces a DIRECT taxon link, not a name search", {
  h <- inat_photo_link(bee_taxon_id(57681L, "species"), "Anthophora urbana")
  expect_true(grepl("/taxa/57681", h, fixed = TRUE))
  expect_false(grepl("taxa/search", h, fixed = TRUE))
})
