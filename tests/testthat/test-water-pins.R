# Observations whose GPS lands in the OCEAN (a known beeple/intern data problem).
# They join the same per-surveyor "pins to fix" worklist as other bad pins, with
# their own reason so the surveyor knows what went wrong.
#
# Tolerance matters: cabr_boundary (NPS) is digitized slightly seaward of the City
# coastline, so a legitimate pin right at the shore must NOT flag. Only pins clearly
# out in the water do.

skip_if_not_installed("sf")
src("inat_observations/inat_bee_clean.R")

# a simple square of "land" in metres-friendly WGS84 near Point Loma
land_square <- function() {
  sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(-117.245, 32.665), c(-117.240, 32.665),
    c(-117.240, 32.670), c(-117.245, 32.670), c(-117.245, 32.665)))), crs = 4326))
}

test_that("a pin well inside the land polygon is not flagged", {
  f <- ibc_water_flag(lat = 32.6675, lon = -117.2425, land = land_square())
  expect_false(f)
})

test_that("a pin far out in the water IS flagged", {
  f <- ibc_water_flag(lat = 32.6675, lon = -117.2600, land = land_square())  # ~1.5 km west
  expect_true(f)
})

test_that("a pin just past the shoreline is NOT flagged (digitization tolerance)", {
  # ~20 m outside the edge: within the default tolerance, so it stays unflagged
  f <- ibc_water_flag(lat = 32.6675, lon = -117.24522, land = land_square(), tol_m = 50)
  expect_false(f)
})

test_that("tightening the tolerance does flag that same near-shore pin", {
  f <- ibc_water_flag(lat = 32.6675, lon = -117.24522, land = land_square(), tol_m = 1)
  expect_true(f)
})

test_that("missing or unusable coordinates never flag", {
  L <- land_square()
  expect_false(ibc_water_flag(NA, -117.2425, L))
  expect_false(ibc_water_flag(32.6675, NA, L))
  expect_false(ibc_water_flag(32.6675, -117.2425, land = NULL))   # no shapefile -> no claim
})

test_that("it is vectorised over many observations", {
  L <- land_square()
  f <- ibc_water_flag(lat = c(32.6675, 32.6675, NA), lon = c(-117.2425, -117.2600, -117.2425), land = L)
  expect_equal(f, c(FALSE, TRUE, FALSE))
})

# ---- the worklist itself ----------------------------------------------------

test_that("a water pin lands in the review worklist with its own reason", {
  df <- data.frame(obs_id = c("a", "b"), observer = "x", observed_on = "2026-05-01",
                   transect = "TP", taxon_id = 1L, scientific_name = "Bombus sp.",
                   latitude = c(32.6675, 32.6675), longitude = c(-117.2425, -117.2600),
                   url = "u", location_needs_fix = c(FALSE, FALSE),
                   stringsAsFactors = FALSE)
  out <- ibc_location_review(df, land = land_square())
  expect_equal(out$obs_id, "b")                                  # only the offshore one
  expect_true(grepl("water|ocean", out$fix_reason, ignore.case = TRUE))
})

test_that("a pin that is BOTH off-transect and in water reports the water reason", {
  # water is the more specific, more actionable problem, so it wins
  df <- data.frame(obs_id = "a", observer = "x", observed_on = "2026-05-01",
                   transect = "TP", taxon_id = 1L, scientific_name = "Bombus sp.",
                   latitude = 32.6675, longitude = -117.2600, url = "u",
                   location_needs_fix = TRUE, stringsAsFactors = FALSE)
  out <- ibc_location_review(df, land = land_square())
  expect_equal(nrow(out), 1L)
  expect_true(grepl("water|ocean", out$fix_reason, ignore.case = TRUE))
})

test_that("with no land layer the worklist behaves exactly as before", {
  df <- data.frame(obs_id = c("a", "b"), observer = "x", observed_on = "2026-05-01",
                   transect = "TP", taxon_id = 1L, scientific_name = "Bombus sp.",
                   latitude = 32.6675, longitude = c(-117.2425, -117.2600), url = "u",
                   location_needs_fix = c(TRUE, FALSE), stringsAsFactors = FALSE)
  out <- ibc_location_review(df, land = NULL)
  expect_equal(out$obs_id, "a")
})
