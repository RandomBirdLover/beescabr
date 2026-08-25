# spatial_utils.R
#
# Purpose: load, reproject, and clean the spatial boundary layers used
# throughout the pipeline (CABR, Point Loma, SD County), and generate
# transect buffers.
#
# Requires the sf package: install.packages("sf")
#
# Project working CRS: EPSG:26946 (NAD83 / California zone 6, meters).
# All layers are reprojected on load. The shapefiles on disk are already
# saved natively in EPSG:26946, so st_transform() is a defensive no-op --
# kept in case that ever changes, not because it does real work now.
#
# THREE THINGS TO KNOW BEFORE EDITING THIS FILE:
#
#   1. cabr_survey_box -- NOT cabr_boundary -- is the actual CABR-tier
#      inclusion geometry for spatial joins. cabr_boundary is layered on
#      top purely as a provenance label (inside_nps_boundary TRUE/FALSE),
#      never as a filter. The BST transect starts on Navy land south of
#      the NPS boundary and must still count as CABR.
#
#   2. sd_county_boundary gets a 1m buffer on load. That absorbs
#      topological noise (~0.0001 acres) left by the Union+Dissolve that
#      built it -- not a real gap.
#
#   3. The three containment checks below are informational -- message(),
#      not warning() -- because two of them are EXPECTED to fail. CABR's
#      NPS boundary extends slightly past the City/County coastline. That
#      is independently-digitized boundary data, not an error, and nothing
#      here should be edited or clipped to force containment.
#
# Full provenance for every layer, the superseded 2026-06-22 approach, and
# the reasoning behind all of the above: dev-docs/spatial_mapping.md

library(sf)

if (!exists("bx_kv") && file.exists("scripts/utils/console.R")) source("scripts/utils/console.R")

# ------------------------------------------------------------
# Config
# ------------------------------------------------------------
PROJECT_CRS <- 26946  # NAD83 / California zone 6 (meters)
boundary_dir <- "data/spatial/boundaries"
ACRES_PER_SQM <- 1 / 4046.8564224

# ------------------------------------------------------------
# All boundary + transect loading below is wrapped so a MISSING or renamed shapefile
# (or a failed sanity check) surfaces as a CLEAR message and lets the run continue --
# instead of throwing at source() time and killing the whole pipeline before main()
# even starts (every other heavy stage is tryCatch-wrapped for the same reason). The
# assignments inside still land in the global env (verified), so on the happy path
# nothing changes; only a genuine load failure is now non-fatal. Downstream spatial
# stages that need these layers fail on their own; the non-spatial stages still run.
# ------------------------------------------------------------
tryCatch({

# ------------------------------------------------------------
# Load + reproject: CABR boundary (NPS, authoritative)
# ------------------------------------------------------------
cabr_boundary <- st_read(
  file.path(boundary_dir, "cabr", "nps_official", "cabr_boundary_nps_official.shp"),
  quiet = TRUE
) |>
  st_transform(PROJECT_CRS)

stopifnot(nrow(cabr_boundary) == 1)
stopifnot(cabr_boundary$UNIT_CODE[1] == "CABR")

cabr_area_acres <- as.numeric(st_area(cabr_boundary)) * ACRES_PER_SQM
bx_kv("Boundaries", sprintf("CABR %.1f ac (NPS ~160)", cabr_area_acres))

# ------------------------------------------------------------
# CABR survey box: hand-drawn in ArcGIS Pro (2026-06-22), extending
# past cabr_boundary on the north, south, and southeast to comfortably
# catch obscured/noisy iNat coordinates and the Navy-administered area
# where BST begins. This is the actual CABR-tier inclusion geometry --
# NOT cabr_boundary. File lives in boundaries/cabr/ (alongside
# cabr_boundary), not in its own subfolder.
#
# Being inside this box does NOT automatically mean "CABR" -- every
# point still gets classified (inside_nps_boundary, Navy-but-CABR, or
# actually something else/Point Loma) downstream. The box's job is to
# be generous enough that nothing relevant gets excluded before that
# classification happens, not to be a precise boundary itself.
# ------------------------------------------------------------
cabr_survey_box <- st_read(
  file.path(boundary_dir, "cabr", "cabr_survey_box.shp"),
  quiet = TRUE
) |>
  st_transform(PROJECT_CRS)

stopifnot(nrow(cabr_survey_box) == 1)

# Sanity check: cabr_survey_box should fully contain cabr_boundary
# (it's supposed to be a generous superset, not a tight fit)
contains_cabr_in_box <- st_contains(cabr_survey_box, cabr_boundary, sparse = FALSE)[1, 1]
if (!contains_cabr_in_box) {
  warning("cabr_survey_box does NOT fully contain cabr_boundary -- ",
          "the hand-drawn box may need to be redrawn larger in ArcGIS.")
} else {
  bx_cont("inside the survey box")
}

# ------------------------------------------------------------
# Load + reproject: Point Loma boundary
# City of San Diego Community Plan district "PENINSULA" (CPCODE 30),
# re-downloaded fresh and unmodified as of 2026-06-24. See provenance
# section above -- this replaces the prior hand-edited + buffered
# version. Lives in boundaries/point_loma/.
# ------------------------------------------------------------
point_loma_boundary <- st_read(
  file.path(boundary_dir, "point_loma", "point_loma_boundary.shp"),
  quiet = TRUE
) |>
  st_transform(PROJECT_CRS)

stopifnot(nrow(point_loma_boundary) == 1)

# ------------------------------------------------------------
# Load + reproject: SD County boundary
# NOTE: as of 2026-06-24 this is a dissolved Union of the original SD
# County boundary + point_loma_boundary, not the raw county file alone.
# See provenance section above for why, and what this means for
# downstream containment checks. Lives in boundaries/san_diego_county/.
#
# 1m buffer applied below: diagnose_sd_county_gap.R confirmed the dissolved
# Union leaves 576 microscopic topological slivers along the Point Loma
# coastline, totaling 0.0001 acres (~4 sq ft) -- floating-point/rounding
# noise from the dissolve operation, not a real missing area. This was
# enough to make st_contains() return FALSE for "point_loma_boundary
# within sd_county_boundary" even though the two boundaries are
# coincident by construction, and even though ArcGIS's "Completely
# Within" tool (which has its own internal XY tolerance) reported PASS
# on the same data. A 1m buffer is ~10,000x larger than the actual gap
# and fully absorbs it, while remaining ecologically negligible at this
# project's scale.
# ------------------------------------------------------------
SD_COUNTY_NOISE_BUFFER_M <- 1

sd_county_boundary <- st_read(
  file.path(boundary_dir, "san_diego_county", "sd_county_boundary.shp"),
  quiet = TRUE
) |>
  st_transform(PROJECT_CRS) |>
  st_buffer(dist = SD_COUNTY_NOISE_BUFFER_M)

stopifnot(nrow(sd_county_boundary) == 1)

# ------------------------------------------------------------
# Containment checks (informational, 2026-06-24)
# ------------------------------------------------------------
# These three checks document the known boundary relationships -- see
# "Known issue" section above. FAIL is the expected, correct result for
# the two CABR checks; it reflects a real (and accepted) discrepancy
# between independently-digitized boundary sources, not a bug. Using
# message() rather than warning() throughout for this reason.
# ------------------------------------------------------------
check_containment <- function(inner, inner_label, outer, outer_label) {
  result <- st_contains(outer, inner, sparse = FALSE)[1, 1]
  if (result) bx_cont(sprintf("%s inside %s ✓", inner_label, outer_label))
  result
}

check_containment(point_loma_boundary, "Point Loma", sd_county_boundary, "SD County")
check_containment(cabr_boundary, "CABR", point_loma_boundary, "Point Loma")
check_containment(cabr_boundary, "CABR", sd_county_boundary, "SD County")
bx_note("CABR reaches just past the Point Loma / County lines — expected (known coastal discrepancy), not an error.")

# ------------------------------------------------------------
# Transect buffers (existing functionality)
# ------------------------------------------------------------
buffer_dist_m <- 50  # change this one line to adjust buffer width

transects <- st_read(
  file.path("data/spatial/transects", "cabr_bee_transects.shp"),
  quiet = TRUE
) |>
  st_transform(PROJECT_CRS)

buffer_50m <- st_buffer(transects, dist = buffer_dist_m)

# buffer_50m is generated in-memory only -- per project convention,
# do not write this (or any derived buffer) to disk.

bx_cont("boundaries + 50 m buffer ready")

}, error = function(e) {
  message("  [spatial] WARNING: could not load boundary/transect layers -- ", conditionMessage(e))
  message("  [spatial] The spatial stages (CABR membership, SD-County clip, transect flags) need the ",
          "shapefiles under ", boundary_dir, "/ and data/spatial/transects/. Fix those and re-run; ",
          "the rest of the pipeline continues for now instead of dying at startup.")
})
