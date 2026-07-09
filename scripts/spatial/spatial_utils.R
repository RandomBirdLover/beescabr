# spatial_utils.R
# install.packages("sf")
#
# Purpose: load, reproject, and clean the spatial boundary layers used
# throughout the pipeline (CABR, Point Loma, SD County), and generate
# transect buffers. Sourced by native_bee_data_analysis.Rmd.
#
# Project working CRS: EPSG:26946 (NAD83 / California zone 6, meters)
# All boundary layers below are reprojected to this CRS on load. As of
# 2026-06-24, the underlying shapefiles on disk are ALSO already saved
# natively in EPSG:26946 (confirmed in ArcGIS Pro Layer Properties for
# every file below) -- the st_transform() calls are kept as a defensive
# no-op in case that ever changes, not because they're doing real work
# right now.
#
# ============================================================
# Boundary layer provenance
# ============================================================
# cabr_boundary       : NPS Land Resources Division tract/boundary data,
#                       UNIT_CODE == "CABR". Single dissolved polygon,
#                       ~160 acres, matches NPS-published CABR acreage.
#                       Unmodified authoritative source. File lives in
#                       boundaries/cabr/.
#
# cabr_survey_box     : CUSTOM hand-drawn rectangle (2026-06-22), built
#                       in ArcGIS Pro to extend past cabr_boundary on
#                       the north, south, and southeast -- generous on
#                       every side, not a uniform/formula-based buffer.
#                       This is the actual CABR-tier inclusion geometry
#                       for spatial joins (NOT cabr_boundary itself).
#                       Verified via st_contains() below to fully
#                       contain cabr_boundary. File lives in
#                       boundaries/cabr/ (alongside cabr_boundary).
#
# point_loma_boundary : City of San Diego Community Plan district
#                       "PENINSULA" (CPCODE 30), re-downloaded fresh
#                       from the City's open data portal on 2026-06-24.
#                       Unmodified authoritative source -- this REPLACES
#                       an earlier hand-edited + 5m-buffered version
#                       used in prior sessions (see superseded note
#                       below). File lives in boundaries/point_loma/.
#
#                       SUPERSEDED 2026-06-22 approach: that version
#                       hand-edited the city's PENINSULA polygon and
#                       added a 5m seam buffer to force full containment
#                       of cabr_boundary. As of 2026-06-24 we no longer
#                       do this -- see "Known issue" section below for
#                       why, and for the current, non-destructive
#                       handling of the same underlying discrepancy.
#
# sd_county_boundary  : NOTE -- this is NOT the raw County of San Diego
#                       boundary on its own. As of 2026-06-24 this file
#                       is a DISSOLVED UNION of the original SD County
#                       boundary + point_loma_boundary, built in ArcGIS
#                       Pro (Union, then Dissolve with no fields
#                       selected, producing a single coverage polygon).
#                       The original county-only polygon was not
#                       preserved separately. A 1m buffer is applied on
#                       load (see load section below) to absorb ~0.0001
#                       acres of topological noise left by the dissolve
#                       -- not a real gap, confirmed via
#                       diagnose_county_gap.R.
#
#                       Practical effect: because point_loma_boundary
#                       is now literally unioned into sd_county_boundary,
#                       "point_loma_boundary completely within
#                       sd_county_boundary" is true by construction, not
#                       a fact about two independently-sourced
#                       boundaries. Keep this in mind if this check is
#                       ever used to validate something else -- it
#                       won't catch a real future county/Point Loma
#                       mismatch the way it would have when
#                       sd_county_boundary was the raw county file.
#                       File lives in boundaries/san_diego_county/.
#
# ============================================================
# Known issue: CABR boundary extends beyond Point Loma / SD County
# ============================================================
# Confirmed 2026-06-24 in ArcGIS Pro (Select By Location, "Completely
# within", all layers standardized to EPSG:26946 first):
#
#   point_loma_boundary WITHIN sd_county_boundary  -> PASS
#     (true by construction -- see provenance note above)
#   cabr_boundary       WITHIN point_loma_boundary -> FAIL
#   cabr_boundary       WITHIN sd_county_boundary  -> FAIL
#
# Both failures trace to the same cause: cabr_boundary (NPS authoritative
# source) extends slightly into the water/coastline beyond where
# point_loma_boundary and sd_county_boundary (City/County authoritative
# sources) draw the coastline. This is treated as an EXPECTED feature of
# independently-digitized boundary data, not an error.
#
# Decision (2026-06-24): do NOT edit point_loma_boundary or
# sd_county_boundary to force containment of cabr_boundary, and do NOT
# clip cabr_boundary to fit inside them. All three are kept as
# unmodified authoritative sources (sd_county_boundary's Point-Loma-union
# status aside -- see provenance note). If acreage totals across
# CABR/Point Loma/County tiers don't reconcile exactly, this coastal
# discrepancy is the expected explanation, not a data error.
#
# This supersedes the 2026-06-22 approach of hand-editing
# point_loma_boundary + applying a 5m seam buffer to force containment.
# The three checks below are informational (message(), not warning())
# because failing is the known, correct state for two of them.
#
# Separately, per Taro (2026-06-22): the BST transect begins on
# Navy-owned land south of the official CABR (NPS) boundary, but this
# area has historically been surveyed as part of CABR and should be
# counted as such. cabr_survey_box (see provenance above) is the actual
# CABR-tier inclusion geometry for this reason -- generous on every
# side, not just south. cabr_boundary is layered on top purely as a
# provenance label (inside_nps_boundary TRUE/FALSE), never as a filter.

library(sf)

# ------------------------------------------------------------
# Config
# ------------------------------------------------------------
PROJECT_CRS <- 26946  # NAD83 / California zone 6 (meters)
boundary_dir <- "data/spatial/boundaries"
ACRES_PER_SQM <- 1 / 4046.8564224

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
message(sprintf("cabr_boundary loaded: %.1f acres (NPS-published figure: ~160 acres)", cabr_area_acres))

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
  message("cabr_survey_box fully contains cabr_boundary.")
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
# 1m buffer applied below: diagnose_county_gap.R confirmed the dissolved
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
  message(sprintf(
    "%s completely within %s: %s",
    inner_label, outer_label, ifelse(result, "PASS", "FAIL (expected -- see Known issue notes)")
  ))
  result
}

message("\n--- Boundary containment checks ---")
check_containment(point_loma_boundary, "point_loma_boundary", sd_county_boundary, "sd_county_boundary")
check_containment(cabr_boundary, "cabr_boundary", point_loma_boundary, "point_loma_boundary")
check_containment(cabr_boundary, "cabr_boundary", sd_county_boundary, "sd_county_boundary")

# ------------------------------------------------------------
# Transect buffers (existing functionality)
# ------------------------------------------------------------
buffer_dist_m <- 10  # change this one line to adjust buffer width

transects <- st_read(
  file.path("data/spatial/transects", "cabr_bee_transects.shp"),
  quiet = TRUE
) |>
  st_transform(PROJECT_CRS)

buffer_10m <- st_buffer(transects, dist = buffer_dist_m)

# buffer_10m is generated in-memory only -- per project convention,
# do not write this (or any derived buffer) to disk.

message("\nspatial_utils.R: boundaries and buffer_10m ready in environment.")
