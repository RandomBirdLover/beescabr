# spatial_utils.R
# install.packages("sf")
#
# Purpose: load, reproject, and clean the spatial boundary layers used
# throughout the pipeline (CABR, Point Loma, SD County), and generate
# transect buffers. Sourced by native_bee_data_analysis.Rmd.
#
# Project working CRS: EPSG:26946 (NAD83 / California zone 6, meters)
# All boundary layers below are reprojected to this CRS on load, since
# none of their source files arrive in it natively.
#
# ============================================================
# Boundary layer provenance
# ============================================================
# cabr_boundary      : NPS Land Resources Division tract/boundary data,
#                       UNIT_CODE == "CABR". Source CRS: NAD83 geographic
#                       (degrees). Single dissolved polygon, ~160 acres,
#                       matches NPS-published CABR acreage.
#                       File lives in boundaries/cabr/.
#
# cabr_survey_box     : CUSTOM hand-drawn rectangle (2026-06-22), built
#                       in ArcGIS Pro to extend past cabr_boundary on
#                       the north, south, and southeast -- generous on
#                       every side, not a uniform/formula-based buffer.
#                       This is the actual CABR-tier inclusion geometry
#                       for spatial joins (NOT cabr_boundary itself).
#                       Verified via st_contains() below to fully
#                       contain cabr_boundary. File lives in
#                       boundaries/cabr_survey_box/.
#
# point_loma_boundary : CUSTOM hand-drawn boundary (2026-06-22), built
#                       in ArcGIS Pro from the City of San Diego's
#                       "PENINSULA" community plan district, manually
#                       extended to fully cover cabr_boundary (an
#                       earlier automated Union+Dissolve attempt at
#                       this actually made the gap worse, not better --
#                       see RESOLVED note below). Not sourced directly
#                       from a single official dataset -- treat as a
#                       project-specific working boundary, not an
#                       authoritative city/NPS polygon. File lives in
#                       boundaries/point_loma/.
#
#                       RESOLVED 2026-06-22: containment vs.
#                       cabr_boundary went 4.69 acre gap (original
#                       PENINSULA) -> 5.69 acre gap (failed automated
#                       Union+Dissolve) -> 0.0105 acre gap (careful
#                       manual edit) -> fully resolved with a 5m
#                       buffer (genuine seam/precision artifact at
#                       that point, not a real missing area). Verified
#                       via st_contains() below on every run.
#
# sd_county_boundary  : CUSTOM hand-drawn boundary (2026-06-22), built
#                       in ArcGIS Pro from the County of San Diego open
#                       data portal export, expanded by hand. Not the
#                       raw county export as originally pulled -- treat
#                       as a project-specific working boundary,
#                       not an authoritative county polygon. File lives
#                       in boundaries/san_diego_county/.
#
# ============================================================
# Known issue: CABR survey area vs. official NPS boundary
# ============================================================
# Per Taro (2026-06-22): the BST transect begins on Navy-owned land
# south of the official CABR (NPS) boundary, but this area has
# historically been surveyed as part of CABR and should be counted as
# such. Decision: do NOT exclude anything that falls within Taro's
# understanding of CABR's true survey footprint, regardless of
# official ownership.
#
# Implementation: cabr_survey_box is a hand-drawn polygon (see
# provenance section above) used as the actual inclusion boundary for
# CABR-tier spatial joins -- generous on every side, not just south.
# cabr_boundary (the official NPS polygon) is layered on top purely as
# a provenance label -- every point gets classified as
# inside_nps_boundary == TRUE/FALSE, but this label never excludes a
# point from being counted as CABR. This is intentionally a label,
# not a filter.
#
# point_loma_boundary (custom-drawn) fully contains this CABR survey
# area as of 2026-06-22, confirmed via st_contains() below -- see the
# RESOLVED note in the provenance section above for the fix history.

library(sf)

# ------------------------------------------------------------
# Config
# ------------------------------------------------------------
PROJECT_CRS <- 26946  # NAD83 / California zone 6 (meters)
boundary_dir <- "data/spatial/boundaries"

# ------------------------------------------------------------
# Load + reproject: CABR boundary (NPS, authoritative)
# ------------------------------------------------------------
cabr_boundary <- st_read(
  file.path(boundary_dir, "cabr", "cabr_boundary.shp"),
  quiet = TRUE
) |>
  st_transform(PROJECT_CRS)

stopifnot(nrow(cabr_boundary) == 1)
stopifnot(cabr_boundary$UNIT_CODE[1] == "CABR")

cabr_area_acres <- as.numeric(st_area(cabr_boundary)) / 4046.8564224
message(sprintf("cabr_boundary loaded: %.1f acres (NPS-published figure: ~160 acres)", cabr_area_acres))

# ------------------------------------------------------------
# CABR survey box: hand-drawn in ArcGIS Pro (2026-06-22), extending
# past cabr_boundary on the north, south, and southeast to comfortably
# catch obscured/noisy iNat coordinates and the Navy-administered area
# where BST begins. This is the actual CABR-tier inclusion geometry --
# NOT cabr_boundary. Replaces an earlier formula-based south-only
# bounding box, which couldn't capture the irregular (north + SE)
# extension Taro wanted. File lives in boundaries/cabr_survey_box/.
#
# Being inside this box does NOT automatically mean "CABR" -- every
# point still gets classified (inside_nps_boundary, Navy-but-CABR, or
# actually something else/Point Loma) downstream. The box's job is to
# be generous enough that nothing relevant gets excluded before that
# classification happens, not to be a precise boundary itself.
# ------------------------------------------------------------
cabr_survey_box <- st_read(
  file.path(boundary_dir, "cabr_survey_box", "cabr_survey_box.shp"),
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
# Custom hand-drawn boundary (2026-06-22), manually extended in
# ArcGIS over two gap areas (a northern tongue and a southern/
# BST-transect-spur area of CABR not covered by the original
# "PENINSULA" district). Lives in boundaries/point_loma/.
#
# History: an earlier Union+Dissolve attempt made the gap WORSE
# (5.69 acres vs. the original 4.69), but a subsequent careful manual
# edit brought it down to ~0.0105 acres (~460 sq ft) -- a genuine
# seam/precision artifact at this point, not a real missing area.
# A small buffer is the appropriate fix for a gap this size.
# ------------------------------------------------------------
point_loma_boundary <- st_read(
  file.path(boundary_dir, "point_loma", "point_loma_boundary.shp"),
  quiet = TRUE
) |>
  st_transform(PROJECT_CRS)

stopifnot(nrow(point_loma_boundary) == 1)

POINT_LOMA_SEAM_BUFFER_M <- 5
point_loma_boundary <- st_buffer(point_loma_boundary, dist = POINT_LOMA_SEAM_BUFFER_M)

# Sanity check: point_loma_boundary should fully contain cabr_boundary
contains_cabr <- st_contains(point_loma_boundary, cabr_boundary, sparse = FALSE)[1, 1]
if (!contains_cabr) {
  warning("point_loma_boundary does NOT fully contain cabr_boundary, even after ",
          POINT_LOMA_SEAM_BUFFER_M, "m buffer -- re-check DIAGNOSTIC_cabr_gap.shp.")

  # Diagnostic: compute exactly which part of CABR falls outside Point Loma
  cabr_gap <- st_difference(cabr_boundary, st_union(point_loma_boundary))
  gap_area_acres <- as.numeric(st_area(cabr_gap)) / 4046.8564224
  message(sprintf(
    "DIAGNOSTIC: %.4f acres of cabr_boundary fall OUTSIDE point_loma_boundary.",
    gap_area_acres
  ))

  # Write out just the gap polygon so it can be loaded in ArcGIS/QGIS to see exactly where it is
  st_write(
    cabr_gap,
    file.path(boundary_dir, "DIAGNOSTIC_cabr_gap.shp"),
    delete_layer = TRUE,
    quiet = TRUE
  )
  message("DIAGNOSTIC: gap polygon written to boundaries/DIAGNOSTIC_cabr_gap.shp -- load this in ArcGIS to see exactly where it is.")
} else {
  message(sprintf("point_loma_boundary fully contains cabr_boundary (with %dm seam buffer applied).",
                   POINT_LOMA_SEAM_BUFFER_M))
}

# ------------------------------------------------------------
# Load + reproject: SD County boundary
# Custom hand-drawn boundary (2026-06-22). Lives in its own
# subfolder per project convention: boundaries/san_diego_county/
# ------------------------------------------------------------
sd_county_boundary <- st_read(
  file.path(boundary_dir, "san_diego_county", "sd_county_boundary.shp"),
  quiet = TRUE
) |>
  st_transform(PROJECT_CRS)

stopifnot(nrow(sd_county_boundary) == 1)

# ------------------------------------------------------------
# Transect buffers (existing functionality)
# ------------------------------------------------------------
buffer_dist_m <- 10  # change this one line to adjust buffer width

transects <- st_read(
  file.path("data/spatial/transects", "Bee_Transects.shp"),
  quiet = TRUE
) |>
  st_transform(PROJECT_CRS)

buffer_10m <- st_buffer(transects, dist = buffer_dist_m)

# buffer_10m is generated in-memory only -- per project convention,
# do not write this (or any derived buffer) to disk.

message("spatial_utils.R: boundaries and buffer_10m ready in environment.")
