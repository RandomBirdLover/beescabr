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
#
# point_loma_boundary : City of San Diego Community Plan district
#                       CPNAME == "PENINSULA" (CPCODE 30). Source CRS:
#                       NAD83 State Plane CA Zone VI (US feet).
#                       NOTE: renamed "Point Loma" for this project's
#                       purposes -- this is NOT a literal match, just
#                       this project's naming convention.
#
#                       Verified in ArcGIS (2026-06-22): clicking the
#                       area immediately south of cabr_boundary --
#                       i.e. the Navy-adjacent land where BST begins --
#                       returns CPNAME == "PENINSULA". The city's
#                       separate "MILITARY FACILITIES" district
#                       (CPCODE 97) is located elsewhere in the city
#                       (does not overlap Point Loma/CABR), so no union
#                       is needed -- PENINSULA alone already contains
#                       CABR's full survey area, Navy portion included.
#
# sd_county_boundary  : County of San Diego open data portal boundary
#                       export. Source CRS: WGS84 geographic (degrees).
#                       Single polygon; bounding box matches the known
#                       extent of San Diego County. Note: attribute
#                       table has no NAME/COUNTY field, only a generic
#                       'tranum' tracking field -- identity confirmed
#                       by geometry/extent only, not by attribute.
#
# ============================================================
# Known issue: CABR survey area vs. official NPS boundary
# ============================================================
# Per Taro (2026-06-22): the BST transect begins on Navy-owned land
# south of the official CABR (NPS) boundary, but this area has
# historically been surveyed as part of CABR and should be counted as
# such. Decision: do NOT exclude anything south of CABR's northernmost
# border -- treat it as in-scope.
#
# Implementation: cabr_survey_box is a bounding box anchored at CABR's
# northern extent, open-ended to the south, used as the actual
# inclusion boundary for CABR-tier spatial joins. cabr_boundary (the
# official NPS polygon) is layered on top purely as a provenance label
# -- every point gets classified as inside_nps_boundary == TRUE/FALSE,
# but this label never excludes a point from being counted as CABR.
# This is intentionally a label, not a filter.
#
# point_loma_boundary ("PENINSULA") was visually verified in ArcGIS to
# already contain this CABR survey area, Navy portion included -- no
# union with another district is needed.

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
  file.path(boundary_dir, "cabr_boundary.shp"),
  quiet = TRUE
) |>
  st_transform(PROJECT_CRS)

stopifnot(nrow(cabr_boundary) == 1)
stopifnot(cabr_boundary$UNIT_CODE[1] == "CABR")

cabr_area_acres <- as.numeric(st_area(cabr_boundary)) / 4046.8564224
message(sprintf("cabr_boundary loaded: %.1f acres (NPS-published figure: ~160 acres)", cabr_area_acres))

# ------------------------------------------------------------
# CABR survey box: bounding box anchored at CABR's north edge,
# extended south to include Navy-administered survey area.
# This is the actual CABR-tier inclusion geometry -- NOT cabr_boundary.
# ------------------------------------------------------------
cabr_bbox <- st_bbox(cabr_boundary)

# Extend south by a generous margin to comfortably cover Navy land.
# Adjust SOUTH_BUFFER_M if BST or other transects still fall outside.
SOUTH_BUFFER_M <- 1000

cabr_survey_box <- st_bbox(c(
  xmin = cabr_bbox["xmin"],
  xmax = cabr_bbox["xmax"],
  ymin = cabr_bbox["ymin"] - SOUTH_BUFFER_M,
  ymax = cabr_bbox["ymax"]
), crs = st_crs(cabr_boundary)) |>
  st_as_sfc() |>
  st_as_sf()

# ------------------------------------------------------------
# Load + reproject: Point Loma boundary
# (City of San Diego Community Plan district "PENINSULA")
# ------------------------------------------------------------
community_plans <- st_read(
  file.path(boundary_dir, "Community_Plan_SD.shp"),
  quiet = TRUE
)

point_loma_boundary <- community_plans |>
  dplyr::filter(CPNAME == "PENINSULA") |>
  st_transform(PROJECT_CRS)

stopifnot(nrow(point_loma_boundary) == 1)

# Sanity check: point_loma_boundary should fully contain cabr_boundary
contains_cabr <- st_contains(point_loma_boundary, cabr_boundary, sparse = FALSE)[1, 1]
if (!contains_cabr) {
  warning("point_loma_boundary does NOT fully contain cabr_boundary -- check inputs.")
}

# Save out a standalone renamed copy for downstream use / clarity on disk
st_write(
  point_loma_boundary,
  file.path(boundary_dir, "point_loma_boundary.shp"),
  delete_layer = TRUE,
  quiet = TRUE
)

# ------------------------------------------------------------
# Load + reproject: SD County boundary
# ------------------------------------------------------------
sd_county_boundary <- st_read(
  file.path(boundary_dir, "geo_export_3ad6de55-e500-4f17-9cd1-1389bd9a06a2.shp"),
  quiet = TRUE
) |>
  st_transform(PROJECT_CRS)

stopifnot(nrow(sd_county_boundary) == 1)

# Save out a clearly-named copy
st_write(
  sd_county_boundary,
  file.path(boundary_dir, "sd_county_boundary.shp"),
  delete_layer = TRUE,
  quiet = TRUE
)

# ------------------------------------------------------------
# Transect buffers (existing functionality)
# ------------------------------------------------------------
buffer_dist_m <- 10  # change this one line to adjust buffer width

transects <- st_read(
  file.path("data/spatial/transects", "CABR_transects.shp"),
  quiet = TRUE
) |>
  st_transform(PROJECT_CRS)

buffer_10m <- st_buffer(transects, dist = buffer_dist_m)

# buffer_10m is generated in-memory only -- per project convention,
# do not write this (or any derived buffer) to disk.

message("spatial_utils.R: boundaries and buffer_10m ready in environment.")
