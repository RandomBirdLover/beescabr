# check_boundaries.R
# Standalone diagnostic: load all four project boundary shapefiles,
# reproject to the project CRS using sf::st_transform() (NOT manual
# coordinate math -- see spatial_utils.R header notes on why), and
# plot them together so containment/overlap can be checked visually.
#
# Run this from the beescabr project root in RStudio.
# Expects the same folder layout as spatial_utils.R:
#   data/spatial/boundaries/cabr/cabr_boundary.shp
#   data/spatial/boundaries/cabr/cabr_survey_box.shp
#   data/spatial/boundaries/point_loma/point_loma_boundary.shp
#   data/spatial/boundaries/san_diego_county/sd_county_boundary.shp

library(sf)
library(ggplot2)

PROJECT_CRS <- 26946
boundary_dir <- "data/spatial/boundaries"

# ------------------------------------------------------------
# Load + reproject all four boundaries. Each load is wrapped so one
# missing/corrupt shapefile doesn't kill the whole script -- you'll
# get a clear message about exactly which one failed and why.
# ------------------------------------------------------------
safe_load <- function(label, path) {
  if (!file.exists(path)) {
    cat(sprintf("[MISSING] %s -- file not found at %s\n", label, path))
    return(NULL)
  }
  tryCatch({
    shp <- st_read(path, quiet = TRUE) |> st_transform(PROJECT_CRS)
    cat(sprintf("[OK] %s -- %d feature(s), loaded and reprojected to EPSG:%d\n",
                label, nrow(shp), PROJECT_CRS))
    shp
  }, error = function(e) {
    cat(sprintf("[ERROR] %s -- failed to load: %s\n", label, conditionMessage(e)))
    NULL
  })
}

cabr_boundary      <- safe_load("cabr_boundary",      file.path(boundary_dir, "cabr", "cabr_boundary.shp"))
cabr_survey_box    <- safe_load("cabr_survey_box",    file.path(boundary_dir, "cabr", "cabr_survey_box.shp"))
point_loma_boundary<- safe_load("point_loma_boundary",file.path(boundary_dir, "point_loma", "point_loma_boundary.shp"))
sd_county_boundary <- safe_load("sd_county_boundary", file.path(boundary_dir, "san_diego_county", "sd_county_boundary.shp"))

# cabr_nps_tracts: NOT currently referenced in spatial_utils.R or README --
# role in the pipeline is undocumented as of this script. Loaded here
# for visual reference only. If this turns out to matter for the
# pipeline, it should get a provenance entry in spatial_utils.R's
# header comments like the other four boundary layers have.
cabr_nps_tracts <- safe_load("cabr_nps_tracts", file.path(boundary_dir, "cabr", "cabr_nps_tracts.shp"))

# Transects: lines, not polygons -- loaded the same way sf handles it
# automatically based on the shapefile's own geometry type.
transects <- safe_load("transects", file.path("data/spatial/transects", "Bee_Transects.shp"))

# 10m buffer around transects, same buffer_dist_m used in spatial_utils.R
transects_buffer_10m <- NULL
if (!is.null(transects)) {
  transects_buffer_10m <- st_buffer(transects, dist = 10)
}

# Apply the same 1m noise buffer spatial_utils.R applies to
# sd_county_boundary (SD_COUNTY_NOISE_BUFFER_M), so this plot/check
# matches what the actual pipeline uses for containment checks.
#
# UPDATED 2026-06-24: this used to be a 5m buffer on point_loma_boundary
# instead -- that approach was dropped entirely (point_loma_boundary is
# now a clean, unmodified, re-downloaded file with no buffer applied).
# The buffer moved to sd_county_boundary, and shrank to 1m, to absorb
# topological noise left by that layer's Union+Dissolve construction.
# See README "Spatial analysis" section for the full history.
if (!is.null(sd_county_boundary)) {
  sd_county_boundary <- st_buffer(sd_county_boundary, dist = 1)
}

# ------------------------------------------------------------
# Plot whatever loaded successfully. Layer order: county (largest,
# bottom) -> Point Loma -> CABR survey box -> CABR boundary (smallest,
# top, most detail) so nothing gets visually buried.
# ------------------------------------------------------------
p <- ggplot()

if (!is.null(sd_county_boundary)) {
  p <- p + geom_sf(data = sd_county_boundary, fill = "grey85", color = "grey50", linewidth = 0.4)
}
if (!is.null(point_loma_boundary)) {
  p <- p + geom_sf(data = point_loma_boundary, fill = NA, color = "steelblue", linewidth = 0.8)
}
if (!is.null(cabr_survey_box)) {
  p <- p + geom_sf(data = cabr_survey_box, fill = NA, color = "darkorange", linewidth = 0.8, linetype = "dashed")
}
if (!is.null(cabr_boundary)) {
  p <- p + geom_sf(data = cabr_boundary, fill = "forestgreen", alpha = 0.4, color = "forestgreen", linewidth = 0.8)
}
if (!is.null(cabr_nps_tracts)) {
  p <- p + geom_sf(data = cabr_nps_tracts, fill = NA, color = "purple", linewidth = 0.5, linetype = "dotted")
}
if (!is.null(transects_buffer_10m)) {
  p <- p + geom_sf(data = transects_buffer_10m, fill = "red", alpha = 0.3, color = NA)
}
if (!is.null(transects)) {
  p <- p + geom_sf(data = transects, color = "red", linewidth = 0.6)
}

p <- p +
  labs(
    title = "beescabr boundary + transect layers — visual check",
    subtitle = "grey = SD County (+1m noise buffer) | blue = Point Loma (unmodified) | orange dashed = CABR survey box |\ngreen = CABR boundary (NPS) | purple dotted = cabr_nps_tracts (undocumented role) | red = transects (+10m buffer)",
    caption = paste("Reprojected to EPSG:", PROJECT_CRS, "via sf::st_transform()")
  ) +
  theme_minimal()

print(p)

# Optional: zoom to just the CABR/Point Loma area instead of the whole
# county, since that's where the interesting containment relationships
# are. Uncomment and adjust if the full-county view is too zoomed out
# to see CABR/Point Loma detail clearly:
#
# cabr_bbox <- st_bbox(cabr_survey_box)
# pad <- 2000  # meters
# p + coord_sf(
#   xlim = c(cabr_bbox["xmin"] - pad, cabr_bbox["xmax"] + pad),
#   ylim = c(cabr_bbox["ymin"] - pad, cabr_bbox["ymax"] + pad)
# )

# ------------------------------------------------------------
# Re-run the same containment checks spatial_utils.R does, as a
# numeric cross-check alongside the visual one.
# ------------------------------------------------------------
cat("\n--- Containment checks (numeric, same logic as spatial_utils.R) ---\n")

if (!is.null(cabr_survey_box) && !is.null(cabr_boundary)) {
  ok <- st_contains(cabr_survey_box, cabr_boundary, sparse = FALSE)[1, 1]
  cat("cabr_survey_box contains cabr_boundary:", ok, "(expected: TRUE)\n")
}

if (!is.null(point_loma_boundary) && !is.null(cabr_boundary)) {
  ok <- st_contains(point_loma_boundary, cabr_boundary, sparse = FALSE)[1, 1]
  cat("point_loma_boundary contains cabr_boundary:", ok,
      "(expected: FALSE -- known coastal discrepancy, not a bug; see README)\n")
}

if (!is.null(sd_county_boundary) && !is.null(point_loma_boundary)) {
  ok <- st_contains(sd_county_boundary, point_loma_boundary, sparse = FALSE)[1, 1]
  cat("sd_county_boundary contains point_loma_boundary:", ok,
      "(expected: TRUE -- true by construction, sd_county_boundary was unioned from this layer; see README)\n")
}

if (!is.null(cabr_survey_box) && !is.null(transects)) {
  ok <- st_contains(cabr_survey_box, transects, sparse = FALSE)
  n_outside <- sum(!ok)
  if (n_outside == 0) {
    cat("cabr_survey_box contains ALL transect lines: TRUE\n")
  } else {
    cat(sprintf("cabr_survey_box does NOT contain all transects: %d of %d transect feature(s) fall partially/fully outside the box.\n",
                n_outside, length(ok)))
    cat("This matters operationally -- the survey box is supposed to be generous enough that no real transect falls outside it.\n")
  }
}
