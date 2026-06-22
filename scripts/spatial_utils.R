# =============================================================================
# spatial_utils.R
# Generates transect buffers in memory from CABR_transects.shp
#
# DO NOT store output buffers as shapefiles in data/ — generate on the fly.
# Source this script in native_bee_data_analysis.Rmd or any script that
# needs buffers.
#
# CRS: EPSG:26946 (NAD83 / California zone 6, meters) — already projected,
#      so st_buffer dist is in meters directly.
# =============================================================================

library(sf)

# --- Parameters (change buffer size here, not downstream) ---
buffer_dist_m <- 10  # meters

# --- Load transects ---
transects <- st_read(
  "data/spatial/transects/CABR_transects.shp",
  quiet = TRUE
)

# Safety check: catch any future CRS changes that would break meter-based buffering
if (st_is_longlat(transects)) {
  stop(
    "Transects CRS is geographic (degrees), not projected (meters). ",
    "Reproject to EPSG:26946 or another meter-based CRS before buffering."
  )
}

# --- Generate buffer ---
# EPSG:26946 is in meters, so dist = 10 gives a 10m buffer
buffer_10m <- st_buffer(transects, dist = buffer_dist_m)

message(sprintf(
  "Buffer ready: %d features | %dm radius | CRS: EPSG:%s",
  nrow(buffer_10m),
  buffer_dist_m,
  st_crs(transects)$epsg
))

# buffer_10m is now available for spatial joins (see TODO #6)
# Example usage downstream:
#   obs_with_transect <- st_join(obs_sf, buffer_10m, join = st_within)
