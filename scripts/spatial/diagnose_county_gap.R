# diagnose_county_gap.R
# STATUS (2026-06-24): the gap this script was built to diagnose is
# resolved -- spatial_utils.R now applies a 1m buffer to
# sd_county_boundary specifically to absorb it (see README "Spatial
# analysis" section). Running this script today should report "No gap
# detected." Kept as a VERIFICATION/regression-check tool: re-run it
# any time boundary shapefiles are re-sourced or re-downloaded, to
# confirm point_loma_boundary is still fully contained before assuming
# it.
#
# Computes exactly which part(s) of point_loma_boundary fall OUTSIDE
# sd_county_boundary, and writes that gap geometry to a shapefile so
# it can be loaded directly in ArcGIS Pro to see exactly where the
# remaining problem area(s) are.
#
# Run this AFTER spatial_utils.R (needs sd_county_boundary and
# point_loma_boundary already loaded).

library(sf)

gap <- st_difference(
  st_union(point_loma_boundary),
  st_union(sd_county_boundary)
)

if (length(gap) == 0 || all(st_is_empty(gap))) {
  cat("No gap detected -- point_loma_boundary is now fully contained.\n")
} else {
  gap_area_acres <- as.numeric(st_area(gap)) / 4046.8564224
  cat(sprintf("Point Loma area OUTSIDE sd_county_boundary: %.4f acres\n", gap_area_acres))

  # If the gap is actually several disconnected pieces (likely, given
  # "many big gaps"), break it out so you can see each one's size
  # individually rather than just one combined total.
  gap_parts <- st_cast(gap, "POLYGON")
  if (length(gap_parts) > 1) {
    cat(sprintf("Gap is made up of %d separate disconnected piece(s):\n", length(gap_parts)))
    for (i in seq_along(gap_parts)) {
      part_area <- as.numeric(st_area(gap_parts[i])) / 4046.8564224
      cat(sprintf("  Piece %d: %.4f acres\n", i, part_area))
    }
  }

  st_write(
    st_sf(geometry = gap),
    "data/spatial/boundaries/san_diego_county/DIAGNOSTIC_county_gap.shp",
    delete_layer = TRUE,
    quiet = TRUE
  )
  cat("\nWritten to data/spatial/boundaries/san_diego_county/DIAGNOSTIC_county_gap.shp\n")
  cat("Load this directly in ArcGIS Pro on top of sd_county_boundary and\n")
  cat("point_loma_boundary to see exactly where the remaining gap(s) are.\n")
}
