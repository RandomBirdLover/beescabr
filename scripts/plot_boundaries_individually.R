# plot_boundaries_individually.R
# Plots each boundary layer on its own, one map per layer, rather than
# all overlaid together (see check_boundaries.R for the combined view).
# Assumes the objects below already exist in your environment (e.g.
# from running spatial_utils.R / check_boundaries.R earlier).

library(sf)
library(ggplot2)

plot_one <- function(shp, name, fill = "steelblue", color = "black") {
  if (is.null(shp)) {
    cat(sprintf("[SKIP] %s -- not found in environment\n", name))
    return(invisible(NULL))
  }
  p <- ggplot() +
    geom_sf(data = shp, fill = fill, color = color, alpha = 0.5, linewidth = 0.8) +
    labs(title = name, caption = "EPSG:26946") +
    theme_minimal()
  print(p)
}

plot_one(sd_county_boundary,  "SD County Boundary",   fill = "grey70",     color = "grey30")
plot_one(point_loma_boundary, "Point Loma Boundary",  fill = "steelblue",  color = "navy")
plot_one(cabr_survey_box,     "CABR Survey Box",      fill = NA,           color = "darkorange")
plot_one(cabr_boundary,       "CABR Boundary (NPS)",  fill = "forestgreen",color = "forestgreen")
plot_one(cabr_nps_tracts,     "CABR NPS Tracts",      fill = NA,           color = "purple")
plot_one(transects,           "Transects",            fill = NA,           color = "red")
