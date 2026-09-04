# =============================================================
# analysis/transect_map.R -- clean reference map of the 4 CABR bee survey transects.
#
# A house-styled remake of the ArcGIS transect map: SAME trail colors (BEE_TRANSECT) and a
# topographic basemap, but drawn straight from the GIS shapefile so the lines are crisp, with the
# full trail names + lengths. Interactive, self-contained HTML (like the bounty maps).
#
# Run from repo root:  Rscript scripts/analysis/transect_map.R
# Depends on: sf, leaflet, htmlwidgets (+ config.R, theme_beescabr.R).
# =============================================================

# Dependencies are CHECKED here, not installed: see beescabr_require() in config.R.
if (!exists("beescabr_require")) source("scripts/config.R")
beescabr_require()
suppressPackageStartupMessages({ library(sf); library(leaflet) })
if (!exists("PATHS"))        source("scripts/config.R")
if (!exists("BEE_TRANSECT")) source("scripts/analysis/theme_beescabr.R")
if (!exists("transects_in_year")) source("scripts/analysis/transect_years.R")

# ---- 1. transects + park boundary from the shapefiles (single source: data/spatial/shapefiles/) ----
read_shp <- function(p) tryCatch(sf::st_read(p, quiet = TRUE), error = function(e) NULL)
tran <- read_shp("data/spatial/shapefiles/transects/cabr_bee_transects.shp")
park <- read_shp("data/spatial/shapefiles/boundaries/cabr/nps_official/cabr_boundary_nps_official.shp")
stopifnot(!is.null(tran))
tran$code  <- toupper(trimws(tran$Name))
tran$len_m <- round(as.numeric(sf::st_length(tran)))          # native CRS (EPSG:26946) is in metres
tran$name  <- unname(BEE_TRANSECT_NAME[tran$code])
tran$col   <- unname(BEE_TRANSECT[tran$code])
tran <- sf::st_transform(tran, 4326)
if (!is.null(park)) park <- sf::st_transform(park, 4326)

# Everything below draws ONE map. It is called twice: once for the overall map
# (every transect that has ever existed) and once for the report year, so a
# published report keeps the transects of its own season. See transect_years.R.
render_transect_map <- function(tran, OUT_DIR, span_label) {
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# midpoint of each line (project to metres, interpolate to 50%, back to WGS84) -> permanent labels
.midpt <- function(g) { g <- sf::st_line_merge(sf::st_transform(g, 3310))
  if (as.character(sf::st_geometry_type(g)) == "MULTILINESTRING") {
    p <- sf::st_cast(g, "LINESTRING"); g <- p[which.max(as.numeric(sf::st_length(p)))] }
  sf::st_coordinates(sf::st_transform(sf::st_line_interpolate(g, 0.5, normalized = TRUE), 4326))[1, 1:2] }
mm  <- t(vapply(seq_len(nrow(tran)), function(i) .midpt(sf::st_geometry(tran)[i]), numeric(2)))
lab <- data.frame(code = tran$code, lon = mm[, 1], lat = mm[, 2])

# ---- 2. house-style overlay cards (white card, NPS eyebrow, teal heads -- matches the report) ----
CARD <- sprintf("background:%s;border:1px solid %s;border-radius:12px;box-shadow:0 4px 20px rgba(20,20,20,.14);font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,sans-serif;color:%s",
                BEE_HTML[["page"]], BEE_HTML[["border"]], BEE_HTML[["ink"]])
title <- paste0('<div style="', CARD, ';padding:9px 15px;max-width:360px">',
  sprintf('<div style="font-size:9.5px;font-weight:700;text-transform:uppercase;letter-spacing:.11em;color:%s;margin-bottom:2px">Cabrillo National Monument</div>', BEE_HTML_GREEN[["mid"]]),
  sprintf('<div style="font-weight:700;font-size:15px;letter-spacing:-.01em;white-space:nowrap;color:%s">Native Bee Survey Transects</div>', BEE_HTML_GREEN[["deep"]]),
  sprintf('<div style="font-size:11.5px;color:%s;margin-top:3px;line-height:1.35">%s<br>%s m surveyed in total</div>',
          BEE_HTML[["sub"]], span_label, format(sum(tran$len_m), big.mark = ",", trim = TRUE)),
  # Brief provenance, matching the other maps: where the lines come from, and lengths
  # are measured off those lines rather than from a field odometer.
  sprintf('<div style="font-size:10px;color:%s;margin-top:6px;padding-top:6px;border-top:1px solid %s;line-height:1.35">Transect lines from the park&rsquo;s survey layer (data/spatial/shapefiles), with lengths measured along them. Source: Cabrillo National Monument.</div>',
          BEE_HTML[["sub"]], BEE_HTML[["border"]]),
  '</div>')

ord <- order(tran$name)   # legend ordered by full trail name, like the original
leg_row <- function(i) sprintf(
  '<div style="margin:6px 0;white-space:nowrap"><span style="display:inline-block;width:28px;height:4px;border-radius:2px;background:%s;vertical-align:middle;margin-right:10px"></span>%s <span style="color:%s">(%s) \u00b7 %d m</span></div>',
  tran$col[i], tran$name[i], BEE_HTML[["sub"]], tran$code[i], tran$len_m[i])
legend <- paste0('<div style="', CARD, ';padding:11px 14px;font-size:12.5px">',
  sprintf('<div style="font-weight:700;font-size:10.5px;text-transform:uppercase;letter-spacing:.05em;color:%s;margin:0 0 6px">Transects</div>', BEE_HTML_GREEN[["deep"]]),
  paste(vapply(ord, leg_row, character(1)), collapse = ""), '</div>')

north <- paste0('<div id="bx-north" style="', CARD, ';padding:5px 9px 6px;text-align:center;line-height:1.05">',
  sprintf('<div style="font-weight:700;font-size:11px;color:%s;margin-bottom:1px">N</div>', BEE_HTML_GREEN[["deep"]]),
  sprintf('<svg width="14" height="16" viewBox="0 0 14 16"><polygon points="7,0 12.5,15.5 7,11.5 1.5,15.5" fill="%s"/></svg>', BEE_INK[["primary"]]),
  '</div>')

# ---- 3. the map: topographic base (like the original) + satellite/street toggle ----
m <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
  # short one-line basemap credit (the provider default is a long source list that wraps onto the legend)
  leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topographic", options = leaflet::providerTileOptions(attribution = "Tiles &copy; Esri")) %>%
  leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite", options = leaflet::providerTileOptions(attribution = "Tiles &copy; Esri")) %>%
  leaflet::addProviderTiles("CartoDB.Positron",  group = "Street", options = leaflet::providerTileOptions(attribution = "&copy; OpenStreetMap &copy; CARTO"))
if (!is.null(park))
  m <- m %>% leaflet::addPolygons(data = park, fillColor = BEE_HTML_GREEN[["light"]], fillOpacity = 0.18,
      color = BEE_HTML_GREEN[["deep"]], weight = 1.5, opacity = 0.7, group = "park boundary")
m <- m %>%
  leaflet::addPolylines(data = tran, color = ~col, weight = 5, opacity = 0.95,
      label = ~sprintf("%s (%s) \u2014 %d m", name, code, len_m),
      highlightOptions = leaflet::highlightOptions(weight = 8, opacity = 1, bringToFront = TRUE),
      group = "transects") %>%
  leaflet::addLabelOnlyMarkers(data = lab, lng = ~lon, lat = ~lat, label = ~code,
      labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "top",
        style = list("font-weight" = "700", "background" = "rgba(255,255,255,0.85)", "padding" = "1px 5px")),
      group = "transects") %>%
  leaflet::addControl(html = north,  position = "topright") %>%
  leaflet::addControl(html = title,  position = "topleft") %>%
  leaflet::addControl(html = legend, position = "bottomleft") %>%
  leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = FALSE, maxWidth = 150)) %>%
  leaflet::addLayersControl(baseGroups = c("Topographic", "Satellite", "Street"),
      overlayGroups = c("park boundary", "transects"),
      options = leaflet::layersControlOptions(collapsed = TRUE)) %>%
  htmlwidgets::onRender(BEE_MAP_CTRLROW_JS)                 # zoom/basemap/north/scale -> one bottom-centre row
m <- htmlwidgets::prependContent(m, htmltools::tags$style(htmltools::HTML(BEE_MAP_CTRLROW_CSS)))

# ---- 4. save one self-contained HTML (like every other map in the project) ----
.sc <- requireNamespace("rmarkdown", quietly = TRUE) && isTRUE(try(rmarkdown::pandoc_available(), silent = TRUE))
htmlwidgets::saveWidget(m, file.path(normalizePath(OUT_DIR), "cabr_bee_transects_map.html"),
                        selfcontained = .sc, libdir = if (.sc) NULL else "lib",
                        title = "Cabrillo National Monument -- Native Bee Survey Transects")
if (.sc) unlink(c(file.path(normalizePath(OUT_DIR), "lib"),
                  Sys.glob(file.path(normalizePath(OUT_DIR), "*_files"))), recursive = TRUE)
message(sprintf("Wrote cabr_bee_transects_map.html to %s  (%d transects, %s m total)",
                OUT_DIR, nrow(tran), format(sum(tran$len_m), big.mark = ",", trim = TRUE)))

# ---- 5. STATIC PNG (paste into docs/slides) -- same data, clean light base via ggspatial ----
# CartoDB Voyager tiles (soft parks + water + labels, like the original) with a cartolight fallback.
if (requireNamespace("ggspatial", quietly = TRUE) && requireNamespace("prettymapr", quietly = TRUE)) {
  suppressPackageStartupMessages({ library(ggplot2); library(ggspatial) })
  tr3857  <- sf::st_transform(tran, 3857)
  pk3857  <- if (!is.null(park)) sf::st_transform(park, 3857) else NULL
  mid3857 <- sf::st_transform(sf::st_as_sf(lab, coords = c("lon", "lat"), crs = 4326), 3857)
  ord2 <- order(tran$name)                                   # legend ordered by full trail name
  tr3857$code_f <- factor(tr3857$code, levels = tran$code[ord2])
  vals  <- setNames(tran$col, tran$code)
  labs2 <- setNames(sprintf("%s (%s) · %d m", tran$name, tran$code, tran$len_m), tran$code)
  ext   <- sf::st_bbox(sf::st_buffer(if (!is.null(pk3857)) pk3857 else tr3857, 120))
  ext["ymin"] <- ext["ymin"] - 150                                                 # extend SOUTH just to the tip of Point Loma (no extra ocean)
  asp   <- as.numeric((ext["ymax"] - ext["ymin"]) / (ext["xmax"] - ext["xmin"]))   # map aspect (tall peninsula)
  Wimg  <- 7.6; Himg <- round(Wimg * asp + 0.7, 1)                                  # image sized to it (+ title band)
  # clean light base (Voyager: soft parks + water + labels, no hillshade/contour clutter) -> like the original
  BASE  <- "https://a.basemaps.cartocdn.com/rastertiles/voyager/${z}/${x}/${y}.png"
  # legend INSIDE the map, top-right (version-safe: c()-coords vs the 3.5+ "inside" API)
  leg_pos <- if (utils::packageVersion("ggplot2") >= "3.5.0")
    ggplot2::theme(legend.position = "inside", legend.position.inside = c(0.985, 0.985),
                   legend.justification.inside = c(1, 1))
  else ggplot2::theme(legend.position = c(0.985, 0.985), legend.justification = c(1, 1))

  # Basemap tiles must land on disk for annotation_map_tile() to read them back. They sit
  # beside the shapefiles in data/spatial/basemap_tiles/ so everything spatial is together.
  # Disposable: the publish step clears them first so the static map always redraws with
  # CURRENT tiles, and within one run the cache saves re-downloading per map variant.
  tile_cache <- "data/spatial/basemap_tiles"; dir.create(tile_cache, recursive = TRUE, showWarnings = FALSE)
  # The tile filenames and that URL-shaped subfolder are ggspatial's doing, not ours: it
  # looks tiles up again by that exact name, so renaming them only forces a re-download.
  # Leave a note in the folder instead, rewritten each run because publishing wipes it.
  writeLines(c(
    "What is this folder?",
    "",
    "Downloaded map background images (\"tiles\") for the STATIC transect map PNG.",
    "Written by scripts/analysis/transect_map.R via the ggspatial package.",
    "",
    "Reading the file names: 16_11424_26469.png",
    "  16     zoom level (higher = closer in)",
    "  11424  tile column (x), counted east from the far west of the world map",
    "  26469  tile row (y), counted south from the far north",
    "Together they name one square of the map. The folder name is the tile server's",
    "web address with the punctuation removed, which is how ggspatial labels a source.",
    "",
    "Safe to delete. It is rebuilt automatically the next time a map is drawn, and the",
    "publishing pipeline clears it every run so the map always uses current tiles.",
    "The interactive maps on the website do NOT use these; they load tiles live.",
    "Source: CartoDB Voyager basemap, (c) OpenStreetMap contributors."),
    file.path(tile_cache, "WHAT_THESE_FILES_ARE.txt"))  # keep basemap tiles out of the repo root
  build_map <- function(tiletype) {
    g <- ggplot() + annotation_map_tile(type = tiletype, zoomin = 0, progress = "none", cachedir = tile_cache)
    if (!is.null(pk3857)) g <- g + geom_sf(data = pk3857, fill = NA, color = BEE_HTML_GREEN[["deep"]], linewidth = 0.6)
    g + geom_sf(data = tr3857, aes(color = code_f), linewidth = 1.4, lineend = "round") +
      geom_sf_label(data = mid3857, aes(label = code), size = 2.9, fontface = "bold", fill = "white",
                    alpha = 0.82, linewidth = 0, label.padding = unit(0.14, "lines"), color = BEE_INK[["primary"]]) +
      scale_color_manual(values = vals[levels(tr3857$code_f)], labels = labs2[levels(tr3857$code_f)],
                         name = "CABR Bee Transects") +
      coord_sf(xlim = c(ext["xmin"], ext["xmax"]), ylim = c(ext["ymin"], ext["ymax"]), expand = FALSE, crs = 3857) +
      annotation_scale(location = "br", unit_category = "metric", height = unit(0.18, "cm"), text_cex = 0.7) +
      annotation_north_arrow(location = "bl", which_north = "true", height = unit(1, "cm"),
                             width = unit(0.85, "cm"), style = north_arrow_orienteering) +
      labs(title = "Native Bee Survey Transects",
           subtitle = sprintf("Cabrillo National Monument · four fixed walking transects, 2021–2026 · %s m total",
                              format(sum(tran$len_m), big.mark = ",", trim = TRUE))) +
      theme_void(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 15, color = BEE_HTML_GREEN[["deep"]]),
            plot.subtitle = element_text(size = 10, color = BEE_INK[["secondary"]], margin = margin(b = 6)),
            legend.title = element_text(face = "bold", size = 10, color = BEE_HTML_GREEN[["deep"]]),
            legend.text = element_text(size = 9),
            legend.background = element_rect(fill = "white", color = BEE_HTML[["border"]], linewidth = 0.3),
            legend.margin = margin(6, 9, 7, 9),
            plot.margin = margin(8, 10, 8, 10),
            plot.background = element_rect(fill = "white", color = NA)) +
      leg_pos
  }
  PNG <- file.path(OUT_DIR, "cabr_bee_transects_map.png"); saved <- FALSE
  for (ty in list(BASE, "cartolight")) {
    ok <- tryCatch({ ggplot2::ggsave(PNG, build_map(ty), width = Wimg, height = Himg, dpi = 200, bg = "white"); TRUE },
                   error = function(e) { message("  basemap ", if (identical(ty, BASE)) "CartoDB Voyager" else ty,
                                                 " failed (", conditionMessage(e), ")"); FALSE })
    if (ok) { saved <- TRUE; message("Wrote cabr_bee_transects_map.png (",
                                     if (identical(ty, BASE)) "CartoDB Voyager" else ty, " base) to ", OUT_DIR); break }
  }
} else message("  (ggspatial/prettymapr not available -- skipped static PNG; HTML written)")
}

# ---- 6. the two maps ------------------------------------------------------------
# OVERALL: what exists now. Lives in data/spatial/ beside the shapefile it is drawn
# from, and is free to change whenever the park adds or retires a transect.
render_transect_map(tran, "data/spatial/transect_map_generated",
                    sprintf("%d fixed walking transects, all seasons", nrow(tran)))

# THIS REPORT'S YEAR: only the transects that existed that season. OT was first
# surveyed in 2024, so a 2023 report must not show it -- an empty transect on the
# map reads as "nobody surveyed it", which is not what happened.
.yrs   <- read_transect_years()
.codes <- transects_in_year(BEESCABR_SEASON, .yrs)
.tran_y <- if (length(.codes)) tran[tran$code %in% .codes, , drop = FALSE] else tran
if (!length(.codes))
  message("  transect_map: no transect_years_manual.csv -- report map shows all transects")
render_transect_map(.tran_y, file.path(DIR_REPORT, "reference/transects"),
                    sprintf("%d transects surveyed in %d", nrow(.tran_y), BEESCABR_SEASON))
