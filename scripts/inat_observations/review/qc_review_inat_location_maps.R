# =============================================================
# observations/qc_review_inat_location_maps.R
# beescabr -- builds the LOCATION-REVIEW handoff folder: one self-contained iNaturalist
# "pins to fix" map per observer, plus the two location_review CSVs and the shared
# instruction page, all under data/inat_observations/review/location/.
#
# This is the R port of the old map/build_per_user.py. The per-pin survey-log annotation
# (which transect the log says the observer walked that day) is computed HERE from
# master_per_survey_info.csv -- no external _pin_logmap.json, no hard-coded survey days.
# Because tag-only intern days (e.g. 2024-05-05) now live in the master via the intern
# seed, they annotate correctly on their own.
#
# Defines build_location_review_maps(write=TRUE). Run AFTER the clean scripts have written
# the two location_review CSVs and AFTER the brain has written the master. Self-contained
# output: leaflet.js/css + control images are inlined from scripts/inat_observations/assets/,
# so each HTML opens offline / when emailed.
#   source(...); build_location_review_maps()
# =============================================================
if (!exists("PATHS")) source("scripts/config.R")   # centralized paths (see PATHS in config.R)
suppressWarnings(suppressMessages({library(dplyr); library(readr); library(jsonlite); library(sf)}))

BLRM_DIR        <- "data/inat_observations/review/location"
BLRM_MAPS_DIR   <- file.path(BLRM_DIR, "by_surveyors")   # the per-surveyor maps live here; CSVs + instructions stay at the top of review/location/
BLRM_BEE_CSV    <- file.path(BLRM_DIR, "qc_review_inat_bee_location.csv")
BLRM_PLANT_CSV  <- file.path(BLRM_DIR, "qc_review_inat_plant_location.csv")
BLRM_ROSTER     <- PATHS$surveyor_roster
BLRM_MASTER     <- PATHS$per_survey
BLRM_TRANSECTS  <- "data/spatial/transects/cabr_bee_transects.shp"
BLRM_BOUNDARY   <- "data/spatial/boundaries/cabr/nps_official/cabr_boundary_nps_official.shp"
BLRM_ASSETS     <- "scripts/inat_observations/assets"
BLRM_TEMPLATE   <- file.path(BLRM_ASSETS, "location_map_template.html")
BLRM_INSTRUCT   <- file.path(BLRM_ASSETS, "cabr_fix_instructions.html")
BLRM_LEAFLET    <- file.path(BLRM_ASSETS, "leaflet")
BLRM_OFF_M      <- 50   # on-transect buffer radius (metres) -- matches IBC_OFF_TRANSECT_M

# ---- literal placeholder replace (value taken VERBATIM -- no regex/backref/backslash
#      processing, so inlined leaflet.js and embedded JSON survive intact) ----
.blrm_inject <- function(text, placeholder, value) {
  loc <- gregexpr(placeholder, text, fixed = TRUE)[[1]]
  if (loc[1] == -1L) return(text)
  plen <- nchar(placeholder); out <- ""; last <- 1L
  for (pos in loc) { out <- paste0(out, substr(text, last, pos - 1L), value); last <- pos + plen }
  paste0(out, substr(text, last, nchar(text)))
}

.blrm_str <- function(x) if (length(x) != 1L || is.na(x)) "" else as.character(x)

# transect label normaliser (master transects are already canonical; this just tidies)
.blrm_norm_tr <- function(v) {
  u <- toupper(trimws(gsub("^#", "", as.character(v))))
  u <- ifelse(startsWith(u, "TP"), "TP",
       ifelse(startsWith(u, "UPMON"), "UPMON",
       ifelse(startsWith(u, "BST"), "BST", u)))
  u[u %in% c("", "NA", "N/A")] <- NA_character_
  u
}

# ---- pins from a location_review CSV -> list of pin records ----
.blrm_load_pins <- function(path) {
  if (!file.exists(path)) return(list())
  d <- suppressWarnings(read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")))
  pins <- list()
  for (i in seq_len(nrow(d))) {
    lat <- suppressWarnings(as.numeric(d$latitude[i])); lon <- suppressWarnings(as.numeric(d$longitude[i]))
    if (is.na(lat) || is.na(lon)) next
    pins[[length(pins) + 1L]] <- list(
      lat = lat, lon = lon, sci = .blrm_str(d$scientific_name[i]), obs = .blrm_str(d$observer[i]),
      date = .blrm_str(d$observed_on[i]), tr = .blrm_str(d$transect[i]),
      url = .blrm_str(d$url[i]), id = .blrm_str(d$obs_id[i]))
  }
  pins
}

# ---- per-obs survey-log annotation from the master: obs_id -> list(a=|m=|o=) ----
# a: single logged transect that day  |  m: several (crew day)  |  o: date not in the log
.blrm_logmap <- function(pins, master_path) {
  out <- list()
  if (!length(pins) || !file.exists(master_path)) return(out)
  m <- suppressWarnings(read_csv(master_path, show_col_types = FALSE))
  if (!all(c("inat_username", "transects", "date") %in% names(m))) return(out)
  dch <- as.character(as.Date(m$date))
  reg <- new.env(parent = emptyenv())                     # (uname|date) -> transect set
  for (i in seq_len(nrow(m))) {
    uns <- tolower(trimws(strsplit(ifelse(is.na(m$inat_username[i]), "", m$inat_username[i]), "\\s*,\\s*")[[1]]))
    uns <- uns[nzchar(uns) & !(uns %in% c("n/a", "na"))]
    if (!length(uns)) next
    trs <- .blrm_norm_tr(strsplit(ifelse(is.na(m$transects[i]), "", m$transects[i]), "\\s*[,;]\\s*")[[1]])
    trs <- unique(trs[!is.na(trs)])
    if (!length(trs)) next
    for (u in uns) { k <- paste(u, dch[i]); reg[[k]] <- unique(c(reg[[k]], trs)) }
  }
  for (p in pins) {
    k <- paste(tolower(trimws(p$obs)), as.character(as.Date(p$date)))
    trs <- reg[[k]]
    if (is.null(trs) || !length(trs)) out[[p$id]] <- list(o = TRUE)
    else if (length(trs) == 1L)       out[[p$id]] <- list(a = unname(trs))
    else                              out[[p$id]] <- list(m = sort(unname(trs)))
  }
  out
}

.blrm_annotate <- function(p, lm) {
  e <- lm[[p$id]]; if (is.null(e)) e <- list()
  p$assigned <- if (!is.null(e$a)) e$a else ""
  p$multi    <- if (!is.null(e$m)) paste(e$m, collapse = ", ") else ""
  p$offlog   <- isTRUE(e$o)
  p
}

# ---- sf -> GeoJSON string (WGS84), via the GDAL GeoJSON driver ----
.blrm_geojson <- function(x) {
  tf <- tempfile(fileext = ".geojson")
  suppressWarnings(sf::st_write(x, tf, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE))
  txt <- paste(readLines(tf, warn = FALSE, encoding = "UTF-8"), collapse = "")
  unlink(tf); txt
}

.blrm_layers <- function() {
  tr <- suppressWarnings(sf::st_read(BLRM_TRANSECTS, quiet = TRUE))
  names(tr)[tolower(names(tr)) == "name"] <- "Name"
  tr <- sf::st_zm(tr, drop = TRUE, what = "ZM")
  transects_gj <- .blrm_geojson(sf::st_transform(tr["Name"], 4326))
  buf <- sf::st_buffer(sf::st_union(sf::st_geometry(tr)), BLRM_OFF_M)      # 50 m in the shapefile's metre CRS
  buffer_gj <- .blrm_geojson(sf::st_sf(geometry = sf::st_transform(buf, 4326)))
  boundary_gj <- "{\"type\":\"FeatureCollection\",\"features\":[]}"
  if (file.exists(BLRM_BOUNDARY)) {
    bd <- suppressWarnings(sf::st_read(BLRM_BOUNDARY, quiet = TRUE))
    bd <- sf::st_zm(bd, drop = TRUE, what = "ZM")
    boundary_gj <- .blrm_geojson(sf::st_sf(geometry = sf::st_transform(sf::st_geometry(bd), 4326)))
  }
  list(transects = transects_gj, buffer = buffer_gj, boundary = boundary_gj)
}

# ---- inline leaflet.js/css + control images (data URIs) ----
.blrm_leaflet_inline <- function() {
  css <- paste(readLines(file.path(BLRM_LEAFLET, "leaflet.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  js  <- paste(readLines(file.path(BLRM_LEAFLET, "leaflet.js"),  warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  js  <- gsub("</script>", "<\\/script>", js, fixed = TRUE)
  for (img in c("layers.png", "layers-2x.png")) {
    fp <- file.path(BLRM_LEAFLET, "images", img)
    b64 <- jsonlite::base64_enc(readBin(fp, "raw", file.info(fp)$size))
    css <- gsub(paste0("url(images/", img, ")"), paste0("url(data:image/png;base64,", b64, ")"), css, fixed = TRUE)
  }
  list(css = css, js = js)
}

# ---- one pin -> the compact record the client template consumes ----
.blrm_pin_json <- function(pins) {
  recs <- lapply(pins, function(p) list(lat = p$lat, lon = p$lon, sci = p$sci, date = p$date,
                                        tr = p$tr, url = p$url, assigned = p$assigned,
                                        multi = p$multi, offlog = p$offlog))
  if (!length(recs)) return("[]")
  jsonlite::toJSON(recs, auto_unbox = TRUE, na = "null")
}

build_location_review_maps <- function(write = TRUE) {
  if (!file.exists(BLRM_TEMPLATE)) { message("  (no map template at ", BLRM_TEMPLATE, " -- skipping maps)"); return(invisible(NULL)) }
  # Force a UTF-8 ctype for this call -- in a C locale R escapes the em-dash / arrow glyphs
  # to literal "<80>" text on write. Best-effort; restored on exit.
  .oldc <- Sys.getlocale("LC_CTYPE")
  for (.loc in c("C.UTF-8", "C.utf8", "en_US.UTF-8", "UTF-8"))
    if (suppressWarnings(Sys.setlocale("LC_CTYPE", .loc)) != "") break
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", .oldc)), add = TRUE)

  bee   <- .blrm_load_pins(BLRM_BEE_CSV)
  plant <- .blrm_load_pins(BLRM_PLANT_CSV)
  if (!length(bee) && !length(plant)) { message("  (no location-review pins -- nothing to map)"); return(invisible(character(0))) }

  lm    <- .blrm_logmap(c(bee, plant), BLRM_MASTER)
  bee   <- lapply(bee,   .blrm_annotate, lm = lm)
  plant <- lapply(plant, .blrm_annotate, lm = lm)

  names_lu <- list()
  if (file.exists(BLRM_ROSTER)) {
    r <- suppressWarnings(read_csv(BLRM_ROSTER, show_col_types = FALSE, col_types = cols(.default = "c")))
    for (i in seq_len(nrow(r))) {
      u <- trimws(.blrm_str(r$inaturalist_username[i]))
      fn <- trimws(.blrm_str(r$first_name[i]))
      if (nzchar(u) && is.null(names_lu[[u]])) names_lu[[u]] <- if (nzchar(fn)) fn else u
    }
  }

  L  <- .blrm_layers()
  lf <- .blrm_leaflet_inline()
  template <- rawToChar(readBin(BLRM_TEMPLATE, "raw", n = file.info(BLRM_TEMPLATE)$size))
  Encoding(template) <- "UTF-8"

  if (write) {
    dir.create(BLRM_DIR, recursive = TRUE, showWarnings = FALSE)
    dir.create(BLRM_MAPS_DIR, recursive = TRUE, showWarnings = FALSE)
  }

  observers <- unique(vapply(c(bee, plant), function(p) p$obs, character(1)))
  made <- list()
  for (u in observers) {
    fb <- Filter(function(p) identical(p$obs, u), bee)
    fp <- Filter(function(p) identical(p$obs, u), plant)
    n  <- length(fb) + length(fp)
    fn <- if (!is.null(names_lu[[u]])) names_lu[[u]] else u
    parts <- character(0)
    if (length(fb)) parts <- c(parts, paste(length(fb), "bee"))
    if (length(fp)) parts <- c(parts, paste(length(fp), "plant"))
    mix <- paste(parts, collapse = " + ")
    allconf <- (length(fb) + length(fp)) > 0 && all(vapply(c(fb, fp), function(p) nzchar(p$assigned), logical(1)))
    title <- sprintf("%s — %d pin%s to re-check", fn, n, if (n != 1) "s" else "")

    if (allconf) {
      subtitle <- paste0(
        "Hi ", fn, "! All <span class='n'>", n, "</span> of your flagged pins (", mix, ") sit ",
        "<span class='n'>&gt;50&nbsp;m</span> from a transect. Each pin shows the transect you logged that day — ",
        "but only you know whether you were <b>on</b> it right there or just <b>near</b> it. <b>Click each pin</b> and ",
        "decide: on the transect → drag the pin onto it (keep your tags); just nearby, not actually surveying → ",
        "remove the survey tag. See <b>How do I fix these?</b> (Toggle <b>Satellite</b> top-right for the trails.)")
      hownote <- paste0(
        "<div class='ask' style='background:#eefaf3;border-color:rgba(27,175,122,.45)'>\U0001F4CB Each pin names the ",
        "transect you logged that day — so <i>if</i> it's a move, you'll know which one. But decide <b>A or B</b> below ",
        "first: were you actually <b>on</b> the transect there, or just <b>nearby</b>? Don't assume it's always a move — ",
        "often the pin is in the right place and the survey tag simply shouldn't be on it.</div>")
    } else {
      subtitle <- paste0(
        "Hi ", fn, "! <span class='n'>", n, "</span> of your observations (", mix, ") have a pin ",
        "<span class='n'>&gt;50&nbsp;m</span> from a transect. <b>Click each pin</b> — it shows the transect from the ",
        "survey log, or flags if it needs a closer look — then fix it on iNaturalist (<b>How do I fix these?</b>). ",
        "They stay in our data; this just tidies the map.")
      hownote <- ""
    }

    data_json <- paste0('{"bee":', .blrm_pin_json(fb), ',"plant":', .blrm_pin_json(fp),
                        ',"transects":', L$transects, ',"buffer":', L$buffer, ',"boundary":', L$boundary, '}')

    html <- template
    html <- .blrm_inject(html, "@@TITLE@@", title)
    html <- .blrm_inject(html, "@@SUBTITLE@@", subtitle)
    html <- .blrm_inject(html, "@@HOWNOTE@@", hownote)
    html <- .blrm_inject(html, "__DATA__", data_json)
    html <- .blrm_inject(html, "__LFCSS__", lf$css)
    html <- .blrm_inject(html, "__LFJS__", lf$js)

    if (write)
      writeBin(charToRaw(enc2utf8(html)), file.path(BLRM_MAPS_DIR, paste0("cabr_pins_to_fix_", u, ".html")))
    made[[length(made) + 1L]] <- list(fn = fn, u = u, n = n)
  }

  if (write && file.exists(BLRM_INSTRUCT))
    file.copy(BLRM_INSTRUCT, file.path(BLRM_DIR, "cabr_fix_instructions.html"), overwrite = TRUE)

  n_bee <- length(bee); n_plant <- length(plant)
  message(sprintf("  %d bee + %d plant survey pins sit >50 m from any transect — still in the clean data, just flagged to re-check on iNaturalist.",
                  n_bee, n_plant))
  message(sprintf("  Built a 'pins to fix' map per surveyor (+ cabr_fix_instructions.html) in %s/", BLRM_DIR))
  if (length(made)) {
    message("  → SEND each surveyor their own map (with the instructions page) so they can re-check theirs — biggest lists first:")
    ord <- order(-vapply(made, function(s) s$n, numeric(1)))          # biggest lists first (Tom / Phil at the top)
    for (s in made[ord])
      message(sprintf("       %-10s @%-15s %3d pin%s  ->  by_surveyors/cabr_pins_to_fix_%s.html",
                      s$fn, s$u, s$n, if (s$n != 1) "s" else " ", s$u))
    if (interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1")
      invisible(readline("  Email each surveyor their map when you can.  [Enter] to continue: "))
  }
  invisible(vapply(made, function(s) s$u, character(1)))
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) build_location_review_maps()
