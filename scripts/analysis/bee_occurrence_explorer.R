# =============================================================
# Bee Occurrence Explorer -- interactive "where has this bee been found?" map
# beescabr / Cabrillo National Monument (CABR) native bees
#
# An interactive Leaflet map of EVERY georeferenced bee record (iNaturalist photos
# + museum specimens). A filter panel lets you:
#   * pick a GENUS, then a SPECIES within it (cascading dropdowns), and
#   * toggle each TRANSECT (BST / OT / TP / UPMON / off-transect) and METHOD,
# to see exactly where that taxon has turned up. Points are colored by transect.
#
# Built as a self-contained HTML (Leaflet from CDN + embedded point data), so it
# publishes to GitHub Pages like the bounty maps. Matches the site's green theme.
#
# Run from the repo root:  Rscript scripts/analysis/bee_occurrence_explorer.R
# Depends on: dplyr, stringr, jsonlite, sf (+ config.R, theme_beescabr.R).
# =============================================================

for (pkg in c("jsonlite", "sf")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_TRANSECT")) source("scripts/analysis/theme_beescabr.R")
OUT_DIR <- file.path(DIR_REPORT, "reference/occurrence_map")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
TRANSECTS     <- c("BST", "OT", "TP", "UPMON")
SPECIES_RANKS <- c("species", "subspecies")

# ---- 1. pool georeferenced records, normalise the fields we filter on ----------
prep <- function(f, method) {
  d <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  tr <- toupper(str_squish(as.character(d$transect)))
  tr <- ifelse(grepl("^TP", tr), "TP", tr)                 # TP1/TP2 -> TP
  tr <- ifelse(tr %in% TRANSECTS, tr, "off-transect")      # blanks / others -> off-transect
  sg <- if ("subgenus" %in% names(d)) str_squish(d$subgenus) else NA_character_
  cx <- if ("complex"  %in% names(d)) str_squish(d$complex)  else NA_character_
  cx <- sub("^\\(Complex\\)\\s*", "", ifelse(is.na(cx), "", cx))   # keep the clean species-group name
  data.frame(
    lat    = suppressWarnings(round(as.numeric(d$latitude), 5)),
    lon    = suppressWarnings(round(as.numeric(d$longitude), 5)),
    family = if ("family" %in% names(d)) str_squish(d$family) else NA_character_,
    genus  = str_squish(d$genus),
    subgenus = ifelse(is.na(sg), "", sg),                  # "" if not identified to subgenus
    complex  = cx,                                          # "" if not in a named species-complex
    sp     = ifelse(d$taxon_rank %in% SPECIES_RANKS & !is.na(d$species) & d$species != "",
                    word(d$species, -1), ""),               # species epithet, "" if genus-only
    subspecies = if ("subspecies" %in% names(d))
                   ifelse(d$taxon_rank == "subspecies" & !is.na(d$subspecies) & d$subspecies != "",
                          word(d$subspecies, -1), "") else "",   # subspecies epithet, "" otherwise
    tran   = tr,
    year   = suppressWarnings(as.integer(substr(d$observed_on, 1, 4))),
    month  = suppressWarnings(as.integer(substr(d$observed_on, 6, 7))),   # 1-12, for the phenology strip
    method = method,
    url    = if ("url" %in% names(d)) ifelse(is.na(d$url), "", d$url) else "",
    stringsAsFactors = FALSE)
}
rec <- bind_rows(prep(PATHS$inat_clean, "photo"), prep(PATHS$specimen_clean, "net")) %>%
  filter(!is.na(lat), !is.na(lon), !is.na(genus), genus != "")
message(sprintf("Georeferenced bee records: %d (photo %d, net %d) across %d genera",
                nrow(rec), sum(rec$method == "photo"), sum(rec$method == "net"),
                length(unique(rec$genus))))

# The cascading dropdowns (genus -> subgenus -> complex -> species) are derived
# CLIENT-SIDE from the point payload below, since every record already carries its
# genus/subgenus/complex/species. We still build a genus -> species map HERE, but only
# to know which taxa to fetch a representative photo for (below).
gs_tbl  <- rec %>% filter(sp != "") %>% distinct(genus, sp) %>%
  arrange(genus, sp) %>% group_by(genus) %>% summarise(species = list(sort(sp)), .groups = "drop")
gs_list <- setNames(gs_tbl$species, gs_tbl$genus)
for (g in sort(unique(rec$genus))) if (is.null(gs_list[[g]])) gs_list[[g]] <- character(0)
gs_list <- gs_list[sort(names(gs_list))]

# ---- 1b. one REPRESENTATIVE photo per genus + species (from iNaturalist) --------
# Only OPENLY-LICENSED photos are kept, and the photographer credit + explicit license
# label + a link to the iNat taxon page travel with each (attribution required). The
# taxon's default photo is tried first; if it is closed-license, the taxon's curated
# photo list is searched for an open one (see explorer_photo_helpers.R).
# Results (incl. "no open photo") are cached to disk so re-runs don't re-hit the API.
if (!exists("fetch_taxon_photo")) source("scripts/analysis/explorer_photo_helpers.R")
PHOTO_CACHE <- "data/inat_observations/cache/taxon_photos.json"
photos <- if (file.exists(PHOTO_CACHE)) jsonlite::fromJSON(PHOTO_CACHE, simplifyVector = FALSE) else list()
# Every taxon a selection can land on gets a representative photo: genus, each
# genus+species, each subgenus, and each complex. subgenus/complex cache keys are
# PREFIXED ("subg "/"cx ") so they can't collide with a genus/species of the same text.
sp_full   <- unlist(lapply(names(gs_list), function(g)
               if (length(gs_list[[g]])) paste(g, gs_list[[g]])), use.names = FALSE)
subg_vals <- sort(unique(rec$subgenus[rec$subgenus != ""]))
cx_vals   <- sort(unique(rec$complex [rec$complex  != ""]))
ssp_full  <- sort(unique(with(rec[rec$subspecies != "", ], paste(genus, sp, subspecies))))   # trinomials
taxa <- rbind(
  data.frame(key = names(gs_list),             query = names(gs_list), rank = "genus",      stringsAsFactors = FALSE),
  data.frame(key = sp_full,                     query = sp_full,        rank = "species",    stringsAsFactors = FALSE),
  data.frame(key = paste0("subg ", subg_vals),  query = subg_vals,      rank = "subgenus",   stringsAsFactors = FALSE),
  data.frame(key = paste0("cx ",   cx_vals),    query = cx_vals,        rank = "complex",    stringsAsFactors = FALSE),
  data.frame(key = ssp_full,                    query = ssp_full,       rank = "subspecies", stringsAsFactors = FALSE))
taxa <- taxa[!duplicated(taxa$key), , drop = FALSE]
to_fetch <- taxa[!taxa$key %in% names(photos), , drop = FALSE]
if (nrow(to_fetch)) message(sprintf("Fetching %d taxon photos from iNaturalist (cached after; ~%.0fs)...",
                                     nrow(to_fetch), nrow(to_fetch) * 1.6))
for (i in seq_len(nrow(to_fetch))) {
  ph <- fetch_taxon_photo(to_fetch$query[i], to_fetch$rank[i])
  photos[[to_fetch$key[i]]] <- if (is.null(ph)) list(none = TRUE) else ph   # cache negatives too
}
dir.create(dirname(PHOTO_CACHE), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(photos, PHOTO_CACHE, auto_unbox = TRUE)
photos_ok <- photos[!vapply(photos, function(x) isTRUE(x$none), logical(1))]
message(sprintf("Representative photos: %d of %d taxa have an openly-licensed image", length(photos_ok), nrow(taxa)))

# ---- 2. geometry (park boundary + transect lines) as GeoJSON -------------------
read_shp <- function(p) tryCatch(sf::st_transform(sf::st_read(p, quiet = TRUE), 4326), error = function(e) NULL)
to_geojson <- function(x) { if (is.null(x)) return("null")
  f <- tempfile(fileext = ".geojson"); suppressWarnings(sf::st_write(x, f, quiet = TRUE, delete_dsn = TRUE))
  paste(readLines(f, warn = FALSE), collapse = "") }
park_bnd <- read_shp("data/spatial/boundaries/cabr/nps_official/cabr_boundary_nps_official.shp")
tran_ln  <- read_shp("data/spatial/transects/cabr_bee_transects.shp")
tran_lab <- NULL
if (!is.null(tran_ln)) {
  mm <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(tran_ln))))
  tr <- toupper(str_squish(tran_ln$Name)); tr <- ifelse(grepl("^TP", tr), "TP", tr)
  tran_lab <- data.frame(transect = tr, lon = mm[, 1], lat = mm[, 2])
}

# ---- 3. assemble the point payload + color maps -------------------------------
COLS <- c(BEE_TRANSECT[TRANSECTS], "off-transect" = unname(BEE_INK$muted))   # transect colors (lines + filter dots)
# TAXON colors: each POINT is symbolised by its genus/species, using the SAME family-hue +
# within-family shade scheme as the bounty maps (colors match across the site).
fam_of <- function(f){ f<-str_squish(f); f[is.na(f)|f==""]<-"Other"; ifelse(f %in% names(BEE_FAMILY), f, "Other") }
shade_ramp <- function(base,k){ if (k<=1) return(base)
  lo <- grDevices::rgb(t(255-(255-grDevices::col2rgb(base))*0.40), maxColorValue=255)   # lighter tint
  hi <- grDevices::rgb(t(grDevices::col2rgb(base)*0.48),          maxColorValue=255)    # darker shade
  grDevices::colorRampPalette(c(lo, base, hi))(k) }
rec$fam   <- fam_of(rec$family)
rec$taxon <- ifelse(rec$sp != "", paste(rec$genus, rec$sp), rec$genus)
taxo <- rec %>% distinct(fam, genus, sp, taxon) %>% arrange(match(fam, BEE_FAMILY_ORDER), genus, sp)
taxo$tcol <- NA_character_
for (fm in unique(taxo$fam)) { idx <- which(taxo$fam == fm); taxo$tcol[idx] <- shade_ramp(unname(BEE_FAMILY[fm]), length(idx)) }
TCOLS_JS <- jsonlite::toJSON(as.list(setNames(taxo$tcol, taxo$taxon)), auto_unbox = TRUE)
GREY_JS  <- jsonlite::toJSON(unname(BEE_GENUS_GREY), auto_unbox = TRUE)
# family-grouped genus legend (like the bounty maps): one representative color per genus = its band midpoint
genus_leg <- taxo %>% group_by(fam, genus) %>%
  summarise(col = tcol[ceiling(dplyr::n() / 2)], .groups = "drop") %>%
  arrange(match(fam, BEE_FAMILY_ORDER), genus)
# per-genus subgenus + complex names, listed under each genus in the legend
# (complex names spelled out in full, e.g. "Diadasia australis")
sub_tbl <- rec %>% filter(subgenus != "") %>% distinct(genus, subgenus) %>% arrange(genus, subgenus)
cx_tbl  <- rec %>% filter(complex  != "") %>% distinct(genus, complex)  %>% arrange(genus, complex)
subOf <- tapply(sub_tbl$subgenus, sub_tbl$genus, paste, collapse = ", ")
cxOf  <- tapply(cx_tbl$complex,   cx_tbl$genus, paste, collapse = ", ")
LEGEND_JS <- jsonlite::toJSON(lapply(intersect(BEE_FAMILY_ORDER, genus_leg$fam), function(fm) {
  g <- genus_leg[genus_leg$fam == fm, ]
  list(family = fm, fcol = unname(BEE_FAMILY[fm]),
       genera = unname(Map(function(n, c) list(n = n, c = c,
                             sub = if (n %in% names(subOf)) unname(subOf[n]) else "",
                             cx  = if (n %in% names(cxOf))  unname(cxOf[n])  else ""),
                           g$genus, g$col)))
}), auto_unbox = TRUE)
pts <- jsonlite::toJSON(rec[, c("lat","lon","genus","subgenus","complex","sp","subspecies","tran","year","month","method","url")],
                        dataframe = "values", na = "null", auto_unbox = TRUE)   # compact array-of-arrays
KEYS <- jsonlite::toJSON(c("lat","lon","genus","subgenus","complex","sp","subspecies","tran","year","month","method","url"))

# Record type is shown by marker STYLE, not an icon: specimen = open (outline) circle,
# iNaturalist = filled circle. Both are colored by the taxon's genus/species (see below).

# ---- 4. write the self-contained HTML ------------------------------------------
html <- paste0(sprintf('<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Cabrillo National Monument &mdash; Bee Occurrence Explorer</title>
<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"/>
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<style>
  html,body{margin:0;height:100%%}#map{position:absolute;inset:0}
  .panel{font:13px -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;
    background:__C_PAGE__;border:1px solid __C_BORDER__;border-radius:12px;box-shadow:0 4px 20px rgba(20,20,20,.14);
    padding:12px 14px;width:268px;max-width:calc(100vw - 24px);box-sizing:border-box;max-height:calc(100vh - 200px);overflow-y:auto}
  .titlebox{max-height:none;overflow:visible;padding:9px 15px}
  .titlebox h1{white-space:nowrap}
  /* map controls live in one horizontal strip, bottom-left (zoom, basemap, north, scale) */
  .leaflet-bottom.leaflet-left{left:auto;right:8px;bottom:22px;transform:none;display:flex;flex-direction:row;align-items:flex-end}
  .leaflet-bottom.leaflet-left .leaflet-control{margin:0 0 0 8px;float:none;clear:none}
  /* the transect/record legend owns the top-right, tucked under the back-to-main pill */
  .leaflet-top.leaflet-right .legright{margin-top:52px}
  .genrow.ghide,.famrow.ghide{display:none}
  #taxphoto{width:100%%;max-height:min(190px,calc(100vh - 495px));object-fit:cover;border-radius:7px;display:block}
  .panel .eyebrow{font-size:9.5px;font-weight:700;text-transform:uppercase;letter-spacing:.11em;color:%s;margin-bottom:2px}
  .panel h1{font-size:15px;font-weight:700;letter-spacing:-.01em;margin:0 0 3px;color:%s}
  .panel p.sub{font-size:11.5px;color:__C_SUB__;margin:0 0 10px;line-height:1.35}
  .panel p.scope{font-size:11px;color:__C_SUB__;margin:0 0 9px;line-height:1.4;border-left:2px solid __SCOPE_GREEN__;padding-left:8px}
  .panel label{display:block;font-size:10.5px;font-weight:700;text-transform:uppercase;letter-spacing:.05em;color:%s;margin:8px 0 3px}
  .panel select{width:100%%;padding:5px 7px;border:1px solid __C_FIELD__;border-radius:6px;font-size:12.5px;background:#fff;color:__C_INK__}
  #yrlab{text-transform:none;font-weight:400;letter-spacing:0;color:__C_SUB__;margin-left:5px}
  .allyr{float:right;text-transform:none;letter-spacing:0;font-weight:600;font-size:9.5px;color:__C_MID__;cursor:pointer;text-decoration:underline}
  .rng{position:relative;height:22px;margin-top:1px}
  .rng .track{position:absolute;top:9px;left:0;right:0;height:4px;border-radius:2px;background:__C_FIELD__}
  .rng .trackfill{position:absolute;top:9px;height:4px;border-radius:2px;background:__C_MID__}
  .rng input[type=range]{position:absolute;top:0;left:0;width:100%%;height:22px;margin:0;background:none;pointer-events:none;-webkit-appearance:none;appearance:none}
  .rng input[type=range]::-webkit-slider-thumb{-webkit-appearance:none;appearance:none;width:15px;height:15px;border-radius:50%%;background:#fff;border:2px solid __C_MID__;cursor:pointer;pointer-events:auto}
  .rng input[type=range]::-moz-range-thumb{width:15px;height:15px;border:2px solid __C_MID__;border-radius:50%%;background:#fff;cursor:pointer;pointer-events:auto}
  .legrow{display:flex;align-items:center;gap:8px;font-size:11.5px;font-weight:600;color:#333;margin:4px 0}
  .legnote{font-size:9px;color:__C_CN__;font-style:italic;line-height:1.3;margin:1px 0 2px 20px}
  .lswatch{display:inline-block;width:20px;border-top:3px solid #888;flex:none}
  .cswatch{display:inline-block;width:12px;height:12px;border-radius:50%%;border:2px solid __C_SWATCH__;flex:none}
  .count{margin-top:9px;font-size:11px;color:__C_SUB__}
  .leaflet-popup-content{font:12.5px -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Arial,sans-serif}
  .leaflet-popup-content i{color:#111}
  .leg{border-top:1px solid __C_RULE__;margin-top:12px;padding-top:4px}
  .gclist{margin-bottom:2px}
  .panel.genusbox{max-height:calc(100vh - 392px);display:flex;flex-direction:column;overflow:hidden}
  /* fallback only: fit_genusbox() below measures the real space left under the panels above it */
  .panel.photobox{width:210px}
  .panel.northbox{width:auto;padding:5px 9px 6px;text-align:center;line-height:1.05}
  .panel.genusbox>*{flex:0 0 auto}
  .panel.genusbox>.gclist{flex:0 1 auto;min-height:0;overflow-y:auto}
  .panel.legright{width:auto}
  .famrow{font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:.06em;color:%s;margin:8px 0 3px;padding-left:2px}
  .genrow{display:flex;align-items:center;gap:6px;font-size:11.5px;color:#333;margin:1px 0;padding-left:2px}
  .genrow i{font-style:italic}
  .taxrow{display:none;font-size:10px;color:__C_SUB__;margin:0 0 1px 28px;line-height:1.32}
  .taxrow.cx{margin-left:46px}
  .taxrow.on{display:block}
  .taxrow b{font-weight:400;text-transform:uppercase;font-size:8px;letter-spacing:.03em;color:__C_LABEL__;margin-right:4px}
  .taxrow i{font-style:italic}
  .gdot{width:11px;height:11px;border-radius:50%%;flex:none;border:1px solid rgba(0,0,0,.15)}
  .phen{width:190px}
  .phen label{margin-top:0}
  .phbars{display:flex;align-items:flex-end;gap:2px;height:34px;margin-top:2px}
  .phbar{flex:1;background:__C_BAR__;border-radius:1px 1px 0 0;transition:height .15s}
  .phbar.pk{background:__C_MID__}
  .phmon{display:flex;gap:2px;margin-top:3px}
  .phmon span{flex:1;text-align:center;font-size:8px;color:__C_CN__;line-height:1}
</style></head><body><div id="map"></div>
<script>
var COLS=%s, KEYS=%s, DATA=%s, LABELS=%s, PHOTOS=%s, TCOLS=%s, GREY=%s, LEGEND=%s;
var BOUNDARY=%s, TRANSECTS=%s;
',
  BEE_HTML_GREEN[["mid"]], BEE_HTML_GREEN[["deep"]], BEE_HTML_GREEN[["deep"]], BEE_HTML_GREEN[["deep"]],
  jsonlite::toJSON(as.list(COLS), auto_unbox = TRUE), KEYS, pts,
  if (!is.null(tran_lab)) jsonlite::toJSON(tran_lab, dataframe = "rows", auto_unbox = TRUE) else "[]",
  jsonlite::toJSON(photos_ok, auto_unbox = TRUE), TCOLS_JS, GREY_JS, LEGEND_JS,
  to_geojson(park_bnd), to_geojson(tran_ln)),
'var TR_ORDER=["BST","OT","TP","UPMON","off-transect"];
var map=L.map("map",{preferCanvas:true,zoomControl:false});
var topo=L.tileLayer("https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",{attribution:"Tiles &copy; Esri"}).addTo(map);
var sat=L.tileLayer("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",{attribution:"Tiles &copy; Esri"});
var street=L.tileLayer("https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",{attribution:"&copy; OpenStreetMap &copy; CARTO"});
L.control.zoom({position:"bottomleft"}).addTo(map);
L.control.layers({"Topographic":topo,"Satellite":sat,"Street":street},null,{position:"bottomleft",collapsed:true}).addTo(map);
// park boundary + transect lines (context)
if(BOUNDARY){L.geoJSON(BOUNDARY,{style:{color:"#fff",weight:3,opacity:.95,fill:false}}).addTo(map);}
if(TRANSECTS){L.geoJSON(TRANSECTS,{style:function(f){var t=(f.properties.Name||f.properties.transect||"").toUpperCase();if(t.indexOf("TP")===0)t="TP";return {color:COLS[t]||"#888",weight:4,opacity:.9};}}).addTo(map);}
LABELS.forEach(function(l){L.marker([l.lat,l.lon],{icon:L.divIcon({className:"",html:"<div style=\\"font-weight:700;font-size:11px;background:rgba(255,255,255,.85);padding:1px 5px;border-radius:3px\\">"+l.transect+"</div>",iconSize:null})}).addTo(map);});
// build records from the compact array
var K={}; KEYS.forEach(function(k,i){K[k]=i;});
var recs=DATA.map(function(r){return {lat:r[K.lat],lon:r[K.lon],g:r[K.genus],sub:r[K.subgenus]||"",cx:r[K.complex]||"",s:r[K.sp],ssp:r[K.subspecies]||"",t:r[K.tran],y:r[K.year],mo:r[K.month]||0,m:r[K.method],u:r[K.url]};});
// cascade helpers: distinct, sorted, non-empty values for the current higher-level selection
function uniqSorted(a){return Array.from(new Set(a)).filter(function(x){return x!=="";}).sort();}
function subgeneraFor(g){return uniqSorted(recs.filter(function(r){return r.g===g;}).map(function(r){return r.sub;}));}
function complexesFor(g,sub){return uniqSorted(recs.filter(function(r){return r.g===g&&(sub==="*"||r.sub===sub);}).map(function(r){return r.cx;}));}
function speciesFor(g,sub,cx){return uniqSorted(recs.filter(function(r){return r.g===g&&(sub==="*"||r.sub===sub)&&(cx==="*"||r.cx===cx);}).map(function(r){return r.s;}));}
function subspeciesFor(g,sub,cx,s){return uniqSorted(recs.filter(function(r){return r.g===g&&(sub==="*"||r.sub===sub)&&(cx==="*"||r.cx===cx)&&r.s===s;}).map(function(r){return r.ssp;}));}
var yrsAll=recs.map(function(r){return r.y;}).filter(Boolean);   // year span for the range slider
var YMIN=yrsAll.length?Math.min.apply(null,yrsAll):2006, YMAX=yrsAll.length?Math.max.apply(null,yrsAll):2025;
var layer=L.layerGroup().addTo(map);
function esc(x){return (""+x).replace(/&/g,"&amp;").replace(/</g,"&lt;");}
function draw(){
  layer.clearLayers();
  var g=selG.value, sub=selSub.value, cx=selCx.value, s=selS.value, ssp=selSsp.value, n=0, mo=[0,0,0,0,0,0,0,0,0,0,0,0];
  var yLo=yrLo?+yrLo.value:YMIN, yHi=yrHi?+yrHi.value:YMAX;
  recs.forEach(function(r){
    if(g!=="*"&&r.g!==g) return;
    if(sub!=="*"&&r.sub!==sub) return;
    if(cx!=="*"&&r.cx!==cx) return;
    if(s!=="*"&&r.s!==s) return;
    if(ssp!=="*"&&r.ssp!==ssp) return;
    if(r.y&&(r.y<yLo||r.y>yHi)) return;         // year-range filter (undated records always shown)
    n++; if(r.mo>=1&&r.mo<=12) mo[r.mo-1]++;   // tally month for the phenology strip
    // popup name reflects the most specific rank the record actually reached
    var name = r.ssp ? "<i>"+esc(r.g)+" "+esc(r.s)+" "+esc(r.ssp)+"</i>"
      : r.s ? "<i>"+esc(r.g)+" "+esc(r.s)+"</i>"
      : r.cx ? "<i>"+esc(r.cx)+"</i> <span style=\\"color:#888\\">complex</span>"
      : r.sub ? "<i>"+esc(r.g)+"</i> <span style=\\"color:#888\\">(subgenus <i>"+esc(r.sub)+"</i>)</span>"
      : "<i>"+esc(r.g)+"</i> <span style=\\"color:#888\\">(genus only)</span>";
    var pop = name+"<br>"+(r.m==="net"?"specimen":"photo")+(r.y?" &middot; "+r.y:"")+
              (r.u?"<br><a href=\\""+esc(r.u)+"\\" target=\\"_blank\\">View on iNaturalist &rarr;</a>":"");
    var tx=r.s? r.g+" "+r.s : r.g, col=TCOLS[tx]||GREY;
    // record type by marker STYLE: specimen (net) = open circle, iNaturalist (photo) = filled. Color = genus/species.
    var opt=(r.m==="net")
      ? {radius:5,color:col,weight:2,opacity:1,fillColor:"#fff",fillOpacity:1}
      : {radius:5,color:col,weight:1,opacity:1,fillColor:col,fillOpacity:.9};
    L.circleMarker([r.lat,r.lon],opt).bindPopup(pop).addTo(layer);
  });
  document.getElementById("count").textContent = n.toLocaleString()+" record"+(n===1?"":"s")+" shown";
  renderPhen(mo);
}
// phenology strip: 12 month bars sized to the currently shown records (relative to the busiest month)
function renderPhen(mo){
  var el=document.getElementById("phbars"); if(!el) return;
  var mx=Math.max.apply(null,mo)||1;
  el.innerHTML=mo.map(function(v,i){
    var pk=(v===mx&&v>0)?" pk":"";
    return "<span class=\\"phbar"+pk+"\\" style=\\"height:"+(v?Math.max(Math.round(v/mx*100),6):0)+"%\\" title=\\""+["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"][i]+": "+v+"\\"></span>";
  }).join("");
}
// ---- legend pieces (built once; shared by the cards below) ----
var genera=uniqSorted(recs.map(function(r){return r.g;}));
var gopt="<option value=\\"*\\">All genera</option>"+genera.map(function(g){return "<option>"+esc(g)+"</option>";}).join("");
var trLeg=["BST","OT","TP","UPMON"].map(function(t){return "<div class=legrow><span class=lswatch style=\\"border-top-color:"+(COLS[t]||"#888")+"\\"></span>"+t+"</div>";}).join("");
var G2FAM={};                                            // genus name -> its family (for the collapse filter)
var gcol=LEGEND.map(function(f){                          // genus color key: family -> genus -> subgenus/complex
  var s="<div class=famrow data-fam="+esc(f.family)+" style=\\"border-left:3px solid "+f.fcol+"\\">"+esc(f.family)+"</div>";
  f.genera.forEach(function(g){
    G2FAM[g.n]=f.family;
    s+="<div class=genrow data-g="+esc(g.n)+"><span class=gdot style=\\"background:"+g.c+"\\"></span><i>"+esc(g.n)+"</i></div>";
    if(g.sub) s+="<div class=taxrow data-g="+esc(g.n)+"><b>subgenus</b><i>"+esc(g.sub)+"</i></div>";
    if(g.cx)  s+="<div class=\\"taxrow cx\\" data-g="+esc(g.n)+"><b>complex</b><i>"+esc(g.cx)+"</i></div>";
  });
  return s;
}).join("");
// ---- filter panel (top-left, under the title card) ----
var panel=L.control({position:"topleft"});
panel.onAdd=function(){
  var d=L.DomUtil.create("div","panel"); L.DomEvent.disableClickPropagation(d); L.DomEvent.disableScrollPropagation(d);
  d.innerHTML=
    "<label>Genus</label><select id=selG>"+gopt+"</select>"+
    "<div id=subwrap style=\\"display:none\\"><label>Subgenus</label><select id=selSub><option value=\\"*\\">All subgenera</option></select></div>"+
    "<div id=cxwrap style=\\"display:none\\"><label>Complex</label><select id=selCx><option value=\\"*\\">All complexes</option></select></div>"+
    "<label>Species</label><select id=selS><option value=\\"*\\">All species</option></select>"+
    "<div id=sspwrap style=\\"display:none\\"><label>Subspecies</label><select id=selSsp><option value=\\"*\\">All subspecies</option></select></div>"+
    "<label>Year <span id=yrlab></span><a class=allyr id=allyr>All years</a></label>"+
    "<div class=rng><div class=track></div><div class=trackfill id=yrfill></div>"+
      "<input type=range id=yrLo min="+YMIN+" max="+YMAX+" value="+YMIN+">"+
      "<input type=range id=yrHi min="+YMIN+" max="+YMAX+" value="+YMAX+"></div>"+
    "<div class=count id=count></div>";
  return d;
};
// ---- genus-colors legend (its own card on the LEFT, under the filters) ----
var genusbox=L.control({position:"topleft"});
genusbox.onAdd=function(){
  var d=L.DomUtil.create("div","panel genusbox"); L.DomEvent.disableClickPropagation(d); L.DomEvent.disableScrollPropagation(d);
  d.innerHTML="<label>Genus colors</label><div class=gclist>"+gcol+"</div>"+
    "<div class=legnote>Specimen locations may be approximate.</div>";
  return d;
};
// ---- transect + record-type legend (its own card on the RIGHT) ----
var legright=L.control({position:"topright"});
legright.onAdd=function(){
  var d=L.DomUtil.create("div","panel legright"); L.DomEvent.disableClickPropagation(d); L.DomEvent.disableScrollPropagation(d);
  d.innerHTML="<label>Transects</label>"+trLeg+
    "<label>Record type</label>"+
    "<div class=legrow><span class=cswatch style=\\"background:__C_SWATCH__\\"></span>iNaturalist (filled)</div>"+
    "<div class=legrow><span class=cswatch style=\\"background:#fff\\"></span>specimen (outline)</div>";
  return d;
};
// ---- taxon photo (top-right, under the transects legend + active months; shown only when a taxon is picked) ----
var photobox=L.control({position:"topright"});
photobox.onAdd=function(){
  var d=L.DomUtil.create("div","panel photobox"); d.id="photowrap"; d.style.display="none"; L.DomEvent.disableClickPropagation(d);
  d.innerHTML="<img id=taxphoto style=\\"width:100%%;border-radius:7px;display:block\\" alt=\\"\\"><div id=taxcredit style=\\"font-size:9px;color:__C_CN__;margin-top:3px;line-height:1.3\\"></div>";
  return d;
};
// ---- north arrow (part of the bottom control row) ----
var northbox=L.control({position:"bottomleft"});
northbox.onAdd=function(){
  var d=L.DomUtil.create("div","panel northbox"); L.DomEvent.disableClickPropagation(d);
  d.innerHTML="<div style=\\"font-weight:700;font-size:11px;color:__C_DEEP__;margin-bottom:1px\\">N</div>"+
    "<svg width=14 height=16 viewBox=\\"0 0 14 16\\"><polygon points=\\"7,0 12.5,15.5 7,11.5 1.5,15.5\\" fill=\\"__C_ARROW__\\"/></svg>";
  return d;
};
// title card (its own box above the control panel, matching the other maps)
var titlebox=L.control({position:"topleft"});
titlebox.onAdd=function(){var d=L.DomUtil.create("div","panel titlebox"); L.DomEvent.disableClickPropagation(d);
  d.innerHTML="<div class=eyebrow>Cabrillo National Monument</div><h1>Bee Occurrence Explorer</h1>"+
    "<p class=sub>Pick a genus to see where each bee has been recorded. Many bees are not identified all the way to species, so narrow by subgenus or complex when those appear.</p><!--SCOPE-->";
  return d;};
// phenology strip: month activity of whatever is currently shown; updated by draw().
// Lives TOP-RIGHT, under the transects legend -- per-taxon info grouped on the right.
var phen=L.control({position:"topright"});
phen.onAdd=function(){
  var d=L.DomUtil.create("div","panel phen"); L.DomEvent.disableClickPropagation(d); L.DomEvent.disableScrollPropagation(d);
  d.innerHTML="<label>Active months</label><div class=phbars id=phbars></div>"+
    "<div class=phmon>"+["J","F","M","A","M","J","J","A","S","O","N","D"].map(function(x){return "<span>"+x+"</span>";}).join("")+"</div>";
  return d;
};
titlebox.addTo(map);
panel.addTo(map);
genusbox.addTo(map);
northbox.addTo(map);
// top-right stack, top -> bottom: transects legend, active months, taxon photo
legright.addTo(map);
phen.addTo(map);
photobox.addTo(map);
L.control.scale({position:"bottomleft",imperial:false,maxWidth:150}).addTo(map);
var selG=document.getElementById("selG"), selSub=document.getElementById("selSub"),
    selCx=document.getElementById("selCx"), selS=document.getElementById("selS"), selSsp=document.getElementById("selSsp");
var subwrap=document.getElementById("subwrap"), cxwrap=document.getElementById("cxwrap"), sspwrap=document.getElementById("sspwrap");
function opt(v){return "<option>"+esc(v)+"</option>";}
function fillSub(){                        // subgenera of the chosen genus (dropdown hidden if none)
  var g=selG.value, subs=g==="*"?[]:subgeneraFor(g);
  selSub.innerHTML="<option value=\\"*\\">All subgenera</option>"+subs.map(opt).join("");
  subwrap.style.display=subs.length?"block":"none";
}
function fillCx(){                         // complexes within the current genus/subgenus (hidden if none)
  var g=selG.value, cxs=g==="*"?[]:complexesFor(g,selSub.value);
  selCx.innerHTML="<option value=\\"*\\">All complexes</option>"+cxs.map(opt).join("");
  cxwrap.style.display=cxs.length?"block":"none";
}
function fillSpecies(){                    // species within the current genus/subgenus/complex
  var g=selG.value, spp=g==="*"?[]:speciesFor(g,selSub.value,selCx.value);
  selS.innerHTML="<option value=\\"*\\">All species</option>"+spp.map(opt).join("");
  fillSsp();                               // species just reset -> hide/reset the subspecies box
}
function fillSsp(){                        // subspecies of the chosen species (hidden unless it has any)
  var g=selG.value, s=selS.value, sspp=(g!=="*"&&s!=="*")?subspeciesFor(g,selSub.value,selCx.value,s):[];
  selSsp.innerHTML="<option value=\\"*\\">All subspecies</option>"+sspp.map(opt).join("");
  sspwrap.style.display=sspp.length?"block":"none";
}
function pickSpecies(){                    // choosing a species back-fills its subgenus + complex above
  var g=selG.value, s=selS.value;
  if(g!=="*"&&s!=="*"){
    var mt=recs.filter(function(r){return r.g===g&&r.s===s;});
    var sg=uniqSorted(mt.map(function(r){return r.sub;}));   // the subgenus this species sits in (if unambiguous)
    var cx=uniqSorted(mt.map(function(r){return r.cx;}));    // the complex this species sits in (if any)
    if(sg.length===1){ selSub.value=sg[0]; subwrap.style.display="block"; }
    var cxs=complexesFor(g,selSub.value);                    // re-list complexes under that subgenus
    selCx.innerHTML="<option value=\\"*\\">All complexes</option>"+cxs.map(opt).join("");
    cxwrap.style.display=cxs.length?"block":"none";
    if(cx.length===1&&cxs.indexOf(cx[0])>=0){ selCx.value=cx[0]; }
  }
  fillSsp();                               // list any subspecies under this species
  showPhoto();draw();
}
function showPhoto(){
  // walk from the most specific level down to genus; show the first that has an open photo,
  // labeled with what it actually depicts (so a genus fallback is not mistaken for the species).
  var g=selG.value, sub=selSub.value, cx=selCx.value, s=selS.value, ssp=selSsp.value, w=document.getElementById("photowrap");
  var tries=[];
  if(g!=="*"&&s!=="*"&&ssp!=="*") tries.push([g+" "+s+" "+ssp, "<i>"+esc(g)+" "+esc(s)+" "+esc(ssp)+"</i>"]);
  if(g!=="*"&&s!=="*") tries.push([g+" "+s,   "<i>"+esc(g)+" "+esc(s)+"</i>"]);
  if(cx!=="*")         tries.push(["cx "+cx,   "<i>"+esc(cx)+"</i> complex"]);
  if(sub!=="*")        tries.push(["subg "+sub,"subgenus <i>"+esc(sub)+"</i>"]);
  if(g!=="*")          tries.push([g,          "<i>"+esc(g)+"</i>"]);
  var hit=null; for(var i=0;i<tries.length;i++){ if(PHOTOS[tries[i][0]]){ hit={p:PHOTOS[tries[i][0]],lab:tries[i][1]}; break; } }
  if(hit){ document.getElementById("taxphoto").src=hit.p.u;
    document.getElementById("taxcredit").innerHTML=esc(hit.p.c)+" &middot; <a href=\\""+hit.p.l+"\\" target=_blank>iNaturalist</a>";
    w.style.display="block"; }
  else w.style.display="none";
}
function legendGenus(){                                   // show every genus (grouped by family) until one is picked, then just that genus
  var g=selG.value, all=(g==="*"), fam=all?null:G2FAM[g], i, el;
  var fr=document.querySelectorAll(".famrow");
  for(i=0;i<fr.length;i++){el=fr[i];el.classList.toggle("ghide",!all&&el.getAttribute("data-fam")!==fam);}
  var gr=document.querySelectorAll(".genrow");
  for(i=0;i<gr.length;i++){el=gr[i];el.classList.toggle("ghide",!all&&el.getAttribute("data-g")!==g);}
  var tr=document.querySelectorAll(".taxrow");
  for(i=0;i<tr.length;i++){el=tr[i];el.classList.toggle("on",!all&&el.getAttribute("data-g")===g);}
}
selG.addEventListener("change",function(){fillSub();fillCx();fillSpecies();showPhoto();legendGenus();draw();});
selSub.addEventListener("change",function(){fillCx();fillSpecies();showPhoto();draw();});
selCx.addEventListener("change",function(){fillSpecies();showPhoto();draw();});
selS.addEventListener("change",pickSpecies);
selSsp.addEventListener("change",function(){showPhoto();draw();});
// ---- year range slider ----
var yrLo=document.getElementById("yrLo"), yrHi=document.getElementById("yrHi"),
    yrfill=document.getElementById("yrfill"), yrlab=document.getElementById("yrlab");
function yrShow(){                                        // update the fill bar + label; no redraw
  var lo=+yrLo.value, hi=+yrHi.value, span=(YMAX-YMIN)||1;
  yrfill.style.left =((lo-YMIN)/span*100)+"%";
  yrfill.style.right=((YMAX-hi)/span*100)+"%";
  yrlab.textContent=(lo===YMIN&&hi===YMAX)?"(all)":(lo===hi?(""+lo):(lo+"-"+hi));
}
yrLo.addEventListener("input",function(){ if(+yrLo.value> +yrHi.value) yrLo.value=yrHi.value; yrShow(); draw(); });
yrHi.addEventListener("input",function(){ if(+yrHi.value< +yrLo.value) yrHi.value=yrLo.value; yrShow(); draw(); });
document.getElementById("allyr").addEventListener("click",function(){ yrLo.value=YMIN; yrHi.value=YMAX; yrShow(); draw(); });
yrShow();
// fit to the data + first draw
var lat0=recs.map(function(r){return r.lat;}), lon0=recs.map(function(r){return r.lon;});
// open centered on the records at a fixed zoom (~300 m on the scale bar), matching the bounty maps
var cLat=(Math.min.apply(null,lat0)+Math.max.apply(null,lat0))/2,
    cLon=(Math.min.apply(null,lon0)+Math.max.apply(null,lon0))/2;
map.setView([cLat,cLon],16);
// The genus legend sits under the title and filter panels in the same corner. Its height
// used to be a hardcoded calc(100vh - 392px), which silently broke whenever the panels
// above it changed height (adding a sentence to the title panel pushed it off-screen).
// Measure the space that is actually left instead, and re-measure on resize.
function fit_genusbox(){
  var el=document.querySelector(".panel.genusbox"); if(!el) return;
  var top=el.getBoundingClientRect().top;
  el.style.maxHeight=Math.max(120, window.innerHeight - top - 18)+"px";
}
window.addEventListener("resize", fit_genusbox);
fit_genusbox();

draw();
</script></body></html>')

out <- file.path(OUT_DIR, "bee_occurrence_explorer.html")
# fill the scope marker now that the sprintf template is built (see the titlebox above)
# Colors: the CSS lives inside a sprintf template, so it cannot interpolate R values
# directly without disturbing the argument list. Every color is written as a token and
# filled here from theme_beescabr.R, so the map follows a palette change like every
# other page. Values are unchanged -- this is a sourcing fix, not a restyle.
.COLORS <- c(
  "__C_PAGE__"   = BEE_HTML[["page"]],       "__C_INK__"    = BEE_HTML[["ink"]],
  "__C_SUB__"    = BEE_HTML[["sub"]],        "__C_CN__"     = BEE_HTML[["cn"]],
  "__C_BORDER__" = BEE_HTML[["border"]],     "__C_DEEP__"   = BEE_HTML_GREEN[["deep"]],
  "__C_MID__"    = BEE_HTML_GREEN[["mid"]],  "__C_ARROW__"  = BEE_MAP_CHROME[["arrow"]],
  "__C_SWATCH__" = BEE_MAP_CHROME[["swatch_edge"]], "__C_LABEL__" = BEE_MAP_CHROME[["label"]],
  "__C_BAR__"    = BEE_MAP_CHROME[["bar"]],  "__C_FIELD__"  = BEE_MAP_CHROME[["field"]],
  "__C_RULE__"   = BEE_MAP_CHROME[["rule"]])
for (.k in names(.COLORS)) html <- gsub(.k, .COLORS[[.k]], html, fixed = TRUE)

html <- sub("__SCOPE_GREEN__", BEE_HTML_GREEN[["mid"]], html, fixed = TRUE)
html <- sub("<!--SCOPE-->", sprintf(paste0(
  "<p class=scope>All bee records, photos and netted specimens pooled, that are identified at least ",
  "to genus. A small number of records not identified that far are not mapped. Source: iNaturalist ",
  "observations and park specimen records, Cabrillo National Monument (data as of %s).</p>"),
  bee_data_asof()), html, fixed = TRUE)

writeLines(html, out)
message("Wrote ", normalizePath(out), sprintf("  (%s)", format(structure(file.size(out), class = "object_size"), units = "KB")))
