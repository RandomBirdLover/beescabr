# =============================================================
# Bee Occurrence Explorer -- interactive "where has this bee been found?" map
# beescabr / Cabrillo National Monument (CABR) native bees
#
# An interactive Leaflet map of EVERY georeferenced bee record (iNaturalist photos
# + museum specimens). A filter panel lets you:
#   * pick a GENUS, then a SPECIES within it (cascading dropdowns), and
#   * toggle each TRANSECT (BST / OT / TP / UPMON / off-transect) and METHOD,
# to see exactly where that taxon has turned up. Points are coloured by transect.
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
  data.frame(
    lat    = suppressWarnings(round(as.numeric(d$latitude), 5)),
    lon    = suppressWarnings(round(as.numeric(d$longitude), 5)),
    genus  = str_squish(d$genus),
    sp     = ifelse(d$taxon_rank %in% SPECIES_RANKS & !is.na(d$species) & d$species != "",
                    word(d$species, -1), ""),               # species epithet, "" if genus-only
    tran   = tr,
    year   = suppressWarnings(as.integer(substr(d$observed_on, 1, 4))),
    method = method,
    url    = if ("url" %in% names(d)) ifelse(is.na(d$url), "", d$url) else "",
    stringsAsFactors = FALSE)
}
rec <- bind_rows(prep(PATHS$inat_clean, "photo"), prep(PATHS$specimen_clean, "net")) %>%
  filter(!is.na(lat), !is.na(lon), !is.na(genus), genus != "")
message(sprintf("Georeferenced bee records: %d (photo %d, net %d) across %d genera",
                nrow(rec), sum(rec$method == "photo"), sum(rec$method == "net"),
                length(unique(rec$genus))))

# genus -> sorted species epithets present (for the cascading dropdown)
genus_species <- rec %>% filter(sp != "") %>% distinct(genus, sp) %>%
  arrange(genus, sp) %>% group_by(genus) %>% summarise(species = list(sp), .groups = "drop")
gs_list <- setNames(lapply(genus_species$species, sort), genus_species$genus)
for (g in sort(unique(rec$genus))) if (is.null(gs_list[[g]])) gs_list[[g]] <- character(0)
gs_list <- gs_list[sort(names(gs_list))]

# ---- 1b. one REPRESENTATIVE photo per genus + species (from iNaturalist) --------
# Uses each taxon's iNat "default photo". Only OPENLY-LICENSED photos are kept, and the
# photographer credit + a link to the iNat taxon page travel with each (attribution required).
# Results (incl. "no open photo") are cached to disk so re-runs don't re-hit the API.
PHOTO_CACHE <- "data/observations/cache/taxon_photos.json"
OPEN_LIC <- c("cc0", "pd", "cc-by", "cc-by-nc", "cc-by-sa", "cc-by-nd", "cc-by-nc-sa", "cc-by-nc-nd")
photos <- if (file.exists(PHOTO_CACHE)) jsonlite::fromJSON(PHOTO_CACHE, simplifyVector = FALSE) else list()
fetch_photo <- function(name, rank) {   # -> list(u, c, l) or NULL
  url <- sprintf("https://api.inaturalist.org/v1/taxa?q=%s&rank=%s&per_page=1",
                 utils::URLencode(name, reserved = TRUE), rank)
  res <- tryCatch(jsonlite::fromJSON(url, simplifyVector = FALSE), error = function(e) NULL)
  Sys.sleep(0.7)                         # be polite to the API
  if (is.null(res) || length(res$results) == 0) return(NULL)
  t <- res$results[[1]]; p <- t$default_photo
  if (tolower(t$name) != tolower(name)) return(NULL)                 # exact-match only
  if (is.null(p) || is.null(p$license_code) || !(p$license_code %in% OPEN_LIC)) return(NULL)
  list(u = p$medium_url, c = p$attribution %||% "iNaturalist", l = sprintf("https://www.inaturalist.org/taxa/%s", t$id))
}
`%||%` <- function(a, b) if (is.null(a) || is.na(a) || a == "") b else a
needed <- unique(c(names(gs_list),
                   unlist(lapply(names(gs_list), function(g)
                     if (length(gs_list[[g]])) paste(g, gs_list[[g]])), use.names = FALSE)))
to_fetch <- needed[!needed %in% names(photos)]
if (length(to_fetch)) message(sprintf("Fetching %d taxon photos from iNaturalist (cached after; ~%.0fs)...",
                                       length(to_fetch), length(to_fetch) * 0.9))
for (nm in to_fetch) {
  ph <- fetch_photo(nm, if (grepl(" ", nm)) "species" else "genus")
  photos[[nm]] <- if (is.null(ph)) list(none = TRUE) else ph        # cache negatives too
}
dir.create(dirname(PHOTO_CACHE), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(photos, PHOTO_CACHE, auto_unbox = TRUE)
photos_ok <- photos[!vapply(photos, function(x) isTRUE(x$none), logical(1))]
message(sprintf("Representative photos: %d of %d taxa have an openly-licensed image", length(photos_ok), length(needed)))

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

# ---- 3. assemble the point payload + colour map --------------------------------
COLS <- c(BEE_TRANSECT[TRANSECTS], "off-transect" = unname(BEE_INK$muted))
pts <- jsonlite::toJSON(rec[, c("lat","lon","genus","sp","tran","year","method","url")],
                        dataframe = "values", na = "null", auto_unbox = TRUE)   # compact array-of-arrays
KEYS <- jsonlite::toJSON(c("lat","lon","genus","sp","tran","year","method","url"))

# ---- 4. write the self-contained HTML ------------------------------------------
html <- sprintf('<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Cabrillo National Monument &mdash; Bee Occurrence Explorer</title>
<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"/>
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<style>
  html,body{margin:0;height:100%%}#map{position:absolute;inset:0}
  .panel{font:13px -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;
    background:#fff;border-radius:10px;box-shadow:0 1px 2px rgba(20,50,26,.12),0 8px 24px rgba(20,50,26,.14);
    padding:12px 14px;max-width:290px}
  .panel .eyebrow{font-size:9.5px;font-weight:700;text-transform:uppercase;letter-spacing:.11em;color:%s;margin-bottom:2px}
  .panel h1{font-size:15px;font-weight:700;letter-spacing:-.01em;margin:0 0 3px;color:%s}
  .panel p.sub{font-size:11.5px;color:#6b6a66;margin:0 0 10px;line-height:1.35}
  .panel label{display:block;font-size:10.5px;font-weight:700;text-transform:uppercase;letter-spacing:.05em;color:%s;margin:8px 0 3px}
  .panel select{width:100%%;padding:5px 7px;border:1px solid #d4e6d2;border-radius:6px;font-size:12.5px;background:#fff;color:#22211e}
  .chk{display:flex;flex-wrap:wrap;gap:5px 10px;margin-top:4px}
  .chk label{display:inline-flex;align-items:center;gap:4px;text-transform:none;letter-spacing:normal;font-weight:600;font-size:11.5px;margin:0;color:#333;cursor:pointer}
  .chk .dot{width:10px;height:10px;border-radius:50%%;display:inline-block;border:1px solid #fff;box-shadow:0 0 0 1px rgba(0,0,0,.15)}
  .count{margin-top:9px;font-size:11px;color:#6b6a66}
  .leaflet-popup-content{font:12.5px -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Arial,sans-serif}
  .leaflet-popup-content i{color:#111}
  .lg{line-height:1.5}
  .beebadge{background:none!important;border:none!important}
  .badge{width:22px;height:22px;border-radius:50%%;border:1.5px solid #fff;box-shadow:0 0 2px rgba(0,0,0,.45);display:flex;align-items:center;justify-content:center;font-size:12px;line-height:1}
</style></head><body><div id="map"></div>
<script>
var COLS=%s, KEYS=%s, DATA=%s, GS=%s, LABELS=%s, PHOTOS=%s;
var BOUNDARY=%s, TRANSECTS=%s;
var TR_ORDER=["BST","OT","TP","UPMON","off-transect"];
var map=L.map("map",{preferCanvas:true,zoomControl:false});
var topo=L.tileLayer("https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",{attribution:"Tiles &copy; Esri"}).addTo(map);
var sat=L.tileLayer("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",{attribution:"Tiles &copy; Esri"});
var street=L.tileLayer("https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",{attribution:"&copy; OpenStreetMap &copy; CARTO"});
L.control.zoom({position:"topright"}).addTo(map);
L.control.layers({"Topographic":topo,"Satellite":sat,"Street":street},null,{position:"topright"}).addTo(map);
// park boundary + transect lines (context)
if(BOUNDARY){L.geoJSON(BOUNDARY,{style:{color:"#fff",weight:3,opacity:.95,fill:false}}).addTo(map);}
if(TRANSECTS){L.geoJSON(TRANSECTS,{style:function(f){var t=(f.properties.Name||f.properties.transect||"").toUpperCase();if(t.indexOf("TP")===0)t="TP";return {color:COLS[t]||"#888",weight:4,opacity:.9};}}).addTo(map);}
LABELS.forEach(function(l){L.marker([l.lat,l.lon],{icon:L.divIcon({className:"",html:"<div style=\\"font-weight:700;font-size:11px;background:rgba(255,255,255,.85);padding:1px 5px;border-radius:3px\\">"+l.transect+"</div>",iconSize:null})}).addTo(map);});
// build records from the compact array
var K={}; KEYS.forEach(function(k,i){K[k]=i;});
var recs=DATA.map(function(r){return {lat:r[K.lat],lon:r[K.lon],g:r[K.genus],s:r[K.sp],t:r[K.tran],y:r[K.year],m:r[K.method],u:r[K.url]};});
var layer=L.layerGroup().addTo(map);
function esc(x){return (""+x).replace(/&/g,"&amp;").replace(/</g,"&lt;");}
function draw(){
  layer.clearLayers();
  var g=selG.value, s=selS.value;
  var tOn={}; TR_ORDER.forEach(function(t){var c=document.getElementById("t_"+t); tOn[t]=c?c.checked:true;});
  var mOn={photo:document.getElementById("m_photo").checked, net:document.getElementById("m_net").checked};
  var n=0;
  recs.forEach(function(r){
    if(g!=="*"&&r.g!==g) return;
    if(s!=="*"&&r.s!==s) return;
    if(!tOn[r.t]) return;
    if(!mOn[r.m]) return;
    n++;
    var name = r.s? "<i>"+esc(r.g)+" "+esc(r.s)+"</i>" : "<i>"+esc(r.g)+"</i> <span style=\\"color:#888\\">(genus only)</span>";
    var pop = name+"<br>Transect: <b>"+esc(r.t)+"</b> &middot; "+(r.m==="net"?"specimen":"photo")+(r.y?" &middot; "+r.y:"")+
              (r.u?"<br><a href=\\""+esc(r.u)+"\\" target=\\"_blank\\">View on iNaturalist &rarr;</a>":"");
    var col=COLS[r.t]||"#888", em=(r.m==="net")?"\uD83D\uDD2C":"\uD83D\uDCF7";   // microscope = specimen, camera = photo
    var ic=L.divIcon({className:"beebadge",iconSize:[22,22],iconAnchor:[11,11],popupAnchor:[0,-9],
      html:"<div class=badge style=\\"background:"+col+"\\">"+em+"</div>"});
    L.marker([r.lat,r.lon],{icon:ic}).bindPopup(pop).addTo(layer);
  });
  document.getElementById("count").textContent = n.toLocaleString()+" record"+(n===1?"":"s")+" shown";
}
// ---- filter panel ----
var panel=L.control({position:"topleft"});
panel.onAdd=function(){
  var d=L.DomUtil.create("div","panel"); L.DomEvent.disableClickPropagation(d); L.DomEvent.disableScrollPropagation(d);
  var genera=Object.keys(GS).sort();
  var gopt="<option value=\\"*\\">All genera</option>"+genera.map(function(g){return "<option>"+esc(g)+"</option>";}).join("");
  var trChk=TR_ORDER.map(function(t){var lbl=t==="off-transect"?"off":t;return "<label><input type=checkbox id=t_"+t+" checked><span class=dot style=\\"background:"+(COLS[t]||"#888")+"\\"></span>"+lbl+"</label>";}).join("");
  d.innerHTML=
    "<div class=eyebrow>Cabrillo National Monument</div>"+
    "<h1>Bee Occurrence Explorer</h1>"+
    "<p class=sub>Pick a genus (then a species) and toggle transects to see where each bee has been recorded.</p>"+
    "<label>Genus</label><select id=selG>"+gopt+"</select>"+
    "<label>Species</label><select id=selS><option value=\\"*\\">All species</option></select>"+
    "<div id=photowrap style=\\"display:none;margin-top:10px\\"><img id=taxphoto style=\\"width:100%%;border-radius:7px;display:block\\" alt=\\"\\"><div id=taxcredit style=\\"font-size:9px;color:#8a8880;margin-top:3px;line-height:1.3\\"></div></div>"+
    "<label>Transect</label><div class=chk id=trchk>"+trChk+"</div>"+
    "<label>Record type</label><div class=chk><label><input type=checkbox id=m_net checked>&#128300; specimen</label><label><input type=checkbox id=m_photo checked>&#128247; iNaturalist</label></div>"+
    "<div class=count id=count></div>";
  return d;
};
panel.addTo(map);
var selG=document.getElementById("selG"), selS=document.getElementById("selS");
function fillSpecies(){
  var g=selG.value, opts="<option value=\\"*\\">All species</option>";
  if(g!=="*"&&GS[g]){opts+=GS[g].map(function(s){return "<option>"+esc(s)+"</option>";}).join("");}
  selS.innerHTML=opts;
}
function showPhoto(){
  var g=selG.value, s=selS.value, key=(g!=="*"&&s!=="*")?g+" "+s:(g!=="*"?g:null);
  var w=document.getElementById("photowrap");
  if(key&&PHOTOS[key]){var p=PHOTOS[key];
    document.getElementById("taxphoto").src=p.u;
    document.getElementById("taxcredit").innerHTML="Photo: "+esc(p.c)+" &middot; <a href=\\""+p.l+"\\" target=_blank>iNaturalist</a>";
    w.style.display="block";}
  else{w.style.display="none";}
}
selG.addEventListener("change",function(){fillSpecies();showPhoto();draw();});
selS.addEventListener("change",function(){showPhoto();draw();});
document.getElementById("trchk").addEventListener("change",draw);
document.getElementById("m_photo").addEventListener("change",draw);
document.getElementById("m_net").addEventListener("change",draw);
// fit to the data + first draw
var lat0=recs.map(function(r){return r.lat;}), lon0=recs.map(function(r){return r.lon;});
map.fitBounds([[Math.min.apply(null,lat0),Math.min.apply(null,lon0)],[Math.max.apply(null,lat0),Math.max.apply(null,lon0)]],{padding:[20,20]});
draw();
</script></body></html>',
  BEE_HTML_GREEN[["mid"]], BEE_HTML_GREEN[["deep"]], BEE_HTML_GREEN[["deep"]],
  jsonlite::toJSON(as.list(COLS), auto_unbox = TRUE), KEYS, pts,
  jsonlite::toJSON(gs_list, auto_unbox = FALSE),
  if (!is.null(tran_lab)) jsonlite::toJSON(tran_lab, dataframe = "rows", auto_unbox = TRUE) else "[]",
  jsonlite::toJSON(photos_ok, auto_unbox = TRUE),
  to_geojson(park_bnd), to_geojson(tran_ln))

out <- file.path(OUT_DIR, "bee_occurrence_explorer.html")
writeLines(html, out)
message("Wrote ", normalizePath(out), sprintf("  (%s)", format(structure(file.size(out), class = "object_size"), units = "KB")))
