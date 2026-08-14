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
LEGEND_JS <- jsonlite::toJSON(lapply(intersect(BEE_FAMILY_ORDER, genus_leg$fam), function(fm) {
  g <- genus_leg[genus_leg$fam == fm, ]
  list(family = fm, fcol = unname(BEE_FAMILY[fm]),
       genera = unname(Map(function(n, c) list(n = n, c = c), g$genus, g$col)))
}), auto_unbox = TRUE)
pts <- jsonlite::toJSON(rec[, c("lat","lon","genus","subgenus","complex","sp","tran","year","method","url")],
                        dataframe = "values", na = "null", auto_unbox = TRUE)   # compact array-of-arrays
KEYS <- jsonlite::toJSON(c("lat","lon","genus","subgenus","complex","sp","tran","year","method","url"))

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
    background:#fff;border-radius:10px;box-shadow:0 1px 2px rgba(20,50,26,.12),0 8px 24px rgba(20,50,26,.14);
    padding:12px 14px;width:268px;max-width:calc(100vw - 24px);box-sizing:border-box}
  #taxphoto{width:100%%;max-height:190px;object-fit:cover;border-radius:7px;display:block}
  .panel .eyebrow{font-size:9.5px;font-weight:700;text-transform:uppercase;letter-spacing:.11em;color:%s;margin-bottom:2px}
  .panel h1{font-size:15px;font-weight:700;letter-spacing:-.01em;margin:0 0 3px;color:%s}
  .panel p.sub{font-size:11.5px;color:#6b6a66;margin:0 0 10px;line-height:1.35}
  .panel label{display:block;font-size:10.5px;font-weight:700;text-transform:uppercase;letter-spacing:.05em;color:%s;margin:8px 0 3px}
  .panel select{width:100%%;padding:5px 7px;border:1px solid #d4e6d2;border-radius:6px;font-size:12.5px;background:#fff;color:#22211e}
  .legrow{display:flex;align-items:center;gap:8px;font-size:11.5px;font-weight:600;color:#333;margin:4px 0}
  .lswatch{display:inline-block;width:20px;border-top:3px solid #888;flex:none}
  .cswatch{display:inline-block;width:12px;height:12px;border-radius:50%%;border:2px solid #57564f;flex:none}
  .count{margin-top:9px;font-size:11px;color:#6b6a66}
  .leaflet-popup-content{font:12.5px -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Arial,sans-serif}
  .leaflet-popup-content i{color:#111}
  .legend{width:auto;max-width:196px;max-height:52vh;overflow:auto}
  .legend .famrow{font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:.06em;color:%s;margin:8px 0 3px;padding-left:6px}
  .legend .genrow{display:flex;align-items:center;gap:6px;font-size:11.5px;color:#333;margin:1px 0;padding-left:6px}
  .legend .genrow i{font-style:italic}
  .legend .gdot{width:11px;height:11px;border-radius:50%%;flex:none;border:1px solid rgba(0,0,0,.15)}
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
L.control.zoom({position:"topright"}).addTo(map);
L.control.layers({"Topographic":topo,"Satellite":sat,"Street":street},null,{position:"topright"}).addTo(map);
// park boundary + transect lines (context)
if(BOUNDARY){L.geoJSON(BOUNDARY,{style:{color:"#fff",weight:3,opacity:.95,fill:false}}).addTo(map);}
if(TRANSECTS){L.geoJSON(TRANSECTS,{style:function(f){var t=(f.properties.Name||f.properties.transect||"").toUpperCase();if(t.indexOf("TP")===0)t="TP";return {color:COLS[t]||"#888",weight:4,opacity:.9};}}).addTo(map);}
LABELS.forEach(function(l){L.marker([l.lat,l.lon],{icon:L.divIcon({className:"",html:"<div style=\\"font-weight:700;font-size:11px;background:rgba(255,255,255,.85);padding:1px 5px;border-radius:3px\\">"+l.transect+"</div>",iconSize:null})}).addTo(map);});
// build records from the compact array
var K={}; KEYS.forEach(function(k,i){K[k]=i;});
var recs=DATA.map(function(r){return {lat:r[K.lat],lon:r[K.lon],g:r[K.genus],sub:r[K.subgenus]||"",cx:r[K.complex]||"",s:r[K.sp],t:r[K.tran],y:r[K.year],m:r[K.method],u:r[K.url]};});
// cascade helpers: distinct, sorted, non-empty values for the current higher-level selection
function uniqSorted(a){return Array.from(new Set(a)).filter(function(x){return x!=="";}).sort();}
function subgeneraFor(g){return uniqSorted(recs.filter(function(r){return r.g===g;}).map(function(r){return r.sub;}));}
function complexesFor(g,sub){return uniqSorted(recs.filter(function(r){return r.g===g&&(sub==="*"||r.sub===sub);}).map(function(r){return r.cx;}));}
function speciesFor(g,sub,cx){return uniqSorted(recs.filter(function(r){return r.g===g&&(sub==="*"||r.sub===sub)&&(cx==="*"||r.cx===cx);}).map(function(r){return r.s;}));}
var layer=L.layerGroup().addTo(map);
function esc(x){return (""+x).replace(/&/g,"&amp;").replace(/</g,"&lt;");}
function draw(){
  layer.clearLayers();
  var g=selG.value, sub=selSub.value, cx=selCx.value, s=selS.value, n=0;
  recs.forEach(function(r){
    if(g!=="*"&&r.g!==g) return;
    if(sub!=="*"&&r.sub!==sub) return;
    if(cx!=="*"&&r.cx!==cx) return;
    if(s!=="*"&&r.s!==s) return;
    n++;
    // popup name reflects the most specific rank the record actually reached
    var name = r.s ? "<i>"+esc(r.g)+" "+esc(r.s)+"</i>"
      : r.cx ? "<i>"+esc(r.cx)+"</i> <span style=\\"color:#888\\">complex</span>"
      : r.sub ? "<i>"+esc(r.g)+"</i> <span style=\\"color:#888\\">(subgenus <i>"+esc(r.sub)+"</i>)</span>"
      : "<i>"+esc(r.g)+"</i> <span style=\\"color:#888\\">(genus only)</span>";
    var pop = name+"<br>Transect: <b>"+esc(r.t)+"</b> &middot; "+(r.m==="net"?"specimen":"photo")+(r.y?" &middot; "+r.y:"")+
              (r.u?"<br><a href=\\""+esc(r.u)+"\\" target=\\"_blank\\">View on iNaturalist &rarr;</a>":"");
    var tx=r.s? r.g+" "+r.s : r.g, col=TCOLS[tx]||GREY;
    // record type by marker STYLE: specimen (net) = open circle, iNaturalist (photo) = filled. Color = genus/species.
    var opt=(r.m==="net")
      ? {radius:5,color:col,weight:2,opacity:1,fillColor:"#fff",fillOpacity:1}
      : {radius:5,color:col,weight:1,opacity:1,fillColor:col,fillOpacity:.9};
    L.circleMarker([r.lat,r.lon],opt).bindPopup(pop).addTo(layer);
  });
  document.getElementById("count").textContent = n.toLocaleString()+" record"+(n===1?"":"s")+" shown";
}
// ---- filter panel ----
var panel=L.control({position:"topleft"});
panel.onAdd=function(){
  var d=L.DomUtil.create("div","panel"); L.DomEvent.disableClickPropagation(d); L.DomEvent.disableScrollPropagation(d);
  var genera=uniqSorted(recs.map(function(r){return r.g;}));
  var gopt="<option value=\\"*\\">All genera</option>"+genera.map(function(g){return "<option>"+esc(g)+"</option>";}).join("");
  var trLeg=["BST","OT","TP","UPMON"].map(function(t){return "<div class=legrow><span class=lswatch style=\\"border-top-color:"+(COLS[t]||"#888")+"\\"></span>"+t+"</div>";}).join("");
  d.innerHTML=
    "<div class=eyebrow>Cabrillo National Monument</div>"+
    "<h1>Bee Occurrence Explorer</h1>"+
    "<p class=sub>Pick a genus to see where each bee has been recorded. Many bees are not identified all the way to species, so narrow by subgenus or complex when those appear.</p>"+
    "<label>Genus</label><select id=selG>"+gopt+"</select>"+
    "<div id=subwrap style=\\"display:none\\"><label>Subgenus</label><select id=selSub><option value=\\"*\\">All subgenera</option></select></div>"+
    "<div id=cxwrap style=\\"display:none\\"><label>Complex</label><select id=selCx><option value=\\"*\\">All complexes</option></select></div>"+
    "<label>Species</label><select id=selS><option value=\\"*\\">All species</option></select>"+
    "<div id=photowrap style=\\"display:none;margin-top:10px\\"><img id=taxphoto style=\\"width:100%%;border-radius:7px;display:block\\" alt=\\"\\"><div id=taxcredit style=\\"font-size:9px;color:#8a8880;margin-top:3px;line-height:1.3\\"></div></div>"+
    "<label>Transect lines</label>"+trLeg+
    "<label>Record type</label>"+
    "<div class=legrow><span class=cswatch style=\\"background:#57564f\\"></span>iNaturalist (filled)</div>"+
    "<div class=legrow><span class=cswatch style=\\"background:#fff\\"></span>specimen (outline)</div>"+
    "<div class=count id=count></div>";
  return d;
};
panel.addTo(map);
// family-grouped genus color legend (bottom-right, clear of the top-left filter panel)
var legend=L.control({position:"bottomright"});
legend.onAdd=function(){
  var d=L.DomUtil.create("div","panel legend"); L.DomEvent.disableScrollPropagation(d); L.DomEvent.disableClickPropagation(d);
  var h="<div class=eyebrow>Genus colors</div>";
  LEGEND.forEach(function(f){
    h+="<div class=famrow style=\\"border-left:3px solid "+f.fcol+"\\">"+esc(f.family)+"</div>";
    f.genera.forEach(function(g){ h+="<div class=genrow><span class=gdot style=\\"background:"+g.c+"\\"></span><i>"+esc(g.n)+"</i></div>"; });
  });
  d.innerHTML=h; return d;
};
legend.addTo(map);
var selG=document.getElementById("selG"), selSub=document.getElementById("selSub"),
    selCx=document.getElementById("selCx"), selS=document.getElementById("selS");
var subwrap=document.getElementById("subwrap"), cxwrap=document.getElementById("cxwrap");
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
  showPhoto();draw();
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
selG.addEventListener("change",function(){fillSub();fillCx();fillSpecies();showPhoto();draw();});
selSub.addEventListener("change",function(){fillCx();fillSpecies();showPhoto();draw();});
selCx.addEventListener("change",function(){fillSpecies();showPhoto();draw();});
selS.addEventListener("change",pickSpecies);
// fit to the data + first draw
var lat0=recs.map(function(r){return r.lat;}), lon0=recs.map(function(r){return r.lon;});
map.fitBounds([[Math.min.apply(null,lat0),Math.min.apply(null,lon0)],[Math.max.apply(null,lat0),Math.max.apply(null,lon0)]],{padding:[20,20]});
draw();
</script></body></html>')

out <- file.path(OUT_DIR, "bee_occurrence_explorer.html")
writeLines(html, out)
message("Wrote ", normalizePath(out), sprintf("  (%s)", format(structure(file.size(out), class = "object_size"), units = "KB")))
