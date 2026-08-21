# =============================================================
# analysis/bee_trends_explorer.R
# beescabr -- INTERACTIVE year-trend explorer: pick any well-recorded bee and
# see its share of the park's bee records move year to year.
#
# Same controls as common_bee_trends.R: PHOTO records only (one method across all
# years -- netting ran only 2021-2023), the Mar-Oct standardized survey season
# (config FAIR_MONTHS) so every year covers the same months, and each bee's SHARE
# of all bee photos that season (raw counts would track surveying effort, not
# bees). Taxa shown: species and whole genera with >= MIN_RECORDS in-window
# records (a share on fewer records is noise). The page carries a prominent
# "not enough data yet" notice: six seasons is a first look, not a conclusion.
# Chart convention: species = solid line, whole genus = dashed (the site-wide
# genus-vs-species mark), colored by family hue.
#
# Output: a SINGLE self-contained HTML (no external libs) ->
#   data/analysis/nps_report_2026/phenology/bee_trends_explorer.html
# published to the site by publish_pages.R as bee_trends.html.
#
# Run from the repo root:  Rscript scripts/analysis/bee_trends_explorer.R
# =============================================================
suppressMessages({ library(dplyr); library(stringr); library(jsonlite) })
if (!exists("PATHS"))     source("scripts/config.R")
if (!exists("scope_cap")) source("scripts/analysis/theme_beescabr.R")

OUT      <- file.path(DIR_REPORT, "phenology/bee_trends_explorer.html")
MIN_YEAR    <- 2021
PARTIAL_YR  <- 2026
MIN_RECORDS <- 50
SEASON_MONTHS <- FAIR_MONTHS   # Mar-Oct: the standardized survey season, same as the journal figures

# ---- 1. pool every bee record, tag family -----------------------------------
rd <- function(p) read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
inat <- rd(PATHS$inat_clean)
pool <- inat %>%
  transmute(year  = as.integer(substr(observed_on, 1, 4)),
            month = as.integer(substr(observed_on, 6, 7)), family, genus, species, taxon_rank) %>%
  filter(!is.na(year), year >= MIN_YEAR, month %in% SEASON_MONTHS, !is.na(genus), genus != "")
years  <- sort(unique(pool$year))
totals <- pool %>% count(year, name = "n_total") %>% arrange(year)

fam_of <- function(g) {   # majority family per genus (a few records carry blanks)
  f <- pool$family[pool$genus == g]; f <- f[!is.na(f) & f != ""]
  if (!length(f)) "Other" else names(sort(table(f), decreasing = TRUE))[1]
}

# ---- 2. taxa with enough records: species + whole genera --------------------
counts_by_year <- function(d) {
  d %>% count(year, name = "n") %>% right_join(totals, by = "year") %>%
    arrange(year) %>% mutate(n = coalesce(n, 0L), share = round(100 * n / n_total, 2))
}
sp_ok <- pool %>% filter(taxon_rank %in% c("species", "subspecies"), species != "", !is.na(species)) %>%
  count(genus, species) %>% filter(n >= MIN_RECORDS)
gn_ok <- pool %>% count(genus) %>% filter(n >= MIN_RECORDS)

mk_taxon <- function(name, rank, fam, d) {
  cy <- counts_by_year(d)
  ct <- suppressWarnings(cor.test(cy$year, cy$share, method = "spearman"))
  list(name = name, rank = rank, family = fam, total = sum(cy$n),
       n = cy$n, share = cy$share,
       rho = round(unname(ct$estimate), 2), p = round(ct$p.value, 2))
}
taxa <- c(
  lapply(seq_len(nrow(sp_ok)), function(i) {
    g <- sp_ok$genus[i]; s <- word(sp_ok$species[i], -1)
    mk_taxon(paste(g, s), "species", fam_of(g),
             pool %>% filter(genus == g, str_detect(coalesce(species, ""), s)))
  }),
  lapply(gn_ok$genus, function(g)
    mk_taxon(g, "genus", fam_of(g), pool %>% filter(genus == g))))
# order: family (house order), then species before whole-genus rows, then name
fam_rank <- setNames(seq_along(BEE_FAMILY_ORDER), BEE_FAMILY_ORDER)
ord <- order(vapply(taxa, function(t) fam_rank[t$family] %||% 99, 1),
             vapply(taxa, function(t) t$rank != "species", TRUE),
             vapply(taxa, function(t) t$name, ""))
taxa <- taxa[ord]
message(sprintf("Trend explorer: %d taxa (%d species + %d whole genera, >= %d records since %d)",
                length(taxa), nrow(sp_ok), nrow(gn_ok), MIN_RECORDS, MIN_YEAR))

payload <- toJSON(list(
  years = years, totals = totals$n_total, partial = PARTIAL_YR,
  famcol = as.list(BEE_FAMILY), taxa = taxa), auto_unbox = TRUE)

# ---- 3. the page ------------------------------------------------------------
cap <- scope_cap(
  scope  = sprintf("iNaturalist photo records only, %d-%d, whole park, every observer; March-October only, the standardized survey season, so every year covers the same months; share = the bee's photos / all bee photos that season; taxa with >= %d in-window records; a share is relative, not a population count; %d is partial (its season is missing September and October; open point)",
                   MIN_YEAR, PARTIAL_YR, MIN_RECORDS, PARTIAL_YR),
  method = "non-lethal (photo records) only", rank = "bee species and whole genera",
  source = "iNaturalist observations, Cabrillo NM",
  sig = bee_test("Spearman rank correlation of annual record share vs year (per taxon)"))

html <- paste0(
'<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Bee Trends Explorer &#8211; Cabrillo National Monument</title>
<style>
html{background:', BEE_HTML[["page_alt"]], '}
body{font:15px/1.55 -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;color:', BEE_HTML[["ink"]], ';max-width:1180px;margin:26px auto;background:', BEE_HTML[["page"]], ';padding:34px 34px 42px;border:1px solid #e7e4dc;border-radius:14px;box-shadow:0 1px 2px rgba(20,20,20,.05),0 14px 40px rgba(20,20,20,.06)}
.org{font-size:12.5px;font-weight:700;text-transform:uppercase;letter-spacing:.09em;color:', BEE_HTML_GREEN[["mid"]], ';margin:0 0 3px}
h1{font-size:25px;font-weight:700;letter-spacing:-.01em;margin:0;color:', BEE_HTML_GREEN[["deep"]], '}
h1:after{content:"";display:block;width:56px;height:3px;background:', BEE_HTML_GREEN[["mid"]], ';border-radius:2px;margin:11px 0 2px}
p.sub{color:', BEE_HTML[["sub"]], ';margin:13px 0 4px;font-size:13.5px;max-width:900px}
.wrap{display:flex;gap:22px;margin-top:18px;align-items:flex-start}
.side{width:288px;flex:none;max-height:72vh;overflow:auto;border:1px solid ', BEE_HTML[["border"]], ';border-radius:10px;padding:10px 12px;background:#fffdf9}
.fam{font-weight:700;font-size:10.5px;text-transform:uppercase;letter-spacing:.06em;color:', BEE_HTML_GREEN[["deep"]], ';margin:10px 0 3px;padding-left:7px;border-left:3px solid #999}
.row{display:flex;align-items:center;gap:8px;padding:3px 6px;margin:1px 0;border-radius:6px;cursor:pointer;font-size:13px;white-space:nowrap}
.row:hover{background:rgba(28,92,40,.08)}
.row.on{background:rgba(28,92,40,.13);font-weight:700}
.dot{width:11px;height:11px;border-radius:50%;flex:none;box-shadow:0 0 0 1px rgba(0,0,0,.15)}
.dot.g{background:#fff !important;border:2.5px dashed transparent;box-sizing:border-box}
.row i{font-style:italic}
.row .wg{color:#8a8071;font-size:10.5px;font-style:normal;margin-left:2px}
.main{flex:1;min-width:0}
.note{font-size:13px;color:', BEE_HTML[["sub"]], ';background:', BEE_HTML[["head_bg"]], ';border-left:3px solid ', BEE_HTML_GREEN[["mid"]], ';padding:9px 13px;border-radius:0 7px 7px 0;margin:0 0 12px}
#chartbox{border:1px solid ', BEE_HTML[["border"]], ';border-radius:10px;padding:14px 16px 8px;background:#fffdf9}
#tname{font-size:18px;font-weight:700;color:', BEE_HTML_GREEN[["deep"]], '}
#tmeta{font-size:12.5px;color:', BEE_HTML[["sub"]], ';margin:2px 0 6px}
svg{width:100%;height:auto;display:block}
.cap{font-size:11.5px;color:', BEE_HTML[["scope"]], ';margin-top:14px;line-height:1.5}
@media (max-width:820px){.wrap{flex-direction:column}.side{width:100%;max-height:38vh}}
</style></head><body>
<div class="org">Cabrillo National Monument</div>
<h1>Bee Trends Explorer &#128200;</h1>
<p class="sub">Pick a bee on the left to see how its share of the park&rsquo;s bee photos has moved from season to season. Share, not raw counts: surveying grew enormously over these years, so counting photos would only measure the surveying. Dividing by everything photographed that season cancels the effort out. Every year is compared over the same March to October season, using photo records only. Solid lines are single species; dashed lines are a whole genus pooled.</p>
<p class="sub"><b>A share is still not a population count.</b> It shows how this bee is doing relative to the rest of the bee community. Small wiggles are normal, so look for sustained runs, not single-year jumps.</p>
<div class="note"><b>Not enough data yet.</b> This program has run for six seasons, and six points cannot prove a trend in either direction. Almost nothing here will test as statistically significant, and that is expected. Treat every line as a first look. Each added year of monitoring makes this page more trustworthy.</div>
<div class="wrap">
  <div class="side" id="list"></div>
  <div class="main">
    <div id="chartbox">
      <div id="tname"></div>
      <div id="tmeta"></div>
      <svg id="chart" viewBox="0 0 860 470" role="img" aria-label="share of bee records by year"></svg>
    </div>
    <div class="cap">', str_replace_all(cap, "([<>&])", function(x) c("<"="&lt;", ">"="&gt;", "&"="&amp;")[x]), '</div>
  </div>
</div>
<script>
var D = ', payload, ';
var INK = "', BEE_INK$primary, '", SUB = "#6b6a66", GRID = "#e4e0d6";
function famcol(f){ return D.famcol[f] || "#8A8880"; }
function el(t,a){ var e=document.createElementNS("http://www.w3.org/2000/svg",t);
  for(var k in a) e.setAttribute(k,a[k]); return e; }
function fmt(n){ return n.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g,","); }

// ---- sidebar ----
var list = document.getElementById("list"), cur = 0;
function buildList(){
  var fams = [];
  D.taxa.forEach(function(t){ if(fams.indexOf(t.family)<0) fams.push(t.family); });
  fams.forEach(function(f){
    var h = document.createElement("div"); h.className="fam"; h.textContent=f;
    h.style.borderLeftColor = famcol(f); list.appendChild(h);
    D.taxa.forEach(function(t,i){
      if(t.family!==f) return;
      var r = document.createElement("div"); r.className="row"; r.dataset.i=i;
      var d = document.createElement("span");
      d.className = "dot" + (t.rank==="genus" ? " g" : "");
      if(t.rank==="genus") d.style.borderColor = famcol(f); else d.style.background = famcol(f);
      r.appendChild(d);
      var nm = document.createElement("i"); nm.textContent = t.name; r.appendChild(nm);
      if(t.rank==="genus"){ var w=document.createElement("span"); w.className="wg"; w.textContent="whole genus"; r.appendChild(w); }
      r.addEventListener("click", function(){ select(i); });
      list.appendChild(r);
    });
  });
}
function select(i){
  cur = i;
  var rows = list.querySelectorAll(".row");
  rows.forEach(function(r){ r.classList.toggle("on", +r.dataset.i===i); });
  draw(D.taxa[i]);
}

// ---- chart ----
function draw(t){
  var svg = document.getElementById("chart"); svg.innerHTML = "";
  var W=860,H=470,L=62,R=24,T=18,B=86, iw=W-L-R, ih=H-T-B;
  var col = famcol(t.family), dash = (t.rank==="genus");
  document.getElementById("tname").innerHTML =
    "<i>"+t.name+"</i>" + (t.rank==="genus" ? " <span style=\'font-weight:400;font-size:13px;color:"+SUB+"\'>(whole genus, all records pooled)</span>" : "");
  var trend = (t.p < 0.05) ? (t.rho > 0 ? "a significant upward trend" : "a significant downward trend")
                           : "no significant trend";
  document.getElementById("tmeta").textContent =
    t.family + " \\u00b7 " + fmt(t.total) + " records since " + D.years[0] +
    " \\u00b7 Spearman rho = " + t.rho + ", p = " + t.p + " (" + trend + ")";
  var ymax = Math.max.apply(null, t.share) * 1.25; if (ymax < 5) ymax = 5;
  var step = ymax > 16 ? 5 : (ymax > 8 ? 2 : 1);
  var X = function(j){ return L + iw * (j / (D.years.length - 1)); };
  var Y = function(v){ return T + ih * (1 - v / ymax); };
  // gridlines + y ticks
  for (var v = 0; v <= ymax; v += step){
    svg.appendChild(el("line",{x1:L,x2:W-R,y1:Y(v),y2:Y(v),stroke:GRID,"stroke-width":1}));
    var ty = document.createElementNS(svg.namespaceURI,"text");
    ty.setAttribute("x",L-8); ty.setAttribute("y",Y(v)+4); ty.setAttribute("text-anchor","end");
    ty.setAttribute("font-size","12.5"); ty.setAttribute("fill",SUB); ty.textContent=v+"%";
    svg.appendChild(ty);
  }
  // x ticks: year + n=
  D.years.forEach(function(yr,j){
    var tx = document.createElementNS(svg.namespaceURI,"text");
    tx.setAttribute("x",X(j)); tx.setAttribute("y",H-B+26); tx.setAttribute("text-anchor","middle");
    tx.setAttribute("font-size","13.5"); tx.setAttribute("fill",INK); tx.textContent=yr;
    svg.appendChild(tx);
    var tn = document.createElementNS(svg.namespaceURI,"text");
    tn.setAttribute("x",X(j)); tn.setAttribute("y",H-B+43); tn.setAttribute("text-anchor","middle");
    tn.setAttribute("font-size","11"); tn.setAttribute("fill",SUB); tn.textContent="n="+fmt(D.totals[j]);
    svg.appendChild(tn);
  });
  // axis titles
  var xt = document.createElementNS(svg.namespaceURI,"text");
  xt.setAttribute("x",L+iw/2); xt.setAttribute("y",H-14); xt.setAttribute("text-anchor","middle");
  xt.setAttribute("font-size","13"); xt.setAttribute("fill",SUB);
  xt.textContent = "year (n = all bee records that year)"; svg.appendChild(xt);
  var yt = document.createElementNS(svg.namespaceURI,"text");
  yt.setAttribute("x",16); yt.setAttribute("y",T+ih/2); yt.setAttribute("font-size","13"); yt.setAttribute("fill",SUB);
  yt.setAttribute("transform","rotate(-90 16 "+(T+ih/2)+")"); yt.setAttribute("text-anchor","middle");
  yt.textContent = "share of all bee records (%)"; svg.appendChild(yt);
  // line
  var pts = t.share.map(function(v,j){ return X(j)+","+Y(v); }).join(" ");
  var ln = el("polyline",{points:pts,fill:"none",stroke:col,"stroke-width":2.6,
                          "stroke-linejoin":"round","stroke-linecap":"round"});
  if (dash) ln.setAttribute("stroke-dasharray","7 6");
  svg.appendChild(ln);
  // points + value labels
  t.share.forEach(function(v,j){
    var partial = (D.years[j] === D.partial);
    svg.appendChild(el("circle",{cx:X(j),cy:Y(v),r:4.6,fill:partial?"#fff":col,
                                 stroke:col,"stroke-width":2.2}));
    var tv = document.createElementNS(svg.namespaceURI,"text");
    tv.setAttribute("x",X(j)); tv.setAttribute("y",Y(v)-11); tv.setAttribute("text-anchor","middle");
    tv.setAttribute("font-size","12.5"); tv.setAttribute("font-weight","700"); tv.setAttribute("fill",INK);
    tv.textContent = Math.round(v)+"%";
    svg.appendChild(tv);
  });
}
buildList();
// open on the most-recorded species (the park favorite)
var start = 0, best = -1;
D.taxa.forEach(function(t,i){ if(t.rank==="species" && t.total > best){ best = t.total; start = i; } });
select(start);
</script>
</body></html>')
writeLines(html, OUT)
message("Wrote ", OUT)
