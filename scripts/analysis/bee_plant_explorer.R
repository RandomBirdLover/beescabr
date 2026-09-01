# =============================================================
# analysis/bee_plant_explorer.R
# beescabr -- INTERACTIVE bee <-> plant explorer: which plant genera has each bee
# been recorded on at the park, and which bees has each plant genus been recorded with.
#
# WHY A PAGE AND NOT THE MATRIX: the full grid is 77 bee species x 72 plant genera and
# only 8.8% of its cells are filled, so as a table it is mostly empty space. The same
# 490 pairs read clearly as two lists.
#
# BOTH DIRECTIONS, deliberately. Bee to plant answers "what does this bee use". Plant to
# bee answers "what should we plant", which is the question restoration work actually
# asks, and a bee-major matrix buries it.
#
# HOW TO READ IT, and why the count is on every row: the median bee here has records on
# TWO plant genera and 33 of the 77 rest on a single record. That is a record of what
# people happened to photograph or net, not a measure of what a bee prefers. Rows built
# on one record are marked so they cannot be mistaken for a finding.
#
# Kept SEPARATE from the occurrence explorer on purpose: that page stays phenology-only
# so it does not steer surveyors toward the flowers a bee is already recorded on, which
# would feed its own evidence back into itself.
#
# Output: a SINGLE self-contained HTML (no external libs) ->
#   <DIR_REPORT>/reference/bee_plant/bee_plant_explorer.html
# published to the site by publish_pages.R as bee_plant_explorer.html.
#
# Run from the repo root:  Rscript scripts/analysis/bee_plant_explorer.R
# =============================================================
suppressMessages({ library(dplyr); library(stringr); library(jsonlite) })

# ---- the index: one entry per bee and one per plant genus --------------------
# PURE. pairs = data.frame(bee, plant, n) as written by bee_plant_matrix.R. Returns
# both directions, each row carrying its record count and whether that count is thin
# enough (a single record) that the page must say so.
bpe_index <- function(pairs, thin_at = 1L) {
  mk <- function(key, val) {
    out <- list()
    if (!nrow(pairs)) return(out)
    d <- data.frame(k = pairs[[key]], name = pairs[[val]],
                    n = as.integer(pairs$n), stringsAsFactors = FALSE)
    # strongest evidence first, then name, so a rerun cannot reshuffle equal rows
    d <- d[order(d$k, -d$n, d$name), , drop = FALSE]
    for (k in unique(d$k)) {
      r <- d[d$k == k, , drop = FALSE]
      out[[k]] <- list(name = r$name, n = r$n, thin = r$n <= thin_at)
    }
    out
  }
  list(by_bee = mk("bee", "plant"), by_plant = mk("plant", "bee"))
}

# ---- build (skipped when a test sources this file for the helper) -------------
if (!exists("BPE_SOURCED_FOR_HELPERS")) {
  if (!exists("PATHS"))           source("scripts/config.R")
  if (!exists("scope_cap"))       source("scripts/analysis/theme_beescabr.R")
  if (!exists("plant_label"))     source("scripts/analysis/plant_names.R")
  if (!exists("inat_photo_link")) source("scripts/analysis/inat_taxon_links.R")

  OUT_DIR <- file.path(DIR_REPORT, "reference/bee_plant")
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  PAIRS <- file.path(DIR_REPORT, "reference/nps_summary/bee_plant_pairs.csv")
  if (!file.exists(PAIRS))
    stop("bee_plant_pairs.csv not found. Run scripts/analysis/bee_plant_matrix.R first.")

  pairs <- read.csv(PAIRS, stringsAsFactors = FALSE)
  ix    <- bpe_index(pairs)

  # iNat links come from the reference table by taxon_id (never by matching a name).
  # A bee with no id is one iNaturalist has not published a taxon for, and
  # inat_photo_link falls back to a name search rather than emitting a dead link.
  .lk  <- read.csv(PATHS$taxonomy_lookup, stringsAsFactors = FALSE)
  .key <- ifelse(str_squish(.lk$species) == "" | is.na(.lk$species), NA_character_,
                 paste(str_squish(.lk$genus), str_squish(.lk$species)))
  .tid_of <- function(nm) {
    i <- match(nm, .key)
    if (is.na(i) || is.na(.lk$taxon_id[i])) NULL else as.integer(.lk$taxon_id[i])
  }

  bees   <- sort(names(ix$by_bee))
  plants <- sort(names(ix$by_plant))
  # italics on every scientific name: bee binomials and plant genera alike
  bee_html   <- function(b) sprintf("<i>%s</i>%s", b, inat_photo_link(.tid_of(b), b))
  plant_html <- function(g) plant_label(g, sci_wrap = "<i>%s</i>")

  payload <- toJSON(list(
    bees   = lapply(bees, function(b) list(
      key = b, label = bee_html(b),
      rows = lapply(seq_along(ix$by_bee[[b]]$name), function(i) list(
        label = plant_html(ix$by_bee[[b]]$name[i]),
        n = ix$by_bee[[b]]$n[i], thin = ix$by_bee[[b]]$thin[i])))),
    plants = lapply(plants, function(g) list(
      key = g, label = plant_html(g),
      rows = lapply(seq_along(ix$by_plant[[g]]$name), function(i) list(
        label = bee_html(ix$by_plant[[g]]$name[i]),
        n = ix$by_plant[[g]]$n[i], thin = ix$by_plant[[g]]$thin[i])))),
    plain = list(bees = bees, plants = plants)), auto_unbox = TRUE)

  n_thin <- sum(pairs$n <= 1)
  message(sprintf("Bee-plant explorer: %d bee species, %d plant genera, %d pairs (%d resting on a single record)",
                  length(bees), length(plants), nrow(pairs), n_thin))

  cap <- scope_cap(
    scope = sprintf("All bee records made on an identified flower, parkwide, across every year and month on record. The data comprise %d bee species and %d plant genera, forming %d recorded pairs, of which %d rest on a single record. A pair means the bee was recorded on that plant. It does not mean the bee prefers that plant or depends on it. Plants are summarized at genus level. Bees identified only to genus are excluded, because a plant list is meaningful only for a species.",
                    length(bees), length(plants), nrow(pairs), n_thin),
    method = "Photo records and specimen records pooled",
    rank   = "Bee species by plant genus",
    source = "iNaturalist observations and park specimen records, Cabrillo NM",
    sig    = "Descriptive summary. No statistical test is applied, because these are counts of what was recorded and survey effort was not equal across plants.")

  html <- paste0(
'<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Bee and Plant Explorer &#8211; Cabrillo National Monument</title>
<style>
html{background:', BEE_HTML[["page_alt"]], '}
body{font:15px/1.55 -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;color:', BEE_HTML[["ink"]], ';max-width:1180px;margin:26px auto;background:', BEE_HTML[["page"]], ';padding:34px 34px 42px;border:1px solid #e7e4dc;border-radius:14px;box-shadow:0 1px 2px rgba(20,20,20,.05),0 14px 40px rgba(20,20,20,.06)}
.org{font-size:12.5px;font-weight:700;text-transform:uppercase;letter-spacing:.09em;color:', BEE_HTML_GREEN[["mid"]], ';margin:0 0 3px}
h1{font-size:25px;font-weight:700;letter-spacing:-.01em;margin:0;color:', BEE_HTML_GREEN[["deep"]], '}
h1:after{content:"";display:block;width:56px;height:3px;background:', BEE_HTML_GREEN[["mid"]], ';border-radius:2px;margin:11px 0 2px}
p.sub{color:', BEE_HTML[["sub"]], ';margin:13px 0 4px;font-size:13.5px;max-width:900px}
.scopebox{font-size:13px;color:', BEE_HTML[["ink"]], ';background:', BEE_HTML[["head_bg"]], ';border-left:3px solid ', BEE_HTML_GREEN[["mid"]], ';padding:9px 13px;border-radius:0 7px 7px 0;margin:16px 0 0}
.note{font-size:13px;color:', BEE_HTML[["sub"]], ';background:', BEE_HTML[["head_bg"]], ';border-left:3px solid ', BEE_HTML_GREEN[["mid"]], ';padding:9px 13px;border-radius:0 7px 7px 0;margin:14px 0 4px}
.tabs{display:flex;gap:8px;margin:18px 0 0}
.tab{font:inherit;font-size:13.5px;font-weight:600;padding:8px 15px;border:1px solid ', BEE_HTML[["border"]], ';border-radius:9px 9px 0 0;background:#fffdf9;color:', BEE_HTML[["sub"]], ';cursor:pointer;border-bottom:none}
.tab.on{background:', BEE_HTML_GREEN[["mid"]], ';color:#fff;border-color:', BEE_HTML_GREEN[["mid"]], '}
.wrap{display:flex;gap:22px;align-items:flex-start;border-top:1px solid ', BEE_HTML[["border"]], ';padding-top:16px}
.side{width:300px;flex:none;max-height:70vh;overflow:auto;border:1px solid ', BEE_HTML[["border"]], ';border-radius:10px;padding:10px 12px;background:#fffdf9}
#q{width:100%;box-sizing:border-box;font:inherit;font-size:13px;padding:7px 9px;margin-bottom:8px;border:1px solid ', BEE_HTML[["border"]], ';border-radius:7px}
.row{padding:4px 7px;margin:1px 0;border-radius:6px;cursor:pointer;font-size:13px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.row:hover{background:rgba(28,92,40,.08)}
.row.on{background:rgba(28,92,40,.13);font-weight:700}
.row .c{color:', BEE_HTML[["sub"]], ';font-size:11.5px;font-style:normal;margin-left:9px;white-space:nowrap}
.main{flex:1;min-width:0}
#tname{font-size:19px;font-weight:700;color:', BEE_HTML_GREEN[["deep"]], ';white-space:nowrap}
#tmeta{font-size:12.5px;color:', BEE_HTML[["sub"]], ';margin:3px 0 12px}
table{border-collapse:collapse;width:auto;min-width:min(560px,100%);font-size:13.5px}
th,td{padding:7px 13px;border-bottom:1px solid ', BEE_HTML[["border"]], ';text-align:left}
th{font-size:11px;text-transform:uppercase;letter-spacing:.05em;color:', BEE_HTML_GREEN[["deep"]], ';background:', BEE_HTML[["head_bg"]], '}
th.num,td.num{text-align:right}
tr:last-child td{border-bottom:none}
.thin{color:', BEE_HTML[["sub"]], '}
.badge{display:inline-block;font-size:10.5px;font-weight:700;color:#8a5a00;background:#fdf0d5;border-radius:20px;padding:1px 8px;margin-left:8px;white-space:nowrap}
.cap{font-size:11.5px;color:', BEE_HTML[["scope"]], ';margin-top:18px;line-height:1.5}
a.inat{text-decoration:none}
@media (max-width:820px){.wrap{flex-direction:column}.side{width:100%;max-height:34vh}}
</style></head><body>
<div class="org">Cabrillo National Monument</div>
<h1>Bee and Plant Explorer &#127804;</h1>
<p class="sub">Every flower a bee has been recorded on at the park, readable from either end. Pick <b>By bee</b> to see the plants it has been found on, or switch to <b>By plant</b> and pick a plant genus to see which bees have been recorded visiting it. The second direction is the one that matters for planting and restoration.</p>
<div class="scopebox"><b>What is on this page.</b> Every record of a bee on an identified flower at Cabrillo National Monument, pooling netted specimens and every iNaturalist photo, not just formal survey records, across all years and months and the whole park. That comes to ', length(bees), ' bee species and ', length(plants), ' plant genera, forming ', nrow(pairs), ' recorded bee and plant pairs. Plants are grouped at genus level, and bees identified only to genus are left out, because a flower list is only meaningful for a species. Source: iNaturalist photos and netted specimens, Cabrillo National Monument (data as of ', bee_data_asof(), ').</div>
<div class="note"><b>These are records, not preferences.</b> A row means somebody photographed or netted that bee on that plant. It does not mean the bee depends on that plant, or that it favors it over another. Effort was never spread evenly across plants, so a common roadside shrub that many people walk past will collect more records than an uncommon plant that is just as important to the bee. The number of records is shown on every row so you can see how much is behind it, and rows resting on a single record are marked. Most rows here are thin: the typical bee on this page has records on only two plant genera.</p>
<div class="tabs"><button class="tab on" id="tb-bee" onclick="setMode(0)">By bee</button><button class="tab" id="tb-plant" onclick="setMode(1)">By plant</button></div>
<div class="wrap">
  <div class="side"><input id="q" type="search" placeholder="Search" oninput="draw()" autocomplete="off"><div id="list"></div></div>
  <div class="main"><div id="tname"></div><div id="tmeta"></div><div id="panel"></div></div>
</div>
<div class="cap">', cap, '</div>
<script>
var D = ', payload, ', mode = 0, sel = null;
function items(){ return mode ? D.plants : D.bees }
function draw(){
  var q = (document.getElementById("q").value||"").toLowerCase(), L = document.getElementById("list");
  var keys = mode ? D.plain.plants : D.plain.bees, all = items();
  L.innerHTML = "";
  all.forEach(function(it, i){
    if (q && keys[i].toLowerCase().indexOf(q) < 0) return;
    var d = document.createElement("div");
    d.className = "row" + (sel === keys[i] ? " on" : "");
    // the icon names WHAT is being counted, so it flips with the direction:
    // a bee row counts plant genera, a plant row counts bee species.
    var unit = mode ? "bee species" : "plant " + (it.rows.length === 1 ? "genus" : "genera"),
        icon = mode ? "&#128029;" : "&#127804;";
    d.innerHTML = it.label + \'<span class="c" title="\' + it.rows.length + " " + unit +
                  \'">\' + icon + "&nbsp;" + it.rows.length + "</span>";
    d.onclick = function(){ sel = keys[i]; draw(); show(it) };
    L.appendChild(d);
  });
}
function show(it){
  var one = mode ? "bee species" : "plant genus",
      many = mode ? "bee species" : "plant genera", tot = 0;
  it.rows.forEach(function(r){ tot += r.n });
  document.getElementById("tname").innerHTML = it.label;
  document.getElementById("tmeta").textContent =
    it.rows.length + " " + (it.rows.length === 1 ? one : many) +
    ", " + tot + " record" + (tot === 1 ? "" : "s") + " in total.";
  var h = \'<table><tr><th>\' + (mode ? "Bee species" : "Plant genus") +
          \'</th><th class="num">Records</th></tr>\';
  it.rows.forEach(function(r){
    h += \'<tr><td\' + (r.thin ? \' class="thin"\' : \'\') + \'>\' + r.label +
         (r.thin ? \' <span class="badge">one record</span>\' : \'\') +
         \'</td><td class="num">\' + r.n + "</td></tr>";
  });
  document.getElementById("panel").innerHTML = h + "</table>";
}
function setMode(m){
  mode = m; sel = null;
  document.getElementById("tb-bee").className = "tab" + (m ? "" : " on");
  document.getElementById("tb-plant").className = "tab" + (m ? " on" : "");
  document.getElementById("q").value = "";
  document.getElementById("tname").innerHTML = "";
  document.getElementById("tmeta").textContent = "";
  document.getElementById("panel").innerHTML = "";
  draw();
}
draw();
</script></body></html>')

  writeLines(html, file.path(OUT_DIR, "bee_plant_explorer.html"))
  message("  wrote ", file.path(OUT_DIR, "bee_plant_explorer.html"))
}
