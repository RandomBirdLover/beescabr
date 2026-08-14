#!/usr/bin/env bash
# =============================================================
# publish_pages.sh -- copy the PUBLIC report HTML pages into docs/ (tracked)
# so GitHub Pages can serve them, and (re)build the docs/index.html landing page.
#
# The generated HTML lives under data/analysis/ (gitignored), so this script
# copies the chosen public pages into docs/ with clean, stable filenames. Run it
# after the pipeline whenever the pages change:
#     bash scripts/publish_pages.sh
# then commit docs/ and push. GitHub Pages source = main branch, /docs folder.
#
# Only the PUBLIC report set is published here -- internal QA tools (per-surveyor
# pins-to-fix pages, reviewer drafts) are deliberately excluded.
# =============================================================
set -euo pipefail
cd "$(dirname "$0")/.."                       # repo root
SRC="data/analysis/nps_report_2026"
DOCS="docs"
mkdir -p "$DOCS"
touch "$DOCS/.nojekyll"                        # serve files as-is, no Jekyll processing

# source_path | docs_filename | card title | card blurb | icon (emoji) | category tag
# (one list drives both the file copy and the landing-page cards)
pages=(
  "$SRC/reference/field_guide/bee_field_guide_species.html|field_guide_species.html|Bee Field Guide (Species)|Every bee species recorded at Cabrillo, with photos, IUCN status, abundance, and forage.|🐝|Field guide"
  "$SRC/reference/field_guide/bee_field_guide_genus.html|field_guide_genus.html|Bee Field Guide (Genus)|The same guide grouped by genus for quicker browsing.|🐝|Field guide"
  "$SRC/reference/nps_summary/nps_summary_tables.html|summary_tables.html|Park Summary Tables|Headline counts: species, genera, plants, survey effort, and participation.|📊|Summary"
  "$SRC/coverage/least_sampled/least_sampled_bees.html|least_sampled_bees.html|Least-Sampled Bees|The bees with the thinnest evidence, where more surveying would help most.|❗|Priorities"
  "$SRC/reference/transects/cabr_bee_transects_map.html|transects_map.html|Survey Transect Map|Interactive map of the fixed survey transects at Cabrillo National Monument.|🗺️|Map"
  "$SRC/coverage/bee_bounties/specimen_bee_bounty_map.html|specimen_bounty_map.html|Specimen Bee Bounty Map|Where to net a voucher specimen: gaps the collection still needs.|🔬|Map"
  "$SRC/coverage/bee_bounties/inaturalist_bee_bounty_map.html|inaturalist_bounty_map.html|iNaturalist Bee Bounty Map|Where to photograph bees to fill iNaturalist gaps.|📷|Map"
)

# ---- copy each page into docs/ ---------------------------------------------
for row in "${pages[@]}"; do
  IFS='|' read -r src out _title _blurb _icon _tag <<< "$row"
  if [ -f "$src" ]; then
    cp "$src" "$DOCS/$out"
    echo "published  $out"
  else
    echo "SKIP (missing)  $src" >&2
  fi
done

# ---- build the landing page (docs/index.html) ------------------------------
cards=""
for row in "${pages[@]}"; do
  IFS='|' read -r src out title blurb icon tag <<< "$row"
  [ -f "$DOCS/$out" ] || continue
  cards="$cards
      <a class=\"card\" href=\"./$out\">
        <div class=\"card-head\"><span class=\"icon\">$icon</span><span class=\"tag\">$tag</span></div>
        <h2>$title</h2>
        <p>$blurb</p>
        <span class=\"go\">Open<span class=\"arrow\">&rarr;</span></span>
      </a>"
done

cat > "$DOCS/index.html" <<HTML
<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Cabrillo National Monument &mdash; Native Bee Monitoring Program</title>
<style>
  :root {
    --bg:#f1f8f8; --bg2:#e4f1f1; --fg:#18292c; --muted:#5b6d6f;
    --card:#ffffff; --border:#d4e6e6; --accent:#438990; --accent-deep:#1d5663; --accent-soft:#daeded;
    --shadow:0 1px 2px rgba(20,50,54,.05), 0 8px 24px rgba(20,50,54,.07);
    --shadow-hover:0 2px 6px rgba(20,50,54,.1), 0 16px 40px rgba(20,50,54,.14);
  }
  @media (prefers-color-scheme: dark) {
    :root {
      --bg:#0e1718; --bg2:#0a1213; --fg:#e6efef; --muted:#92a3a4;
      --card:#14201f; --border:#243a3a; --accent:#5aa6ad; --accent-deep:#93c8ce; --accent-soft:#123030;
      --shadow:0 1px 2px rgba(0,0,0,.3), 0 10px 30px rgba(0,0,0,.35);
      --shadow-hover:0 2px 8px rgba(0,0,0,.4), 0 20px 50px rgba(0,0,0,.5);
    }
  }
  * { box-sizing:border-box; }
  body { margin:0; color:var(--fg); line-height:1.55;
         background:linear-gradient(180deg,var(--bg) 0%,var(--bg2) 100%);
         background-attachment:fixed; min-height:100vh;
         font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;
         -webkit-font-smoothing:antialiased; }
  .hero { position:relative; overflow:hidden; background:#16302b; border-bottom:1px solid var(--border); }
  .hero-bg { position:absolute; inset:0; z-index:0; width:100%; height:100%;
             object-fit:cover; object-position:center 45%; transform:scaleX(-1); }
  .hero::after { content:""; position:absolute; inset:0; z-index:1; pointer-events:none;
                 background:linear-gradient(180deg, rgba(14,28,30,.34) 0%, rgba(14,28,30,.72) 100%); }
  .hero-inner { position:relative; z-index:2; max-width:960px; margin:0 auto; padding:6rem 1.5rem 3.5rem; }
  .eyebrow { display:inline-block; font-size:.72rem; letter-spacing:.14em; text-transform:uppercase;
             font-weight:700; color:#fff; margin:0 0 .7rem;
             padding:.3rem .7rem; background:rgba(255,255,255,.16); backdrop-filter:blur(4px);
             border:1px solid rgba(255,255,255,.22); border-radius:999px; }
  h1 { margin:0 0 .5rem; font-size:2.5rem; line-height:1.12; letter-spacing:-.02em; font-weight:800;
       color:#fff; text-shadow:0 2px 24px rgba(0,0,0,.4); }
  .lead { color:rgba(255,255,255,.92); margin:0; max-width:58ch; font-size:1.08rem;
          text-shadow:0 1px 12px rgba(0,0,0,.35); }
  .credit { position:absolute; z-index:2; right:.9rem; bottom:.6rem; margin:0; font-size:.7rem;
            color:rgba(255,255,255,.8); text-shadow:0 1px 6px rgba(0,0,0,.7); max-width:70%; text-align:right; }
  .credit a { color:inherit; text-decoration:underline; text-underline-offset:2px; }
  .credit a.inat { text-decoration:none; }
  .credit img { width:15px; height:15px; vertical-align:-3px; margin-right:.1rem; border-radius:3px; }
  main { max-width:960px; margin:0 auto; padding:1.5rem 1.5rem 3.5rem;
         display:grid; grid-template-columns:repeat(auto-fill,minmax(280px,1fr)); gap:1.15rem; }
  .card { position:relative; display:flex; flex-direction:column; background:var(--card);
          border:1px solid var(--border); border-radius:16px; padding:1.35rem 1.4rem 1.25rem;
          text-decoration:none; color:inherit; box-shadow:var(--shadow); overflow:hidden;
          transition:transform .18s ease, box-shadow .18s ease, border-color .18s ease; }
  .card::before { content:""; position:absolute; inset:0 auto 0 0; width:3px; background:var(--accent);
                  opacity:0; transition:opacity .18s ease; }
  .card:hover { transform:translateY(-4px); box-shadow:var(--shadow-hover); border-color:var(--accent); }
  .card:hover::before { opacity:1; }
  .card-head { display:flex; align-items:center; justify-content:space-between; margin-bottom:.85rem; }
  .icon { font-size:1.7rem; line-height:1; }
  .tag { font-size:.68rem; font-weight:700; letter-spacing:.08em; text-transform:uppercase;
         color:var(--accent); background:var(--accent-soft); padding:.28rem .6rem; border-radius:999px; }
  .card h2 { margin:0 0 .4rem; font-size:1.18rem; letter-spacing:-.01em; color:var(--accent-deep); }
  .card p  { margin:0 0 1.1rem; color:var(--muted); font-size:.93rem; flex:1; }
  .go { display:inline-flex; align-items:center; gap:.35rem; font-size:.88rem; font-weight:600; color:var(--accent); }
  .arrow { transition:transform .18s ease; }
  .card:hover .arrow { transform:translateX(4px); }
  footer { max-width:960px; margin:0 auto; padding:0 1.5rem 3.5rem; color:var(--muted); font-size:.85rem;
           border-top:1px solid var(--border); padding-top:1.5rem; }
</style>
</head>
<body>
  <div class="hero">
    <img class="hero-bg" src="./hero.jpg" alt="">
    <div class="hero-inner">
      <span class="eyebrow">&#127963;&#65039; Cabrillo National Monument</span>
      <h1>Native Bee Monitoring Program</h1>
      <p class="lead">Field guides, checklists, and interactive maps from the Cabrillo native-bee survey. Pick a page to explore.</p>
    </div>
    <p class="credit"><a class="inat" href="https://www.inaturalist.org/observations/98453614" title="View on iNaturalist"><img src="./inat-logo.png" alt="iNaturalist" width="15" height="15"></a> Peridot Sweat Bee (<i>Augochlorella pomoniella</i>) &middot; Michael Ready &middot; <a href="https://creativecommons.org/licenses/by-nc/4.0/">CC BY-NC</a></p>
  </div>
  <main>$cards
  </main>
  <footer>Generated from the beescabr pipeline. Data as of the latest survey export.</footer>
</body>
</html>
HTML
echo "built      index.html"
echo "Done. Review docs/, then commit and push. Enable GitHub Pages: Settings -> Pages -> main /docs."
