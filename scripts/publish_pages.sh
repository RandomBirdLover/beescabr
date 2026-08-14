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

# source_path  ->  docs_filename  |  card title  |  card blurb
# (kept as parallel arrays so the same list drives both the copy and the index)
pages=(
  "$SRC/reference/field_guide/bee_field_guide_species.html|field_guide_species.html|Bee Field Guide (Species)|Every bee species recorded at Cabrillo, with photos, IUCN status, abundance, and forage."
  "$SRC/reference/field_guide/bee_field_guide_genus.html|field_guide_genus.html|Bee Field Guide (Genus)|The same guide grouped by genus for quicker browsing."
  "$SRC/reference/nps_summary/nps_summary_tables.html|summary_tables.html|Park Summary Tables|Headline counts: species, genera, plants, survey effort, and participation."
  "$SRC/coverage/least_sampled/least_sampled_bees.html|least_sampled_bees.html|Least-Sampled Bees|The bees with the thinnest evidence, where more surveying would help most."
  "$SRC/reference/transects/cabr_bee_transects_map.html|transects_map.html|Survey Transect Map|Interactive map of the fixed survey transects at Cabrillo National Monument."
  "$SRC/coverage/bee_bounties/specimen_bee_bounty_map.html|specimen_bounty_map.html|Specimen Bee Bounty Map|Where to net a voucher specimen: gaps the collection still needs."
  "$SRC/coverage/bee_bounties/inaturalist_bee_bounty_map.html|inaturalist_bounty_map.html|iNaturalist Bee Bounty Map|Where to photograph bees to fill iNaturalist gaps."
)

# ---- copy each page into docs/ ---------------------------------------------
for row in "${pages[@]}"; do
  IFS='|' read -r src out _title _blurb <<< "$row"
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
  IFS='|' read -r src out title blurb <<< "$row"
  [ -f "$DOCS/$out" ] || continue
  cards="$cards
      <a class=\"card\" href=\"./$out\">
        <h2>$title</h2>
        <p>$blurb</p>
      </a>"
done

cat > "$DOCS/index.html" <<HTML
<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Cabrillo National Monument Native Bees</title>
<style>
  :root { --bg:#fbfaf7; --fg:#1c1c1c; --muted:#5c5c5c; --card:#ffffff; --border:#e6e2d8; --accent:#4C9E90; }
  @media (prefers-color-scheme: dark) {
    :root { --bg:#161613; --fg:#ececec; --muted:#a6a6a6; --card:#1f1f1c; --border:#2f2f2a; --accent:#5bb0a1; }
  }
  * { box-sizing: border-box; }
  body { margin:0; background:var(--bg); color:var(--fg);
         font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;
         line-height:1.5; }
  header { max-width:920px; margin:0 auto; padding:3rem 1.25rem 1rem; }
  h1 { margin:0 0 .35rem; font-size:1.9rem; }
  .lead { color:var(--muted); margin:0; max-width:60ch; }
  main { max-width:920px; margin:0 auto; padding:1rem 1.25rem 3rem;
         display:grid; grid-template-columns:repeat(auto-fill,minmax(260px,1fr)); gap:1rem; }
  .card { display:block; background:var(--card); border:1px solid var(--border);
          border-radius:12px; padding:1.1rem 1.2rem; text-decoration:none; color:inherit;
          transition:border-color .15s, transform .15s; }
  .card:hover { border-color:var(--accent); transform:translateY(-2px); }
  .card h2 { margin:0 0 .35rem; font-size:1.1rem; color:var(--accent); }
  .card p  { margin:0; color:var(--muted); font-size:.92rem; }
  footer { max-width:920px; margin:0 auto; padding:0 1.25rem 3rem; color:var(--muted); font-size:.85rem; }
</style>
</head>
<body>
  <header>
    <h1>Cabrillo National Monument &mdash; Native Bees</h1>
    <p class="lead">Field guides, checklists, and interactive maps from the Cabrillo native-bee survey. Pick a page below.</p>
  </header>
  <main>$cards
  </main>
  <footer>Generated from the beescabr pipeline. Data as of the latest survey export.</footer>
</body>
</html>
HTML
echo "built      index.html"
echo "Done. Review docs/, then commit and push. Enable GitHub Pages: Settings -> Pages -> main /docs."
