# The public website

Everything about the site at **https://randombirdlover.github.io/beescabr/**: how it is
built, how a page gets published, and the layout every page follows. Before this file
existed the site was mentioned in passing across nine documents and owned by none of
them, which is why one page could drift from the rest without anyone noticing.

---

## 1. What the site is

GitHub Pages serves the `docs/` folder of this repository on the `main` branch. There is
no build step and no framework. Each page is a **single self-contained HTML file** with
its CSS and JavaScript inline, written by a script in `scripts/analysis/`.

`docs/` holds only finished, non-sensitive output. The underlying records stay out of git
entirely (see `DATA_ACCESS.md` and the `/data` rule in `.gitignore`).

## 2. How a page reaches the site

```
Rscript scripts/run_all_analysis_pipeline.R            # writes pages into data/analysis/<season>/
Rscript scripts/run_publishing_materials_pipeline.R    # copies them into docs/ and rebuilds the landing page
git add -A && git commit && git push                   # GitHub Pages serves the new docs/
```

The publisher is `scripts/publish/publish_pages.R`. It copies each page named in
`PUBLISH_PAGES` from the season folder into `docs/`, injects the "Back to main page"
pill, and rebuilds `index.html` from the manifest.

**To add a page:** write the script so it outputs one self-contained HTML file into the
season folder, then add a row to `PUBLISH_PAGES` with its `src`, `out`, `title`, `icon`,
`tag`, and `blurb`. The `tag` decides which section of the landing page it appears under.

**Two guards stop a bad publish**, and both leave `docs/` untouched:

- **"No published pages were found"** — the analysis has not been run for this season.
- **"These pages are OLDER than the cleaned data"** — a page predates the data it is
  built from, which usually means its analysis script failed. Override with
  `BEESCABR_ALLOW_STALE=1` only when you know the pages are fine.

**Cache-busting:** `page_version()` appends a hash to each card link so browsers do not
serve a stale page after an update. Nothing to do by hand.

**The `docs/` gitignore rule is inverted.** `docs/*` is ignored and specific types are
allow-listed back (`.html`, `.nojekyll`, and image formats). A stray CSV or note dropped
into `docs/` is therefore ignored rather than silently published. **If you add a genuinely
new web asset type (a `.css` or `.svg`), you must add a matching `!` line or the live site
will look broken.**

## 3. Page layout

Every table page follows the same slot order: title and byline, a one-sentence lead, an
optional key as a list, one caveat box, the table, column keys under the table, and
provenance last in an "About this data" box. The full standard, with the reasoning, is in
**CLAUDE.md** under "Page layout". `bee_table_css()` in `theme_beescabr.R` carries the
`.scope-foot`, `.tkey`, and `.cov` classes, so a page gets the styling by using them.

Colours never appear as hex literals in a page script. They come from
`theme_beescabr.R`; see "Color" in CLAUDE.md.

## 4. Map pages are built differently, on purpose

Four interactive maps ship on the site. They share a look but not a build path.

| map | built with | why |
|---|---|---|
| bee bounty (x2), transect | R `leaflet()` + `addProviderTiles` | Static layers with popups. The R package writes the whole widget, so there is no JavaScript to maintain. |
| occurrence explorer | hand-written Leaflet JS | It filters 12,000+ points live on genus, species, year, and method. The R package cannot express that, so the page carries its own `L.map()` and draws its markers itself. |

**What every map shares:**

- **Control row.** Zoom, basemap, north arrow, and scale sit in ONE row, bottom-right,
  just above the credit. The R/leaflet maps get this from `BEE_MAP_CTRLROW_CSS` and
  `BEE_MAP_CTRLROW_JS` in `theme_beescabr.R`, which add the four controls normally and
  then move their DOM into a single container. **The occurrence explorer has its own copy**,
  because it never had R controls to relocate. That is the one place the design is
  duplicated: change the control row and you must change it in both.
- **Colours** from `BEE_TRANSECT`, `BEE_MAP`, and `BEE_MAP_CHROME`.
- **Basemaps:** Esri World Topo by default, with Imagery, CartoDB Positron, and OSM in
  the switcher.
- **Title card, top-left**, in this order: eyebrow, title with emoji, a one or two
  sentence intro, any caveat, then a brief source line.
- **Legend bottom-left**, headed "Transects" where transects appear.

Maps are exempt from the page-layout standard in section 3. That standard ends with
provenance at the bottom of the page, and a map has no bottom, so its source line lives
in the title card instead.

## 5. Checking your work

The in-app preview browser renders Leaflet controls differently from a real browser: the
control row can appear scattered and the zoom animation is dropped. **Do not treat that as
a regression.** Compare the generated HTML against what is committed before believing a
layout has broken, or open the file in a normal browser.
