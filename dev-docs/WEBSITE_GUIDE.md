# The public website

**[randombirdlover.github.io/beescabr](https://randombirdlover.github.io/beescabr/)**

GitHub Pages serves `docs/` from `main`. No build step, no framework — each page is
one self-contained HTML file.

## How a page gets there

```
scripts/analysis/<topic>/some_page.R
        ↓  writes
data/analysis/<year>_generated/<topic>/website/some_page.html
        ↓  publish_pages.R copies + wraps it
docs/some_page.html
```

### Three folders are called "website". They are not the same thing.

| | | in git? |
|---|---|---|
| `scripts/website/` | the **code** that builds and publishes pages | yes |
| `data/**/website/` | **drafts** — each analysis writes its HTML beside the figures it came from | no |
| `docs/` | the **live site** — what GitHub Pages serves | yes, committed |

**Why `docs/` exists at all**, given the drafts are already HTML:

1. **`data/` is gitignored**, so GitHub never sees it. A folder outside the repo
   cannot be served.
2. **`docs/` is a gate.** Building writes drafts; publishing copies them here;
   committing and pushing makes them public. Three separate acts, so you can
   rebuild and look before anything reaches a reader.
3. **The names change.** Drafts are named for analysts, `docs/` names are URLs:
   `bee_field_guide_species.html` → `field_guide_species.html`.

`docs/` also holds pages with no analysis behind them — `index.html`,
`acknowledgements.html` — built by `build_content_pages.R`. So the two folders
never match one-for-one.

**To add one:** write the script to output one self-contained HTML file, then add a
row to `PUBLISH_PAGES` in `scripts/website/publish_pages.R` and to `PUBLIC_PAGES` in
the publishing runner.

Two guards stop a bad publish, and both leave `docs/` untouched:
a missing page **stops** the run, and a stale analysis warns.

## Page layout — the fixed order

```
  1  Title + byline
  2  Lead ......................... EXACTLY one sentence
  3  Key .......................... only if the page uses codes; a list, never prose
  4  The one thing to know ........ ONE box, not a stack of warnings
  5  The table
  6  Column keys .................. under the table (.tkey)
  7  Provenance ................... last (.scope-foot, "About this data")
```

| Rule | Why |
|---|---|
| **Each block looks different from its neighbours** | Seven identical paragraphs doing three jobs is what made these unreadable |
| **Never repeat the lead in the note below it** | Say the new thing only |
| **Provenance goes last** | It is what you check when a number looks wrong, not before |
| **Column keys go under the table, not on hover** | A phone cannot hover |

Maps are exempt — they have their own shape.

## Colour

**Never write a hex value in a page script.** Every colour comes from
`theme_beescabr.R`. `bee_table_css()` carries `.scope-foot`, `.tkey` and `.cov`, so
using the classes gets you the styling.

See `BEE_THEMES_PALLETE.png` in this folder for the live swatch.

## Publishing from your own account

Two settings live on GitHub, not in this repository, so neither survives a clone.
**The publish pipeline prints these on your first build** — you don't have to
remember them.

| | |
|---|---|
| **1. Point `origin` at your own repo** | `system("git remote set-url origin https://github.com/<you>/beescabr")` in the R console |
| **2. Turn on Pages** | your repo → Settings → Pages → Source: Deploy from a branch → **main** / **/docs** → Save |

Your site then appears at `https://<you>.github.io/beescabr`.

### A fork publishes its own site. Decide which one is the real one.

Forking gives the fork its own `docs/` and its own Pages URL. Both sites stay live
and neither updates the other — they are separate publications of the same code.

| | |
|---|---|
| **Code** | flows one way: fork → pull request → upstream. Only merged changes reach everyone. |
| **`docs/`** | does **not** flow. Each repo publishes whatever is in its own `docs/`. |
| **Data** | never in git at all, so nothing about it is shared by forking. |

So agree who publishes. The sensible answer is **whoever currently runs the
project** — its site is the one to link from anywhere official, and the other
should stop publishing rather than drift into a second, older version of the same
pages. A stale public page is worse than no page: nothing on it says how old it is.

If both keep publishing, keep `docs/` out of pull requests. Two people rebuilding
the same twelve HTML files guarantees a conflict on every merge, and resolving it
by hand is how a half-built page reaches a reader.

```r
source("scripts/run_publishing_materials_pipeline.R")   # build only
```

Then commit and push `docs/` when you are happy. Safer than auto-deploy —
`docs/` is the one output the public sees.

To build **and** commit + push `docs/` in one step:

```r
Sys.setenv(BEESCABR_DEPLOY = "1"); source("scripts/run_publishing_materials_pipeline.R")
```

That publishes the moment it finishes. Use the two-step version unless you have
already looked at the pages.
