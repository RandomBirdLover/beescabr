# =============================================================
# publish_pages.R
# beescabr pipeline -- PUBLISH: copy the PUBLIC report HTML pages into docs/
# (tracked) so GitHub Pages can serve them, build the Acknowledgements page, and
# (re)build the docs/index.html landing page.
#
# The generated report HTML lives under data/analysis/ (gitignored), so this
# script copies the chosen public pages into docs/ with clean, stable filenames.
# Run it after the pipeline whenever the pages change:
#     Rscript scripts/publish/publish_pages.R
# then commit docs/ and push. GitHub Pages source = main branch, /docs folder.
#
# This is the R replacement for the old publish_pages.sh -- same docs/ output,
# no shell. Only the PUBLIC report set is published here; internal QA tools
# (per-surveyor pins-to-fix pages, reviewer drafts) are deliberately excluded.
# =============================================================

SRC_DIR  <- "data/analysis/nps_report_2026"
DOCS_DIR <- "docs"

# One manifest drives both the file copy and the landing-page cards.
# ADD NEW PUBLIC PAGES HERE (src is relative to SRC_DIR; out is the docs/ name).
PUBLISH_PAGES <- list(
  list(src = "reference/field_guide/bee_field_guide_species.html", out = "field_guide_species.html",
       title = "Bee Field Guide (Species)", icon = "\U0001F41D", tag = "Field guide",
       blurb = "Every bee species recorded at Cabrillo, with photos, IUCN status, abundance, and forage."),
  list(src = "reference/field_guide/bee_field_guide_genus.html", out = "field_guide_genus.html",
       title = "Bee Field Guide (Genus)", icon = "\U0001F41D", tag = "Field guide",
       blurb = "The same guide grouped by genus for quicker browsing."),
  list(src = "reference/nps_summary/nps_summary_tables.html", out = "summary_tables.html",
       title = "Park Summary Tables", icon = "\U0001F4CA", tag = "Summary",
       blurb = "Headline counts: species, genera, plants, survey effort, and participation."),
  list(src = "coverage/least_sampled/least_sampled_bees.html", out = "least_sampled_bees.html",
       title = "Least-Sampled Bees", icon = "❗", tag = "Priorities",
       blurb = "The bees with the thinnest evidence, where more surveying would help most."),
  list(src = "reference/transects/cabr_bee_transects_map.html", out = "transects_map.html",
       title = "Survey Transect Map", icon = "\U0001F5FA\UFE0F", tag = "Map",
       blurb = "Interactive map of the fixed survey transects at Cabrillo National Monument."),
  list(src = "reference/occurrence_map/bee_occurrence_explorer.html", out = "occurrence_explorer.html",
       title = "Bee Occurrence Explorer", icon = "\U0001F50E", tag = "Explore",
       blurb = "Filter by genus, species, transect, and method to see exactly where each bee has been recorded."),
  list(src = "coverage/bee_bounties/specimen_bee_bounty_map.html", out = "specimen_bounty_map.html",
       title = "Specimen Bee Bounty Map", icon = "\U0001F52C", tag = "Map",
       blurb = "Where to net a voucher specimen: gaps the collection still needs."),
  list(src = "coverage/bee_bounties/inaturalist_bee_bounty_map.html", out = "inaturalist_bounty_map.html",
       title = "iNaturalist Bee Bounty Map", icon = "\U0001F4F7", tag = "Map",
       blurb = "Where to photograph bees to fill iNaturalist gaps.")
)

# Fill {{token}} placeholders in a template. gsub(fixed=TRUE) so literal % in the
# CSS is left alone (sprintf would choke on it).
.fill <- function(tpl, ...) {
  v <- list(...)
  for (nm in names(v)) tpl <- gsub(paste0("{{", nm, "}}"), v[[nm]], tpl, fixed = TRUE)
  tpl
}

# One landing-page card. Leading newline + indent matches the old heredoc exactly.
.CARD_TPL <- '
      <a class="card" href="./{{out}}">
        <div class="card-head"><span class="icon">{{icon}}</span><span class="tag">{{tag}}</span></div>
        <h2>{{title}}</h2>
        <p>{{blurb}}</p>
        <span class="go">Open<span class="arrow">&rarr;</span></span>
      </a>'

# Full landing page. {{cards}} / {{herov}} / {{date}} are filled in; everything
# else is verbatim the page the pipeline has always shipped.
.PAGE_TPL <- '<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Cabrillo National Monument &mdash; Native Bee Monitoring Program</title>
<style>
  :root {
    --bg:#f3f8f1; --bg2:#e7f2e4; --fg:#1a271b; --muted:#5c6d5b;
    --card:#ffffff; --border:#d7e6d2; --accent:#3f8f4f; --accent-deep:#1c5c28; --accent-soft:#dcf1e1;
    --shadow:0 1px 2px rgba(24,50,26,.05), 0 8px 24px rgba(24,50,26,.07);
    --shadow-hover:0 2px 6px rgba(24,50,26,.1), 0 16px 40px rgba(24,50,26,.14);
  }
  @media (prefers-color-scheme: dark) {
    :root {
      --bg:#0f170e; --bg2:#0a120a; --fg:#e7efe5; --muted:#93a491;
      --card:#161f13; --border:#26391f; --accent:#66bd77; --accent-deep:#91dba0; --accent-soft:#152e1a;
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
             object-fit:cover; object-position:52% 46%; transform:none; }
  .hero::after { content:""; position:absolute; inset:0; z-index:1; pointer-events:none;
                 background:linear-gradient(90deg, rgba(12,24,14,.74) 0%, rgba(12,24,14,.5) 42%, rgba(12,24,14,.32) 100%),
                            linear-gradient(180deg, rgba(12,24,14,.12) 0%, rgba(12,24,14,.55) 100%); }
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
  footer p { margin:0 0 .55rem; line-height:1.5; }
  footer a { color:var(--accent-deep); text-decoration:underline; text-underline-offset:2px; }
  footer p.ack { font-size:.95rem; margin-bottom:1rem; }
  footer p.ack a { font-weight:600; }
</style>
</head>
<body>
  <div class="hero">
    <img class="hero-bg" src="./hero.jpg?v={{herov}}" alt="">
    <div class="hero-inner">
      <span class="eyebrow">&#127963;&#65039; Cabrillo National Monument</span>
      <h1>Native Bee Monitoring Program</h1>
      <p class="lead">Native bee field guides, checklists, and interactive maps from the Cabrillo native-bee monitoring program. Pick a page to explore.</p>
    </div>
    <p class="credit"><a class="inat" href="https://www.inaturalist.org/observations/248210427" title="View on iNaturalist"><img src="./inat-logo.png" alt="iNaturalist" width="15" height="15"></a> Peridot Sweat Bee (<i>Augochlorella pomoniella</i>) on Wirelettuce (<i>Stephanomeria</i>) &middot; Brandi Sanchez &middot; <a href="https://creativecommons.org/licenses/by-nc/4.0/">CC BY-NC</a></p>
  </div>
  <main>{{cards}}
  </main>
  <footer>
    <p class="ack"><a href="./acknowledgements.html">Acknowledgements, learn about the team behind this work &rarr;</a></p>
    <p>With gratitude to <a href="https://www.nps.gov/cabr/">Cabrillo National Monument</a> and the <a href="https://www.nps.gov/">National Park Service</a> for their support of this monitoring program, and to the <a href="https://www.inaturalist.org/">iNaturalist</a> community whose shared observations made this work possible.</p>
    <p>Generated from the beescabr pipeline by Brandi Sanchez. Data as of {{date}}.</p>
  </footer>
</body>
</html>'

# Pure builder: given the cards to show (list of page specs), the data-as-of date,
# and the hero cache-buster, return the complete index.html as a string.
build_landing_html <- function(cards, date, hero_v) {
  cards_html <- paste0(vapply(cards, function(c)
    .fill(.CARD_TPL, out = c$out, icon = c$icon, tag = c$tag, title = c$title, blurb = c$blurb),
    character(1)), collapse = "")
  .fill(.PAGE_TPL, herov = hero_v, cards = cards_html, date = date)
}

# data-as-of date: read the same "data as of YYYY-MM-DD" the summary page prints,
# so the landing footer matches. Falls back to a phrase if the page is absent.
landing_date <- function() {
  f <- file.path(SRC_DIR, "reference/nps_summary/nps_summary_tables.html")
  if (file.exists(f)) {
    txt <- readLines(f, warn = FALSE)
    m <- regmatches(txt, regexpr("data as of [0-9]{4}-[0-9]{2}-[0-9]{2}", txt))
    if (length(m)) return(sub("data as of ", "", m[[1]]))
  }
  "the latest survey export"
}

# cache-buster: a short content hash of the hero so browsers re-fetch it whenever
# the photo changes (same md5-first-8 the shell version used).
hero_version <- function() {
  f <- file.path(DOCS_DIR, "hero.jpg")
  if (file.exists(f)) {
    h <- unname(tools::md5sum(f))
    if (!is.na(h) && nzchar(h)) return(substr(h, 1, 8))
  }
  "1"
}

# A small fixed "Back to main page" pill, wrapped in sentinels so re-runs replace
# rather than stack it. Injected into every copied report page so no page is a
# dead-end (they are full-screen maps/tables with no header of their own).
BACKLINK <- paste0(
  '<!--bx-back--><style>.bx-backlink{position:fixed;top:16px;right:60px;z-index:2147483000;',
  'display:inline-flex;align-items:center;gap:.35rem;padding:.4rem .8rem;border-radius:999px;',
  'background:rgba(22,48,43,.92);color:#fff;font:600 13px/1.1 -apple-system,BlinkMacSystemFont,',
  '"Segoe UI",Roboto,Helvetica,Arial,sans-serif;text-decoration:none;',
  'box-shadow:0 2px 8px rgba(0,0,0,.28);border:1px solid rgba(255,255,255,.2)}',
  '.bx-backlink:hover{background:rgba(28,92,40,.96)}</style>',
  '<a href="./index.html" class="bx-backlink">&larr; Back to main page</a><!--/bx-back-->')

inject_backlink <- function(path) {
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  html <- gsub("(?s)<!--bx-back-->.*?<!--/bx-back-->", "", html, perl = TRUE)  # idempotent
  if (grepl("<body[^>]*>", html)) html <- sub("(<body[^>]*>)", paste0("\\1", BACKLINK), html)
  else html <- paste0(BACKLINK, html)
  writeLines(html, path)
}

# Orchestrator: copy pages, build the content + landing pages, write docs/.
publish_pages <- function() {
  # locate repo root from the script path so it runs from anywhere (mirrors the
  # old `cd "$(dirname "$0")/../.."`). When sourced (tests) there is no --file=.
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(fa)) setwd(normalizePath(file.path(dirname(sub("^--file=", "", fa[1])), "..", "..")))

  dir.create(DOCS_DIR, showWarnings = FALSE, recursive = TRUE)
  nojekyll <- file.path(DOCS_DIR, ".nojekyll")            # serve files as-is, no Jekyll processing
  if (!file.exists(nojekyll)) file.create(nojekyll)

  # ---- copy each public page into docs/ ----
  present <- list()
  for (p in PUBLISH_PAGES) {
    src <- file.path(SRC_DIR, p$src)
    if (file.exists(src)) {
      file.copy(src, file.path(DOCS_DIR, p$out), overwrite = TRUE)
      inject_backlink(file.path(DOCS_DIR, p$out))     # add the "Back to main page" pill
      message("published  ", p$out)
      present[[length(present) + 1L]] <- p
    } else {
      message("SKIP (missing)  ", src)
    }
  }

  # ---- build the content pages (Acknowledgements, etc.) ----
  # build_content_pages.R writes docs/acknowledgements.html directly from the
  # rosters (self-contained; emails are never output). Sourced in its own
  # environment so its helpers do not leak into this one.
  source(file.path("scripts", "publish", "build_content_pages.R"), local = new.env())
  message("published  acknowledgements.html")

  # ---- landing page (docs/index.html) ----
  writeLines(build_landing_html(present, landing_date(), hero_version()),
             file.path(DOCS_DIR, "index.html"))
  message("built      index.html")
  message("Done. Review docs/, then commit and push. Enable GitHub Pages: Settings -> Pages -> main /docs.")
}

# Run only when executed as a script (Rscript), not when sourced by a test.
if (sys.nframe() == 0L) publish_pages()
