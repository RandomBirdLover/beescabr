# =============================================================
# scripts/publish/build_content_pages.R
# Generates the site's CONTENT pages (as opposed to the data/analysis pages):
# currently the Acknowledgements page. Reads data/project_info/*_roster.csv so
# the people lists stay in sync with the rosters. Matches the landing page look.
#
#   Rscript scripts/publish/build_content_pages.R        # writes docs/acknowledgements.html
#
# About + Get-involved pages will join this file once their copy is written.
# =============================================================
suppressPackageStartupMessages(library(dplyr))

DOCS <- "docs"
PHOTO_DIR <- "data/project_info/research_team_photos"   # gitignored; raw headshots embedded into the page
AVATAR_PX <- 640                                         # downscale longest side before embedding (headroom for zoomed crops)
rd <- function(p) if (file.exists(p)) read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) else NULL
esc <- function(x) { x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x) }
sq  <- function(x) trimws(ifelse(is.na(x), "", as.character(x)))

# ---- survey team (surveyor_roster.csv) ---------------------------------------
r <- rd("data/project_info/surveyor_roster.csv")
r$first_name <- sq(r$first_name); r$last_name <- sq(r$last_name)
r <- r[r$first_name != "" | r$last_name != "", ]
r$name <- trimws(paste(r$first_name, r$last_name))
r$user <- sq(r$inaturalist_username)
team <- r %>% group_by(name) %>%
  summarise(first = first(first_name), last = first(last_name),
            user = { u <- user[user != ""]; if (length(u)) u[[1]] else "" }, .groups = "drop")

# order the survey team by total records submitted (iNat observations + specimens),
# tallied from the MASTER per-survey doc (not the clean sheets, so intern net-survey
# specimens are attributed to the individual interns who ran them).
mp <- rd("data/project_info/master_per_survey_info.csv")
num0 <- function(x) { x <- suppressWarnings(as.numeric(x)); ifelse(is.na(x), 0, x) }
mp$rec <- num0(mp$n_obs) + num0(mp$n_speci)
uu <- tolower(team$user); has <- nzchar(uu) & !is.na(uu)
key_user  <- setNames(team$name[has], uu[has])                                   # iNat username -> person (unambiguous)
key_first <- setNames(team$name, tolower(team$first))                            # first name -> person
key_init  <- setNames(team$name, tolower(paste0(substr(team$first, 1, 1), " ", team$last)))  # "F Lastname" -> person
recs <- setNames(numeric(nrow(team)), team$name)
for (i in seq_len(nrow(mp))) {
  u <- tolower(trimws(mp$inat_username[i]))
  if (nzchar(u) && u != "n/a" && !is.na(key_user[u])) {                          # iNat survey: credit the observer
    recs[key_user[u]] <- recs[key_user[u]] + mp$rec[i]
  } else for (tk in trimws(unlist(strsplit(mp$surveyors[i], "[,;&]")))) {        # net/specimen: credit each collector
    t <- tolower(tk); if (!nzchar(t)) next
    p <- if (!is.na(key_first[t])) key_first[t] else if (!is.na(key_init[t])) key_init[t] else NA_character_
    if (!is.na(p)) recs[p] <- recs[p] + mp$rec[i]
  }
}
team$recs <- recs[team$name]
team <- team[order(-team$recs, tolower(team$last), tolower(team$name)), , drop = FALSE]
yr_span <- { y <- suppressWarnings(as.integer(r$year)); y <- y[!is.na(y)]
             if (length(y)) as.character(min(y)) else "" }   # start year only -> "Since 2021"

# ---- identifiers (identifier_roster.csv -- may be empty for now) --------------
taxa_label <- function(x) { x <- tolower(trimws(x %||% "")); ifelse(is.na(x), "",
  ifelse(x == "bee", "bees", ifelse(x == "plant", "plants", ifelse(x == "both", "bees & plants", x)))) }
`%||%` <- function(a, b) if (is.null(a)) b else a
id <- rd("data/project_info/identifier_roster.csv")
ids <- if (!is.null(id) && nrow(id)) {
  id$name <- trimws(paste(sq(id$first_name), sq(id$last_name)))
  id$name <- ifelse(id$name == "" & sq(id$inaturalist_username) != "", sq(id$inaturalist_username), id$name)  # username-only entries (e.g. itazura) fall back to the handle
  id <- id[id$name != "", , drop = FALSE]
  # plant-only identifiers sort AFTER everyone else (the bulk of the work is bees);
  # within each group, order by combined ID effort most->least. id_count is sort-only
  # metadata (never displayed, like email); blanks last. "both" stays in the bee group.
  cnt <- if ("id_count" %in% names(id)) suppressWarnings(as.integer(id$id_count)) else rep(NA_integer_, nrow(id))
  cnt[is.na(cnt)] <- -1L
  plant_last <- as.integer(tolower(sq(id$taxa_identified)) == "plant")
  id[order(plant_last, -cnt, tolower(sq(id$last_name)), tolower(id$name)), , drop = FALSE]
} else id[0, , drop = FALSE]

# ---- render helpers ----------------------------------------------------------
person_chip <- function(name, user, extra = "") {
  same <- nzchar(user) && tolower(name) == tolower(user)   # username-only person (e.g. itazura): link the name, no @handle
  nm <- if (same) sprintf('<a class="inat" href="https://www.inaturalist.org/people/%s">%s</a>', esc(user), esc(name)) else esc(name)
  handle <- if (nzchar(user) && !same) sprintf(' <a class="inat" href="https://www.inaturalist.org/people/%s">@%s</a>',
                                                esc(user), esc(user)) else ""
  sub <- if (nzchar(extra)) sprintf('<span class="chip-sub">%s</span>', esc(extra)) else ""
  sprintf('<div class="chip"><span class="chip-name">%s</span>%s%s</div>', nm, handle, sub)
}

# Embed a team photo as a data: URI so the published page stays self-contained.
# The source files live under the gitignored PHOTO_DIR, so a missing file just
# yields "" and the chip falls back to an initials circle. Raw headshots are
# multi-MB; since the avatar renders at 52px we downscale to AVATAR_PX (longest
# side) with macOS `sips` first, keeping the embedded page small. The in-browser
# object-fit:cover does the square crop, so no server-side cropping is needed. If
# sips is unavailable or fails, we fall back to embedding the source as-is.
photo_datauri <- function(fname, target_px = AVATAR_PX) {
  if (!nzchar(fname)) return("")
  src <- file.path(PHOTO_DIR, fname)
  if (!file.exists(src)) return("")
  small <- src
  if (nzchar(Sys.which("sips"))) {
    cache <- file.path(tempdir(), "beescabr_avatars"); dir.create(cache, showWarnings = FALSE)
    # Always re-encode to a modest-quality JPEG (invisible at avatar size, far smaller
    # than the raw). Size tier is in the name so zoom tiers do not collide.
    out <- file.path(cache, sprintf("%d_%s.jpg", target_px, tools::file_path_sans_ext(fname)))
    ok <- tryCatch(system2("sips", c("-Z", as.character(target_px), "-s", "format", "jpeg",
                                     "-s", "formatOptions", "65", src, "-o", out),
                           stdout = FALSE, stderr = FALSE) == 0, error = function(e) FALSE)
    if (ok && file.exists(out)) small <- out
  }
  ext  <- tolower(tools::file_ext(small))
  mime <- if (ext == "png") "png" else if (ext == "webp") "webp" else "jpeg"
  sprintf("data:image/%s;base64,%s", mime,
          jsonlite::base64_enc(readBin(small, "raw", file.info(small)$size)))
}

# Like person_chip but with a circular headshot (or initials fallback) on the left.
# `credit` (the photographer/source) rides along as the image title/alt tooltip; the
# section also gathers the credits into one footnote line under the grid.
research_chip <- function(first, last, name, user, role, aff, photo, credit = "", focus = "", zoom = "") {
  zn  <- suppressWarnings(as.numeric(zoom)); if (is.na(zn) || zn < 1) zn <- 1
  tp  <- min(1024L, max(as.integer(AVATAR_PX), as.integer(ceiling(AVATAR_PX * zn))))  # zoomed crops embed sharper
  uri <- photo_datauri(photo, tp)
  ini <- toupper(paste0(substr(first, 1, 1), substr(last, 1, 1)))
  ttl <- if (nzchar(credit)) sprintf(' title="Photo: %s"', esc(credit)) else ""
  if (nzchar(uri)) {
    # An <img> clipped by the circular .avatar wrapper. Per-person framing: focus
    # (object-position, also the zoom anchor) pans the head into view; zoom is a
    # scale factor beyond object-fit:cover (blank/1 = plain cover, centered).
    ist <- character(0)
    if (nzchar(focus)) ist <- c(ist, sprintf("object-position:%s", esc(focus)), sprintf("transform-origin:%s", esc(focus)))
    if (nzchar(zoom))  ist <- c(ist, sprintf("transform:scale(%s)", esc(zoom)))
    sty <- if (length(ist)) sprintf(' style="%s"', paste(ist, collapse = ";")) else ""
    av  <- sprintf('<div class="avatar"><img alt="%s"%s%s src="%s"></div>', esc(name), ttl, sty, uri)
  } else av <- sprintf('<div class="avatar avatar-blank">%s</div>', esc(ini))
  same   <- nzchar(user) && tolower(name) == tolower(user)
  nm     <- if (same) sprintf('<a class="inat" href="https://www.inaturalist.org/people/%s">%s</a>', esc(user), esc(name)) else esc(name)
  handle <- if (nzchar(user) && !same) sprintf(' <a class="inat" href="https://www.inaturalist.org/people/%s">@%s</a>', esc(user), esc(user)) else ""
  rl     <- if (nzchar(role)) sprintf('<span class="chip-role">%s</span>', esc(role)) else ""   # role as a proper-cased subtitle
  af     <- if (nzchar(aff))  sprintf('<span class="chip-aff">%s</span>', esc(aff))   else ""
  sprintf('<div class="chip chip-person">%s<div class="chip-text"><span class="chip-name">%s</span>%s%s%s</div></div>', av, nm, handle, rl, af)
}
team_html <- paste(mapply(function(n, u) person_chip(n, u), team$name, team$user), collapse = "\n")

if (nrow(ids)) {
  id_html <- paste(vapply(seq_len(nrow(ids)), function(i) {
    bits  <- c(sq(ids$affiliation[i]), taxa_label(ids$taxa_identified[i]))
    extra <- paste(bits[nzchar(bits)], collapse = " · ")
    person_chip(ids$name[i], sq(ids$inaturalist_username[i]), extra)
  }, character(1)), collapse = "\n")
  id_html <- paste0(id_html,
    '\n<p class="thanks">And to the rest of the iNaturalist community who have helped identify our bees, thank you.</p>')
} else {
  id_html <- '<p class="pending">The specialists who identified specimens and confirmed photo records are being compiled, and will be credited here. (Fill <code>data/project_info/identifier_roster.csv</code>.)</p>'
}

partners <- list(
  c("Cabrillo National Monument", "https://www.nps.gov/cabr/"),
  c("National Park Service", "https://www.nps.gov/"),
  c("San Diego Natural History Museum", "https://www.sdnhm.org/"),
  c("UC San Diego", "https://www.ucsd.edu/"))
partner_html <- paste(vapply(partners, function(p)
  sprintf('<a class="partner" href="%s">%s</a>', p[2], esc(p[1])), character(1)), collapse = "\n")

# ---- page --------------------------------------------------------------------
css <- '
  :root{--bg:#f3f8f1;--bg2:#e7f2e4;--fg:#1a271b;--muted:#5c6d5b;--card:#ffffff;--border:#d7e6d2;
    --accent:#3f8f4f;--accent-deep:#1c5c28;--accent-soft:#dcf1e1;--shadow:0 1px 2px rgba(24,50,26,.05),0 8px 24px rgba(24,50,26,.07);}
  @media (prefers-color-scheme:dark){:root{--bg:#0f170e;--bg2:#0a120a;--fg:#e7efe5;--muted:#93a491;--card:#161f13;
    --border:#26391f;--accent:#66bd77;--accent-deep:#91dba0;--accent-soft:#152e1a;--shadow:0 1px 2px rgba(0,0,0,.3),0 10px 30px rgba(0,0,0,.35);}}
  *{box-sizing:border-box}
  body{margin:0;color:var(--fg);line-height:1.55;background:linear-gradient(180deg,var(--bg) 0%,var(--bg2) 100%);
    background-attachment:fixed;min-height:100vh;font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;-webkit-font-smoothing:antialiased}
  a{color:var(--accent-deep)}
  .head{background:#16302b;border-bottom:1px solid var(--border);padding:3.2rem 1.5rem 2.4rem}
  .head-inner{max-width:820px;margin:0 auto}
  .eyebrow{display:inline-block;font-size:.72rem;letter-spacing:.14em;text-transform:uppercase;font-weight:700;color:#fff;margin:0 0 .7rem;
    padding:.3rem .7rem;background:rgba(255,255,255,.16);border:1px solid rgba(255,255,255,.22);border-radius:999px}
  .head h1{margin:0 0 .5rem;font-size:2.2rem;line-height:1.14;letter-spacing:-.02em;font-weight:800;color:#fff}
  .head p{color:rgba(255,255,255,.9);margin:0;max-width:58ch;font-size:1.05rem}
  main{max-width:820px;margin:0 auto;padding:2rem 1.5rem 3.5rem}
  section{margin:0 0 2.4rem}
  h2{font-size:1.05rem;letter-spacing:.02em;color:var(--accent-deep);margin:0 0 .3rem}
  .sec-note{color:var(--muted);font-size:.9rem;margin:0 0 1rem}
  .grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(210px,1fr));gap:.6rem}
  .chip{background:var(--card);border:1px solid var(--border);border-radius:12px;padding:.6rem .8rem;box-shadow:var(--shadow)}
  .chip-name{font-weight:600}
  .chip .inat{margin-left:.35rem;font-size:.82rem;text-decoration:none;color:var(--accent)}
  .chip-sub{display:block;font-size:.78rem;color:var(--muted);margin-top:.15rem}
  .grid-team{grid-template-columns:repeat(auto-fill,minmax(250px,1fr))}
  .chip-person{display:flex;align-items:flex-start;gap:.7rem}
  .chip-person .chip-name{display:block}
  .chip-person .inat{margin-left:0}
  .avatar{width:52px;height:52px;border-radius:50%;overflow:hidden;flex:none;border:1px solid var(--border)}
  .avatar img{width:100%;height:100%;object-fit:cover;display:block}
  .avatar-blank{display:flex;align-items:center;justify-content:center;background:var(--accent-soft);color:var(--accent-deep);font-weight:700;font-size:.9rem}
  .chip-text{min-width:0}
  .chip-role{display:block;font-size:.82rem;font-weight:600;color:var(--fg);margin-top:.2rem}
  .chip-aff{display:block;font-size:.76rem;color:var(--muted);margin-top:.12rem}
  .photo-credit{font-size:.78rem;font-style:italic;margin:.7rem 0 0}
  .pending{color:var(--muted);font-size:.92rem;background:var(--accent-soft);border:1px solid var(--border);border-radius:12px;padding:.9rem 1rem}
  .thanks{color:var(--muted);font-size:.95rem;font-style:italic;margin:1.1rem 0 0}
  .pending code{font-size:.85em}
  .partners{display:flex;flex-wrap:wrap;gap:.6rem}
  .partner{background:var(--card);border:1px solid var(--border);border-radius:999px;padding:.5rem 1rem;text-decoration:none;color:var(--fg);font-weight:600;font-size:.92rem;box-shadow:var(--shadow)}
  .backlink{display:inline-block;margin:0 0 1.4rem;color:var(--accent-deep);text-decoration:none;font-size:.9rem}
  footer{max-width:820px;margin:0 auto;padding:0 1.5rem 3.5rem;color:var(--muted);font-size:.85rem;border-top:1px solid var(--border);padding-top:1.5rem}
'

# ---- main research team (research_team_roster.csv) --------------------------
rt <- rd("data/project_info/research_team_roster.csv")
if (!is.null(rt) && nrow(rt)) { rt$name <- trimws(paste(sq(rt$first_name), sq(rt$last_name))); rt <- rt[rt$name != "", , drop = FALSE] } else rt <- NULL
research_section <- if (!is.null(rt) && nrow(rt)) {
  has_photo  <- "photo" %in% names(rt)
  has_credit <- "photo_credit" %in% names(rt)
  has_focus  <- "photo_focus" %in% names(rt)
  has_zoom   <- "photo_zoom"  %in% names(rt)
  creds <- character(0)
  chips <- paste(vapply(seq_len(nrow(rt)), function(i) {
    photo  <- if (has_photo)  sq(rt$photo[i])        else ""
    credit <- if (has_credit) sq(rt$photo_credit[i]) else ""
    focus  <- if (has_focus)  sq(rt$photo_focus[i])  else ""
    zoom   <- if (has_zoom)   sq(rt$photo_zoom[i])   else ""
    if (nzchar(photo) && nzchar(credit) && file.exists(file.path(PHOTO_DIR, photo)))
      creds[[length(creds) + 1L]] <<- sprintf("%s (%s)", esc(rt$name[i]), esc(credit))
    research_chip(sq(rt$first_name[i]), sq(rt$last_name[i]), rt$name[i],
                  sq(rt$inaturalist_username[i]), sq(rt$role[i]), sq(rt$affiliation[i]),
                  photo, credit, focus, zoom)
  }, character(1)), collapse = "\n")
  credit_note <- if (length(creds))
    sprintf('\n    <p class="sec-note photo-credit">Photographs: %s.</p>', paste(creds, collapse = "; ")) else ""
  sprintf('<section>\n    <h2>Main Research Team</h2>\n    <p class="sec-note">The people who lead the native bee monitoring program.</p>\n    <div class="grid grid-team">%s</div>%s\n  </section>\n  ', chips, credit_note)
} else ""

intro <- if (nzchar(yr_span))
  sprintf("The Cabrillo native bee monitoring program is the work of many hands. Since %s, these are the people and partners who made it possible.", yr_span) else
  "The Cabrillo native bee monitoring program is the work of many hands. These are the people and partners who made it possible."

html <- sprintf('<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Cabrillo National Monument &mdash; Acknowledgements</title>
<style>%s</style></head><body>
<div class="head"><div class="head-inner">
  <span class="eyebrow">&#127963;&#65039; Cabrillo National Monument</span>
  <h1>Acknowledgements</h1>
  <p>%s</p>
</div></div>
<main>
  <a class="backlink" href="./index.html">&larr; Back to the program</a>
  %s<section>
    <h2>Survey Team</h2>
    <p class="sec-note">The people who walked the transects, photographed bees, and collected specimens (%d people). iNaturalist handles link to their profiles.</p>
    <div class="grid">%s</div>
  </section>
  <section>
    <h2>Identification Team</h2>
    <p class="sec-note">The specialists who put names to the bees and plants!</p>
    %s
  </section>
  <section>
    <h2>Partners</h2>
    <p class="sec-note">Institutions that host the program, the collections, and the data.</p>
    <div class="partners">%s</div>
  </section>
</main>
<footer>Generated from the beescabr pipeline by Brandi Sanchez. Native Bee Monitoring Program, Cabrillo National Monument.</footer>
</body></html>',
  css, esc(intro), research_section, nrow(team), team_html, id_html, partner_html)

dir.create(DOCS, showWarnings = FALSE)
writeLines(html, file.path(DOCS, "acknowledgements.html"))
message(sprintf("Wrote %s/acknowledgements.html  (%d survey team, %d identifiers)",
                DOCS, nrow(team), nrow(ids)))
