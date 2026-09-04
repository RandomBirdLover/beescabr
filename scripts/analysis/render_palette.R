# =============================================================
# analysis/render_palette.R
# Renders a one-page reference sheet of the LIVE theme tokens (single source of
# truth: scripts/analysis/shared/theme_beescabr.R). Regenerated with the rest of the
# analysis figures; re-run alone after any token change:
#   source("scripts/analysis/render_palette.R")
# Output: dev-docs/BEE_THEMES_PALLETE.png -- it lives with the docs, not under data/,
# because data/ is gitignored and the palette swatch is reference material a developer
# (or Taro) needs to be able to see in the repo.
# =============================================================
if (!exists("BEE_TRANSECT")) source("scripts/analysis/shared/theme_beescabr.R")
OUT <- "dev-docs"; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ---- website design colors, parsed LIVE from their sources so the sheet never goes stale ----
# landing page CSS variables live in publish_pages.R (:root light set first, dark set second)
# The landing CSS carries __W_BG__ tokens, not hex literals -- that is the rule in
# CLAUDE.md, because a hex typed into a template is a color the theme cannot reach.
# Resolve them first, or the regex below matches nothing and the sheet renders two
# empty rows (which it silently did until tests/testthat/test-palette-sheet.R).
.pp <- beescabr_fill_colors(readLines("scripts/website/publish_pages.R", warn = FALSE))
.vars <- regmatches(.pp, gregexpr("--[a-z0-9-]+ *: *#[0-9a-fA-F]{3,8}", .pp))
.vars <- unlist(.vars)
.vn <- sub(" *:.*$", "", .vars); .vh <- sub("^.*: *", "", .vars)
.first <- !duplicated(.vn)
LAND_LIGHT <- setNames(.vh[.first],  sub("^--", "", .vn[.first]))
LAND_DARK  <- setNames(.vh[!.first], sub("^--", "", .vn[!.first]))
.hero <- regmatches(.pp, regexpr("hero \\{[^}]*background:#[0-9a-fA-F]{6}", .pp))
HERO_BG <- if (length(.hero <- unlist(.hero))) sub("^.*background:", "", .hero[1]) else BEE_SITE[["head"]]

# Palettes deliberately NOT drawn below, and why. Everything else in
# theme_beescabr.R must appear on the sheet -- tests/testthat/test-palette-sheet.R
# fails if a new palette is added and this file is not.
PALETTE_NOT_CHARTED <- c(
  BEE_COLOR_TOKENS = "the __C_NAME__ registry -- it is every color above, re-listed",
  BEE_SEQ          = "an alias of BEE_TEAL, already drawn as TEAL (non-urgent)",
  BEE_SITE_DARK    = "the same nine colors as LANDING dark, which is drawn below"
)

rows <- list(
  list("TRANSECT",       "4 transects (BST/UPMON/TP/OT) -- full",      unname(BEE_TRANSECT),     names(BEE_TRANSECT), TRUE),
  list("TRANSECT light", "raw / lower bar tint (e.g. observed)",       unname(BEE_TRANSECT_LT),  names(BEE_TRANSECT_LT), TRUE),
  list("METHOD",         "specimen (net) vs photo (iNat)",             unname(BEE_METHOD_COL),   c("lethal (net)","non-lethal (photo)"), TRUE),
  list("METHOD light",   "still-unresolved side of a method bar",      unname(BEE_METHOD_COL_LT),c("lethal","non-lethal"), TRUE),
  list("OBSERVER",       "who surveyed -- beeple vs interns (method held constant)",
       unname(BEE_OBSERVER_COL), unname(BEE_OBSERVER_LABEL), TRUE),
  list("OBSERVER light", "lower / still-unresolved half of an observer bar",
       unname(BEE_OBSERVER_COL_LT), names(BEE_OBSERVER_COL_LT), TRUE),
  list("TEAL (non-urgent)", "ONE ramp -> evidence + magnitude + neutrals all draw from this", BEE_TEAL, rep("",length(BEE_TEAL)), TRUE),
  list("RARE / URGENT",  "rare bees / act-here -- the red pop (crimson ramp)", BEE_RARE,          rep("",length(BEE_RARE)), TRUE),
  list("WEB nodes",      "plant vs bee (interaction webs)",            c(BEE_WEB[["plant"]], BEE_WEB[["bee"]]), c("plant","bee"), TRUE),
  list("FAMILY",         "bee families",                              unname(BEE_FAMILY),        names(BEE_FAMILY), TRUE),
  list("WEB genus",      "selective bee genera on the webs",           BEE_GENUS,                rep("",length(BEE_GENUS)), FALSE),
  list("SPECIES",        "per-species palette (focused webs)",         BEE_SPECIES,              rep("",length(BEE_SPECIES)), FALSE),
  list("SEASON",         "blue Nov-Feb; green from end-Feb; orange summer; warm tan fall", BEE_SEASON,            rep("",length(BEE_SEASON)), FALSE),
  list("NPS footprint",  "CABR footprint figures -- own NPS theme",    unname(BEE_NPS),          names(BEE_NPS), TRUE),
  list("NPS magnitude",  "forest-green ramp (footprint lollipop)",     NPS_SEQ,                  rep("",length(NPS_SEQ)), TRUE),
  list("EVIDENCE",       "ID confidence: deep voucher -> pale needs-ID (teal stops)",
       unname(BEE_EVIDENCE),    names(BEE_EVIDENCE), TRUE),
  list("SET overlap",    "venn / shared-vs-unique sets (NOT the method venn)",
       unname(BEE_SET),         names(BEE_SET), TRUE),
  list("ACCENTS",        "single reserved colors -- do not reuse for a series",
       c(BEE_SELECT, BEE_FAVORITE, BEE_METHOD_BOTH, BEE_GENUS_GREY),
       c("select","favorite","both methods","too few"), TRUE),
  list("SLIDE",          "drawn bee + paper (rare_bee_plants slide figures)",
       unname(BEE_SLIDE),       names(BEE_SLIDE), TRUE),
  # ---- pills: a background paired with its text color, one row each ----
  list("PREF pills",     "forage preference: selective / generalist / n-a",
       unname(BEE_PREF),        names(BEE_PREF), TRUE),
  list("DIET pills",     "diet breadth backgrounds (field guides)",
       unname(BEE_DIET_BG),     names(BEE_DIET_BG), TRUE),
  list("DIET text",      "pairs with the row above",
       unname(BEE_DIET_FG),     names(BEE_DIET_FG), TRUE),
  list("FORAGE pills",   "forage-preference pill backgrounds",
       unname(BEE_FORAGE_BG),   names(BEE_FORAGE_BG), TRUE),
  list("FORAGE text",    "pairs with the row above",
       unname(BEE_FORAGE_FG),   names(BEE_FORAGE_FG), TRUE),
  list("COVERAGE pills", "method coverage: both / photo-only / specimen-only",
       unname(BEE_COVERAGE_BG), names(BEE_COVERAGE_BG), TRUE),
  list("COVERAGE text",  "pairs with the row above",
       unname(BEE_COVERAGE_FG), names(BEE_COVERAGE_FG), TRUE),
  list("ABUNDANCE pills","abundance status: rare / uncommon / common",
       unname(BEE_ABUND_BG),    names(BEE_ABUND_BG), TRUE),
  list("ABUNDANCE text", "pairs with the row above",
       unname(BEE_ABUND_FG),    names(BEE_ABUND_FG), TRUE),
  list("BADGE",          "callout badge on a guide page",
       unname(BEE_BADGE),       names(BEE_BADGE), TRUE),
  # ---- maps + document-style page chrome ----
  list("MAP surfaces",   "land, inset, boundary -- leaflet + static maps",
       unname(BEE_MAP),         names(BEE_MAP), TRUE),
  list("MAP controls",   "zoom / basemap / north / scale card chrome",
       unname(BEE_MAP_CHROME),  names(BEE_MAP_CHROME), TRUE),
  list("PANEL chrome",   "guides, explorers, summary tables",
       unname(BEE_PANEL),       names(BEE_PANEL), TRUE),
  # ---- website design (the HTML pages + landing site) ----
  list("WEBSITE green",  "peridot accent from the hero bee photo",
       unname(BEE_HTML_GREEN),  names(BEE_HTML_GREEN), TRUE),
  list("WEBSITE chrome", "table-page surfaces + text (BEE_HTML tokens)",
       unname(BEE_HTML),        names(BEE_HTML), TRUE),
  list("TABLE chrome",   "PNG summary tables: zebra stripes + text",
       unname(BEE_TABLE),       names(BEE_TABLE), TRUE),
  list("IUCN chip bg",   "Red List badge backgrounds (field guides + checklists)",
       unname(BEE_IUCN_BG),     toupper(names(BEE_IUCN_BG)), TRUE),
  list("IUCN chip text", "Red List badge text colors (pairs with the row above)",
       unname(BEE_IUCN_FG),     toupper(names(BEE_IUCN_FG)), TRUE),
  list("LANDING light",  "index.html CSS variables (light mode) + the hero band",
       c(unname(LAND_LIGHT), HERO_BG), c(names(LAND_LIGHT), "hero"), TRUE),
  list("LANDING dark",   "index.html CSS variables (dark mode)",
       unname(LAND_DARK),       names(LAND_DARK), TRUE)
)

n <- length(rows)
bee_png(file.path(OUT, "BEE_THEMES_PALLETE.png"), width = 2200, height = 160 * n + 220, res = 200)
par(mar = c(0.5, 0.5, 0.5, 0.5), family = "sans"); plot.new(); plot.window(c(0, 100), c(0, 100))
text(2, 98.5, "beescabr palette -- current theme tokens", adj = c(0, 1), font = 2, cex = 1.5, col = BEE_INK$primary)
text(2, 95.2, "single source of truth: scripts/analysis/shared/theme_beescabr.R", adj = c(0, 1), cex = 0.62, col = BEE_INK$muted)

top <- 91; rowh <- (top - 3) / n
for (i in seq_len(n)) {
  g <- rows[[i]]; yc <- top - (i - 0.5) * rowh
  yt <- yc + rowh * 0.30; yb <- yc - rowh * 0.30
  text(2,  yc + rowh * 0.06, g[[1]], adj = c(0, 0.5), font = 2, cex = 0.88, col = BEE_INK$primary)
  text(2,  yc - rowh * 0.26, g[[2]], adj = c(0, 0.5), cex = 0.56, col = BEE_INK$muted)
  cols <- as.character(g[[3]]); k <- length(cols); x0 <- 33; x1 <- 98; bw <- (x1 - x0) / k
  show_hex <- isTRUE(g[[5]])
  for (j in seq_len(k)) {
    xl <- x0 + (j - 1) * bw
    rect(xl + 0.4, yb, xl + bw - 0.4, yt, col = cols[j], border = "grey70")
    tc <- if (mean(col2rgb(cols[j])) < 130) "white" else "grey15"
    if (show_hex) text((2*xl + bw)/2, (yt + yb)/2 + rowh*0.05, toupper(cols[j]), cex = 0.50, col = tc)
    if (nzchar(g[[4]][j])) text((2*xl + bw)/2, yb - rowh*0.12, g[[4]][j], cex = 0.52, col = BEE_INK$secondary, adj = c(0.5, 1))
  }
}
dev.off(); cat("wrote", file.path(OUT, "BEE_THEMES_PALLETE.png"), "\n")
