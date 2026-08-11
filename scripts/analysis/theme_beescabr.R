# =============================================================
# analysis/theme_beescabr.R
# beescabr -- SHARED figure house style (the ONE source of truth).
#
# Every analysis script sources this so colours, fonts, and spacing are identical
# across all figures. Change a value HERE and every figure updates -- a transect is
# the same colour everywhere, forever.
#
# DESIGN (each VARIABLE keeps ONE encoding everywhere -- colour-blind validated):
#   * TRANSECT -- the 4-colour Okabe-Ito CVD-safe palette below. These 4 hues appear ONLY as transects.
#   * METHOD (lethal net / non-lethal photo) -- its OWN two colours, off the transect palette:
#     rose-red (lethal) / periwinkle (non-lethal), softer jewel tones. Line style is a SECONDARY cue for the
#     one figure where a transect AND a method share a plot (lethal = SOLID, non-lethal = DASHED).
#   * EVIDENCE / ID-confidence -- a TEAL ordinal ramp (voucher dark -> needs-ID faint), so "less certain"
#     literally looks fainter. Teal keeps it distinct from the purple neutrals + off every transect hue.
#   * SCOPE / SET / ID-status -- single-figure accents from one small set: dark = focus/primary,
#     light = background/shared, orchid = the third/other category. Indigo-violet neutrals.
#   * MAGNITUDE (richness / counts) -- one crimson sequential ramp (pale -> deep wine).
#   * TEXT always wears ink tokens, never a series colour.
# Base-R helpers need no packages; the ggplot theme needs ggplot2 (lazy).
#
# TAXON-NAME ITALICS (house rule, applied in every figure that prints a taxon):
#   * Scientific names are ALWAYS italic; common names stay upright.
#   * A BEE label (genus "Bombus" or binomial "Bombus vosnesenskii") is entirely
#     scientific -> italicise the WHOLE label: ggplot `element_text(face="italic")`,
#     base-R `font = 3`, HTML `<i>...</i>`.
#   * A PLANT label is usually MIXED, e.g. "Wild Buckwheats (Eriogonum)" -> italicise
#     ONLY the Latin in parentheses, common name upright. Use the shared helpers in
#     plant_names.R: `plant_label_expr()` (base-R plotmath / ggplot discrete-axis labels
#     via `scale_*_discrete(labels=function(x) setNames(plant_label_expr(x),x)[x])`) and
#     `plant_label(sci_wrap="<i>%s</i>")` for HTML. Never blanket-italicise a mixed label.
# =============================================================

# ---- categorical: TRANSECT is the colour identity -------------------------
# Okabe-Ito colour-blind-safe qualitative set (green / rose / blue / orange) -- keeps each
# transect's original superbloom identity but CVD-optimised: worst adjacent dE 53.6 normal,
# 18.1 under deuteranopia (up from 13.2 for the old superbloom hexes). These 4 hues appear
# ONLY as transects. OT orange is still sub-3:1 on white, so transect figures always ship
# with a legend + labels.
BEE_TRANSECT <- c(BST = "#009E73", UPMON = "#CC79A7", TP = "#0072B2", OT = "#E69F00")
# lighter tints of each transect hue (blend toward white, same 0.38 formula as the method tints) --
# used where a transect carries TWO bars (e.g. observed vs rarefied): light = the "raw / lower" bar,
# full = the "standardized / focal" bar, so hue = transect identity and shade = the within-transect split.
BEE_TRANSECT_LT <- setNames(grDevices::rgb(t(255 - (255 - grDevices::col2rgb(BEE_TRANSECT)) * 0.38), maxColorValue = 255), names(BEE_TRANSECT))

# ---- METHOD: its own two colours (kept OFF the transect palette) -------------
# rose-red (lethal net) / periwinkle (non-lethal photo) -- softer jewel tones that sit with the purple &
# crimson ramps. Red/blue is still a CVD-robust nominal pair; periwinkle leans violet to stay clear of
# transect-TP's cyan-blue. Line style is the SECONDARY cue where a transect AND a method share a plot:
# lethal = SOLID, non-lethal = DASHED (pch: filled circle vs triangle).
BEE_METHOD_LTY   <- c(lethal = 1, nonlethal = 2)                 # solid = lethal net, dashed = non-lethal photo
BEE_METHOD_PCH   <- c(lethal = 16, nonlethal = 17)              # filled circle = net, triangle = photo
BEE_METHOD_COL   <- c(lethal = "#D8455F", nonlethal = "#6B6FCE") # rose-red = net (lethal), periwinkle = photo (non-lethal)
BEE_METHOD_LABEL <- c(lethal = "lethal", nonlethal = "non-lethal")
# "both methods / resolved-by-either" = the perceptual blend of the two method colours (red + blue -> purple).
# Computed from the tokens so it always tracks them; used by coverage_id_targets.R for the "resolved" bar.
BEE_METHOD_BOTH  <- grDevices::rgb(t(rowMeans(grDevices::col2rgb(BEE_METHOD_COL[c("lethal", "nonlethal")]))), maxColorValue = 255)
# lighter tints of each method colour (blend toward white) -- the "still unresolved" side of a method bar.
BEE_METHOD_COL_LT <- setNames(grDevices::rgb(t(255 - (255 - grDevices::col2rgb(BEE_METHOD_COL)) * 0.38), maxColorValue = 255), names(BEE_METHOD_COL))

# ---- EVIDENCE / ID-confidence: LAVENDER ordinal ramp (strong -> faint) ------
# TEAL ordinal ramp (deep teal voucher -> pale teal needs-ID). Moved OFF purple so evidence no longer
# reads like the purple neutrals; CVD-safe and kept clear of transect green/blue (they never co-occur).
BEE_EVIDENCE       <- c(specimen = "#08544B", research = "#4C9E90", needs_id = "#C3E3DC")  # deep-teal voucher -> pale-teal needs-ID, ordinal
BEE_EVIDENCE_LABEL <- c(specimen = "specimen voucher", research = "iNat research-grade",
                        needs_id = "iNat needs-ID")

# ---- NEUTRALS + ACCENT: ONE definition of the shared jewel greys + orchid ------
# Every neutral figure references THESE tokens (never a raw hex), so a tweak here updates the whole
# portfolio. Cool indigo-violet neutrals + an orchid accent -- kept clear of the teal evidence ramp and
# the crimson magnitude ramp (dE 43+ from both), so the three read as distinct families.
BEE_NEUTRAL <- c(dark = "#473C8C", light = "#C8C4EE")   # focus (indigo-violet) / background (cool lavender)
BEE_ACCENT  <- "#A857A0"                                # orchid -- the "third / other / actionable" pop

# ---- SCOPE: focus vs background --------------------------------------------
BEE_SCOPE <- c(`survey-only` = BEE_NEUTRAL[["dark"]], `all records` = BEE_NEUTRAL[["light"]])

# ---- LOCATION / SET OVERLAP: A-only / shared / B-only (on vs off-transect) ----
# dark = focal set (on-transect), light = shared core (background), orchid = the other set (off-transect).
# orchid (BEE_ACCENT) stays in the jewel/purple family and pops against the plum greys. It reads near the
# evidence-purple ramp + transect-rose, but never shares a chart with them. The method-overlap venn uses
# BEE_METHOD_COL, NOT this -- method has its own colours.
BEE_SET <- c(a_only = BEE_NEUTRAL[["dark"]], shared = BEE_NEUTRAL[["light"]], b_only = BEE_ACCENT)

# ---- ID PROGRESS: resolved / keyable / stuck (coverage_id_targets, Q7) -------
# resolved (done) = indigo focus; keyable (specimen, ACT here) = orchid accent; stuck (photo) = lavender background.
BEE_IDSTATUS <- c(resolved = BEE_NEUTRAL[["dark"]], keyable = BEE_ACCENT, stuck = BEE_NEUTRAL[["light"]])

# ---- ID PROGRESS (Q7): removed -- coverage_id_targets.R now colours by METHOD (red = specimen,
# blue = photo, purple = the red/blue blend for "resolved", i.e. a mix of both methods), derived
# straight from BEE_METHOD_COL. No separate ID-status palette needed.

# ---- HTML TABLE BADGES: the pill / chip palettes for the report's HTML tables --------------
# Single source for every coloured badge in the HTML tables (field guides, least-sampled, checklists),
# each a soft tinted background + a darker matching text colour. bee_badge_css() below turns a
# background+foreground pair into the CSS rules, so a colour lives ONCE here, never in a <style> string.
BEE_IUCN_BG     <- c(en = "#f3d2cc", cr = "#f3d2cc", vu = "#f6dcc6", nt = "#efe7cf", lc = "#dfeae0", dd = "#efefef", ne = "#efefef")
BEE_IUCN_FG     <- c(en = "#8a1c1c", cr = "#8a1c1c", vu = "#8a4a12", nt = "#6b5a20", lc = "#2f6b46", dd = "#98968f", ne = "#98968f")
# coverage pills carry the METHOD identity (has-method rule): photo-only = the bee HAS only iNaturalist
# photos (non-lethal), specimen-only = HAS only specimens (lethal), both(thin) = a few of each (blend).
# So the pills are derived straight from BEE_METHOD_COL -- pale tint for the BG, darker shade for the text.
.pill_bg <- function(col) grDevices::rgb(t(255 - (255 - grDevices::col2rgb(col)) * 0.18), maxColorValue = 255)  # ~toward white
.pill_fg <- function(col) grDevices::rgb(t(grDevices::col2rgb(col) * 0.58), maxColorValue = 255)                # ~toward black
BEE_COVERAGE_BG <- c(cb = .pill_bg(BEE_METHOD_BOTH), cp = .pill_bg(BEE_METHOD_COL[["nonlethal"]]), cs = .pill_bg(BEE_METHOD_COL[["lethal"]]))   # both(thin) / photo-only(non-lethal) / specimen-only(lethal)
BEE_COVERAGE_FG <- c(cb = .pill_fg(BEE_METHOD_BOTH), cp = .pill_fg(BEE_METHOD_COL[["nonlethal"]]), cs = .pill_fg(BEE_METHOD_COL[["lethal"]]))
BEE_ABUND_BG    <- c(rare = "#efdcd2", uncommon = "#efe9dc", common = "#dcebe0")   # abundance status pills
BEE_ABUND_FG    <- c(rare = "#8a3d1e", uncommon = "#6b5a2e", common = "#2f6b46")
BEE_DIET_BG     <- c(sp = "#f0dcc8", ge = "#cfe6e2", mo = "#e9e7e0", na = "#f1f1f1")   # specialist / generalist / moderate / n-a
BEE_DIET_FG     <- c(sp = "#7a4a1e", ge = "#0e5a52", mo = "#5a5850", na = "#999999")
BEE_FORAGE_BG   <- c(sel = "#dceee0", gen = "#e9e7e0", na = "#f1f1f1")   # forage preference pills
BEE_FORAGE_FG   <- c(sel = "#1f6b46", gen = "#5a5850", na = "#999999")

# turn a background + foreground pair into CSS rules; `sel` maps each key to its full selector.
# e.g. bee_badge_css(BEE_COVERAGE_BG, BEE_COVERAGE_FG, function(k) paste0(".pill.", k))
bee_badge_css <- function(bg, fg, sel)
  paste0(sprintf("%s{background:%s;color:%s}", vapply(names(bg), sel, ""), unname(bg), unname(fg[names(bg)])), collapse = "")

# ---- ink + chrome tokens (text never wears a series colour) -----------------
BEE_INK <- list(primary = "#0b0b0b", secondary = "#52514e", muted = "#898781",
                grid = "#e1e0d9", axis = "#c3c2b7", note = "#b2182b")   # note = scope-caption accent

# ---- MAP + TABLE chrome: non-data backgrounds / borders / stripes -----------
# Basemap and grid.table styling live here too, so no figure hardcodes a colour (single source rule).
BEE_MAP   <- c(land = "#ECEAE4", land_inset = "#F1EFEA", boundary = "#AEAAA0",
               boundary_inset = "#A7A399", frame = "#CBC7BE")   # basemap fills / boundaries / panel frame
BEE_TABLE <- c(row_odd = "#ffffff", row_even = "#f6f5f2", head = "#1a1a1a", subtext = "#666666")  # grid.table row-stripes + text

# ---- NPS FOOTPRINT theme: the CABR "punches above its weight" figures get their OWN
# National Park Service look (arrowhead sandstone + forest green). Kept deliberately OFF the
# crimson magnitude ramp so the footprint figures read as a DIFFERENT family from the rare /
# least-sampled-bee figures. Used ONLY by the two coverage/footprint scripts
# (coverage_cabr_county_map.R + coverage_cabr_share_of_county.R) via theme_nps().
BEE_NPS <- c(green = "#1E5631", green_md = "#3E8B57", brown = "#8A5A2B",
             sand = "#E7DAC0", sand_dk = "#B9A981", ink = "#2B2117")   # arrowhead palette
NPS_SEQ <- c("#CFE3D2", "#8FC0A0", "#4E9E6E", "#2C7A4B", "#12592B")     # pale sage -> deep forest (NPS magnitude ramp)

# ---- sequential (magnitude): one crimson ramp (pale -> deep wine) -----------
BEE_SEQ <- c("#E2A6AB", "#CE6F79", "#B2404E", "#86202F", "#55121D")

# ---- INTERACTION WEBS: plant vs bee node colours (bipartite visitation figures) --------
# plants = forage green, bees = goldenrod (warm vs the cool green -- the two trophic sides). node fills
# carry the colour; labels are BLACK (ink) for legibility. link = neutral grey. interactions_network.R + *_webs.R.
BEE_WEB <- c(plant = "#3E7D43", bee = "#C8952A",
             plant_label = BEE_INK$primary, bee_label = BEE_INK$primary, link = "#a29e94")

# ---- BEE FAMILY palette: colours the interaction webs' family brackets + selective nodes -------
# One colour per bee family (Paul Tol bright set), CVD-safe; selective genera/species inherit their
# family's colour, the family brackets use it, and species webs group by it. Single source for both
# interactions_network.R and interactions_genus_species_webs.R.
BEE_FAMILY <- c(Apidae = "#4477AA", Halictidae = "#228833", Megachilidae = "#AA3377",
                Andrenidae = "#CCBB44", Colletidae = "#66CCEE", Other = "#9C9A93")
BEE_FAMILY_ORDER <- c("Apidae", "Halictidae", "Megachilidae", "Andrenidae", "Colletidae", "Other")

# ---- FORAGE FAVOURITE: the red heart marking a selective taxon's availability-corrected best plant ----
BEE_FAVORITE <- "#E8000B"   # pure red, reserved for the favourite-plant heart on the interaction webs

# ---- BEE-GENUS categorical palette: colours the overview webs' SELECTIVE genera ----
# A bee genus earns a colour only if it has enough records AND forages selectively (chi-square of its
# plant distribution vs plant availability, p<0.05) -- i.e. it concentrates its visits beyond what mere
# availability/phenology would give. Non-selective or sparse genera stay grey. ~15 genera clear the bar,
# so this is a large qualitative set (as tell-apart as 15+ hues allow); the coloured top-node bars double
# as the legend, and species inherit their genus's colour. Assigned to genera in descending-record order.
BEE_GENUS_GREY <- "#B7B4AC"   # non-selective / too-few-records: nodes + links go neutral grey
# BEE_GENUS = overview genus web (selective genera); BEE_SPECIES = per-genus species webs (one hue/species).
# Single source for interactions_network.R + interactions_genus_species_webs.R (were the local
# BEE_GENUS_PALETTE / SPECIES_PAL). Assigned in descending-record order; non-selective -> BEE_GENUS_GREY.
BEE_GENUS <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7",
               "#B03A2E", "#7D3C98", "#117A65", "#8B4513", "#2C3E50", "#E7298A",
               "#66A61E", "#A6761D", "#1F78B4", "#F0A202", "#0AA6A6", "#5E35B1",
               "#7E6E00", "#8D6E63")
BEE_SPECIES <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7",
                 "#7D3C98", "#117A65", "#8B4513", "#2C3E50", "#66A61E", "#A6761D",
                 "#B03A2E", "#1F78B4", "#E7298A", "#F0A202")

# ---- PHENOLOGY SEASON: spring -> fall diverging ramp (green -> yellow -> orange-red) ----
# RdYlGn-style 6-step for month / day-of-year density in phenology_activity.R.
BEE_SEASON <- c("#1a9850", "#66bd63", "#d9ef8b", "#fee08b", "#fdae61", "#f46d43")

# ---- #12 record-confidence: flag taxa too sparse to claim a preference ------
BEE_MIN_RECORDS <- 10L    # under this many records -> "too few to claim" (matches phenology >=10)
bee_low_n       <- function(n) suppressWarnings(as.numeric(n) < BEE_MIN_RECORDS)
bee_low_n_mark  <- function(n) ifelse(bee_low_n(n), "*", "")      # asterisk (universal glyph) on sparse taxa
BEE_LOW_N_NOTE  <- sprintf("* fewer than %d records -- too few to claim a real preference", BEE_MIN_RECORDS)

# ---- figure caption: ONE consistent provenance/stats line for every figure --
# Every figure ends with a small grey bottom caption built HERE, so scope / data / rank / n /
# significance read identically across the whole portfolio. Replaces the ~12 duplicated local
# scope_cap() definitions. Pass only what applies; `sig` is dropped when no test ran.
# `asof` is read from the ingest-state file (real provenance -- auto-updates on the next data pull).
BEE_SOURCE <- "iNaturalist observations + specimen vouchers, Cabrillo NM"
bee_data_asof <- function(path = "data/observations/cache/last_ingest.txt") {
  d <- tryCatch(readLines(path, warn = FALSE)[1], error = function(e) NA_character_)
  if (is.na(d) || !nzchar(d)) "date n/a" else substr(d, 1, 10)   # ISO timestamp -> YYYY-MM-DD
}
# Significance clause that always NAMES the test, so a caption says WHICH test ran, not just the numbers.
# e.g. bee_test("PERMANOVA (Bray-Curtis)", sprintf("R2=%.2f, p=%.3f", r2, p))
#      -> "Test: PERMANOVA (Bray-Curtis) -- R2=0.73, p=0.001"  (pass as the `sig` arg of scope_cap()).
bee_test <- function(name, stats) paste0("Test: ", name, " -- ", stats)

# Build the caption string. scope_cap() keeps its old 3-arg shape so existing calls still work,
# but now also stamps source + data date (and n / sig when given). `sig` should be a bee_test(...) clause
# on any figure that ran a statistical test; leave it NULL for purely descriptive figures.
scope_cap <- function(scope = NULL, method = NULL, rank = NULL, n = NULL, sig = NULL,
                      source = BEE_SOURCE, asof = bee_data_asof(), width = 110) {
  bits <- c(
    if (!is.null(scope))  paste0("Scope: ",  scope),
    if (!is.null(method)) paste0("Method: ", method),
    if (!is.null(rank))   paste0("Rank: ",   rank),
    if (!is.null(n))      paste0("n = ",     format(n, big.mark = ",", trim = TRUE)),
    if (!is.null(sig))    sig,
    paste0("Source: ", source, " (data as of ", asof, ")"))
  stringr::str_wrap(paste(bits, collapse = "  |  "), width)
}
bee_caption <- scope_cap   # descriptive alias for new call sites

# ---- ggplot house theme ----------------------------------------------------
theme_beescabr <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = BEE_INK$primary),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle    = ggplot2::element_text(colour = BEE_INK$secondary, size = base_size - 2),
      plot.caption     = ggplot2::element_text(colour = BEE_INK$secondary, size = base_size - 2, hjust = 0, margin = ggplot2::margin(t = 8)),
      axis.title       = ggplot2::element_text(colour = BEE_INK$secondary),
      axis.text        = ggplot2::element_text(colour = BEE_INK$muted),
      panel.grid.major = ggplot2::element_line(colour = BEE_INK$grid, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      legend.title     = ggplot2::element_text(colour = BEE_INK$secondary, size = base_size - 2),
      legend.text      = ggplot2::element_text(colour = BEE_INK$secondary, size = base_size - 2),
      plot.title.position = "plot", plot.caption.position = "plot"
    )
}

# ---- NPS FOOTPRINT ggplot theme (the two coverage/footprint figures only) ---
# theme_beescabr with National Park Service accents: forest-green bold title, arrowhead-brown
# axis titles, warm sandstone gridlines, NPS ink text. Gives the CABR "footprint" figures their
# own identity, visually separate from the crimson rare / least-sampled-bee family.
theme_nps <- function(base_size = 12) {
  theme_beescabr(base_size) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = unname(BEE_NPS[["ink"]])),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size + 2, colour = unname(BEE_NPS[["green"]])),
      axis.title       = ggplot2::element_text(colour = unname(BEE_NPS[["brown"]])),
      panel.grid.major = ggplot2::element_line(colour = unname(BEE_NPS[["sand"]]), linewidth = 0.35)
    )
}

# convenience ggplot scales keyed to the canonical palettes
scale_colour_transect <- function(...) ggplot2::scale_colour_manual(values = BEE_TRANSECT, name = "transect", ...)
scale_fill_evidence   <- function(...) ggplot2::scale_fill_manual(values = BEE_EVIDENCE,
                                        labels = BEE_EVIDENCE_LABEL, name = "evidence", ...)

# ---- base-R helper: consistent par + fonts for the non-ggplot figures ------
bee_base_par <- function(...) graphics::par(family = "sans", col.axis = BEE_INK$muted,
  col.lab = BEE_INK$secondary, col.main = BEE_INK$primary, fg = BEE_INK$axis, ...)

# base-R figures draw the caption in the BOTTOM OUTER margin (ggplot uses labs(caption=)).
# Give the device enough bottom oma first (e.g. par(oma = c(4, 0, 2, 0))). Takes the same args as
# scope_cap(); splits its wrapped text into stacked mtext lines so long captions don't clip.
bee_caption_base <- function(..., cex = 0.6, line0 = 0.3) {
  lines <- strsplit(bee_caption(...), "\n", fixed = TRUE)[[1]]
  for (i in seq_along(lines))
    graphics::mtext(lines[i], side = 1, outer = TRUE, adj = 0,
                    line = line0 + (i - 1) * 0.85, cex = cex, col = BEE_INK$secondary)
}
