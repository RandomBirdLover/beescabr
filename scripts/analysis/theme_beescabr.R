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

# ---- NON-URGENT TEAL: ONE canonical ramp -> evidence + neutrals + magnitude all derive from it --------
# The whole NON-URGENT family is a SINGLE teal ramp (pale -> deep). Evidence, the neutral focus/background
# two-tone, and the magnitude ramp (BEE_SEQ, below) are all just stops of BEE_TEAL -- so "non-urgent" is
# ONE colour idea, not three. They never share a figure, so the single ramp is unambiguous. CVD-safe, kept
# clear of transect green/blue. The ONLY other family is RED (BEE_RARE / BEE_ACCENT) = rare / urgent.
BEE_TEAL <- c("#D6ECE6", "#A2D4CA", "#63B3A3", "#2E9584", "#0D6E60", "#08463D")   # pale -> deep teal (non-urgent)

# EVIDENCE / ID-confidence: 3 ordinal stops of BEE_TEAL (deep voucher -> pale needs-ID)
BEE_EVIDENCE       <- c(specimen = BEE_TEAL[[6]], research = BEE_TEAL[[4]], needs_id = BEE_TEAL[[1]])
BEE_EVIDENCE_LABEL <- c(specimen = "specimen voucher", research = "iNat research-grade",
                        needs_id = "iNat needs-ID")

# ---- NEUTRALS + ACCENT: focus/background = two stops of BEE_TEAL; accent = the urgent-red pop ------
# TWO-FAMILY house rule: TEAL = everything non-urgent (evidence, neutrals, magnitude -- ALL derived from
# BEE_TEAL above); RED = rare/urgent. Neutrals are the deep + pale ends of BEE_TEAL; the "act here" accent
# is RED (same family as BEE_RARE). Every neutral figure references THESE tokens (never a raw hex), so a
# tweak to BEE_TEAL updates the whole non-urgent portfolio at once.
BEE_NEUTRAL <- c(dark = BEE_TEAL[[5]], light = BEE_TEAL[[2]])   # focus (deep teal) / background (pale teal)
BEE_ACCENT  <- "#B2404E"                                        # crimson -- the "rare / urgent / act here" pop (red family)

# ---- SCOPE: focus vs background --------------------------------------------
BEE_SCOPE <- c(`survey-only` = BEE_NEUTRAL[["dark"]], `all records` = BEE_NEUTRAL[["light"]])

# ---- LOCATION / SET OVERLAP: A-only / shared / B-only (on vs off-transect) ----
# Three stepped teals, ALL non-urgent: both = DEEP (the shared, best-documented core -- the anchor), on-only
# = pale, off-only = mid. RED is reserved for the genuinely rare / at-risk figures (BEE_RARE) only -- an
# off-transect coverage gap is actionable but not a conservation alarm, so it stays in the teal family.
# Method-overlap venn uses BEE_METHOD_COL, not this.
BEE_SET <- c(a_only = BEE_TEAL[[2]], shared = BEE_TEAL[[6]], b_only = BEE_TEAL[[4]])

# ---- ID PROGRESS: resolved / keyable / stuck (coverage_id_targets, Q7) -------
# resolved (done) = teal focus; keyable (specimen, ACT here) = red accent; stuck (photo) = pale teal background.
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

# ---- HTML interactive tables: single source for the base table chrome -------
# Every report HTML table (field guides, least-sampled, summary tables) shares ONE stylesheet built
# from these tokens -- so page/header/border/stripe/hover colours live here ONCE, never copy-pasted as
# hardcoded hex into each script's <style>. Badge/pill palettes are the BEE_*_BG/FG tokens above
# (rendered by bee_badge_css); this covers the surrounding table chrome + layout.
BEE_HTML <- c(page = "#ffffff", page_alt = "#ece9e2", ink = "#22211e", sub = "#6b6a66", scope = "#4a4945",
              scope_rule = "#d8d5cc", head_bg = "#f2efe8", head_hover = "#e8e4da",
              row_hover = "#e6f1ec",                      # pale teal -> ties tables to the house palette; reads over the zebra stripe
              border = "#dddddd", border_lt = "#ededea", cn = "#8a8880", low = "#a09e98", cs = "#8a1c1c")
# The shared stylesheet (the rules that go BETWEEN <style> and </style>). A script appends its own
# bee_badge_css(...) for whichever pill sets it uses. Zebra striping (BEE_TABLE row_even) + sticky,
# sortable, teal-hover header give every table the same look + organization.
bee_table_css <- function() paste0(
  "*{box-sizing:border-box}",
  # FULL-WIDTH layout (no fixed-width card) -- these field-guide tables are wide (10-13 cols), so a fixed
  # card either clips columns (too narrow) or overflows a laptop screen (too wide). Full width fits any
  # screen and the columns wrap to fit; the teal typography polish stays. (Narrow tables like the NPS
  # summary keep their centred card in their own CSS.)
  "body{font:15px/1.55 -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;color:", BEE_HTML[["ink"]], ";margin:22px 28px 44px;background:", BEE_HTML[["page"]], ";-webkit-font-smoothing:antialiased}",
  ".org{font-size:12.5px;font-weight:700;text-transform:uppercase;letter-spacing:.09em;color:", BEE_TEAL[[4]], ";margin:0 0 3px}",   # park-name eyebrow above the title
  "h1{font-size:25px;font-weight:700;letter-spacing:-.01em;margin:0;white-space:nowrap;color:", BEE_TEAL[[6]], "}",
  "h1:after{content:'';display:block;width:56px;height:3px;background:", BEE_TEAL[[4]], ";border-radius:2px;margin:11px 0 2px}",
  ".byline{font-size:13px;color:", BEE_HTML[["sub"]], ";margin:7px 0 0;font-style:italic}",   # author credit under the title bar
  "h2{font-size:15px;font-weight:700;margin:30px 0 6px;color:", BEE_TEAL[[6]], "}",
  "p.sub{color:", BEE_HTML[["sub"]], ";margin:13px 0 4px;font-size:13.5px;max-width:980px}",
  "p.scope{color:", BEE_HTML[["scope"]], ";margin:14px 0 6px;font-size:12px;font-weight:600;background:", BEE_HTML[["head_bg"]], ";border-left:3px solid ", BEE_TEAL[[4]], ";padding:9px 13px;border-radius:0 7px 7px 0;max-width:1100px}",
  "p.note{color:", BEE_HTML[["sub"]], ";margin:14px 0 0;font-size:12px}",
  "sup.cs{color:", BEE_HTML[["cs"]], ";font-weight:700;margin-left:1px}",
  "table{border-collapse:separate;border-spacing:0;width:100%;font-size:13px;margin-top:14px}",
  "th,td{text-align:left;padding:9px 12px;border-bottom:1px solid ", BEE_HTML[["border_lt"]], ";vertical-align:top}",
  "thead th{position:sticky;top:0;background:", BEE_HTML[["head_bg"]], ";cursor:pointer;font-weight:700;white-space:nowrap;text-transform:uppercase;letter-spacing:.04em;font-size:11px;color:", BEE_TEAL[[6]], ";border-bottom:2px solid ", BEE_TEAL[[3]], "}",
  "thead th:hover{background:", BEE_HTML[["head_hover"]], "}",
  "tbody tr:nth-child(even){background:", BEE_TABLE[["row_even"]], "}",
  "tbody tr:hover{background:", BEE_HTML[["row_hover"]], "}",
  "tbody tr:last-child td{border-bottom:none}",
  "th.num{text-align:right}td.num{text-align:right;font-variant-numeric:tabular-nums}",
  "td.bee i{color:#111}td .cn{display:block;color:", BEE_HTML[["cn"]], ";font-size:11px}",
  "td.loc{color:", BEE_HTML[["scope"]], ";font-size:12px}",
  "tr.low{color:", BEE_HTML[["low"]], "}tr.low td.bee i{color:", BEE_HTML[["low"]], "}",
  ".pill{display:inline-block;padding:3px 10px;border-radius:11px;font-size:11px;font-weight:600;white-space:nowrap}",
  ".iucn{display:inline-block;padding:2px 8px;border-radius:5px;font-size:11px;font-weight:700}")

# ---- NPS FOOTPRINT theme: the CABR "punches above its weight" figures get their OWN
# National Park Service look (arrowhead sandstone + forest green). Its GREEN keeps the footprint
# figures a DIFFERENT family from both the teal magnitude ramp and the red rare/urgent ramp. Used ONLY
# by the two coverage/footprint scripts (coverage_cabr_county_map.R + coverage_cabr_share_of_county.R).
BEE_NPS <- c(green = "#1E5631", green_md = "#3E8B57", brown = "#8A5A2B",
             sand = "#E7DAC0", sand_dk = "#B9A981", ink = "#2B2117")   # arrowhead palette
NPS_SEQ <- c("#CFE3D2", "#8FC0A0", "#4E9E6E", "#2C7A4B", "#12592B")     # pale sage -> deep forest (NPS magnitude ramp)

# ---- sequential ramps (magnitude): two families -----------------------------
# BEE_SEQ = NON-URGENT magnitude = the canonical teal ramp BEE_TEAL itself (visits, richness, trips,
#   top-plants, rarefaction) -- same colour idea as evidence + neutrals, so "more" never reads as "alarming".
# BEE_RARE = the RARE / URGENT ramp -> RED (pale rose -> deep wine). The ONLY red ramp: reserved for figures
#   where the magnitude IS the warning (rare bees' host plants). RED = "look here"; teal = ordinary quantity.
BEE_SEQ  <- BEE_TEAL                                                   # non-urgent magnitude = the canonical teal ramp
BEE_RARE <- c("#E2A6AB", "#CE6F79", "#B2404E", "#86202F", "#55121D")   # rare/urgent magnitude: pale rose -> deep wine

# ---- INTERACTION WEBS: plant vs bee node colours (bipartite visitation figures) --------
# plants = forage green, bees = goldenrod (warm vs the cool green -- the two trophic sides). node fills
# carry the colour; labels are BLACK (ink) for legibility. link = neutral grey. interactions_network.R + *_webs.R.
BEE_WEB <- c(plant = "#3E7D43", bee = "#C8952A",
             plant_label = BEE_INK$primary, bee_label = BEE_INK$primary, link = "#a29e94")
# High-visibility gold for SELECTION markers (availability-corrected favourite): the gold ring on the
# flower petals + the outlined bar / marker on the rare-bee plant figures. Brighter & more saturated than
# the muted web goldenrod so it pops against both the pale-pink and deep-red bars.
BEE_SELECT <- "#FFB300"

# ---- BEE FAMILY palette: colours the interaction webs' family brackets + selective nodes -------
# One colour per bee family (Paul Tol bright set), CVD-safe; selective genera/species inherit their
# family's colour, the family brackets use it, and species webs group by it. Single source for both
# interactions_network.R and interactions_genus_species_webs.R.
# hues spread wide around the wheel (yellow / green / blue / pink / purple / grey) so families read
# as clearly distinct -- this also anchors the genus colouring: a genus takes a shade of its family hue.
BEE_FAMILY <- c(Apidae = "#E5B80B", Halictidae = "#2CA05A", Megachilidae = "#3F6FB5",
                Andrenidae = "#C43C8E", Colletidae = "#8256B4", Other = "#9C9A93")
BEE_FAMILY_ORDER <- c("Apidae", "Halictidae", "Megachilidae", "Andrenidae", "Colletidae", "Other")

# ---- FORAGE FAVOURITE: the red heart marking a selective taxon's availability-corrected best plant ----
BEE_FAVORITE <- "#E8000B"   # pure red, reserved for the favourite-plant heart on the interaction webs

# ---- BEE-GENUS categorical palette: colours the overview webs' SELECTIVE genera ----
# A bee genus earns a colour only if it has enough records AND forages selectively (chi-square of its
# plant distribution vs plant availability, p<0.05) -- i.e. it concentrates its visits beyond what mere
# availability/phenology would give. Non-selective or sparse genera stay grey. ~15 genera clear the bar,
# so this is a large qualitative set (as tell-apart as 15+ hues allow); the coloured top-node bars double
# as the legend. Assigned to genera in descending-record order. NOTE: in the per-genus species webs the
# species do NOT inherit a tint of their genus's colour -- BEE_SPECIES gives each species its own distinct
# hue (assigned by position within that one web) so the species can be told apart, which is that view's job.
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

# ---- PHENOLOGY SEASON: winter BLUE held across Nov-Feb (near-empty bee months), greening from the END
# OF FEBRUARY through spring (more green), soft-orange SUMMER peak, then a WARM tan/gold fall that fades
# back to blue by November. Colours are POSITIONED by day-of-year in phenology_activity.R (via `values`);
# the only "plateau" is the winter blue, which sits in low-activity months so it never reads as a sharp
# edge (unlike the summer orange plateau the user rejected). Fall stays warm (tan), no green on the way down.
BEE_SEASON <- c("#2C7BB6", "#2C7BB6", "#7DC35E", "#EEDB6E", "#F4972A", "#EBD98C", "#D8C288", "#2C7BB6", "#2C7BB6")

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
#      -> "Analysis: PERMANOVA (Bray-Curtis) -- R2=0.73, p=0.001"  (pass as the `sig` arg of scope_cap()).
bee_test <- function(name, stats) paste0("Analysis: ", name, " -- ", stats)

# Build the caption string. scope_cap() keeps its old 3-arg shape so existing calls still work,
# but now also stamps source + data date (and n / sig when given). `sig` should be a bee_test(...) clause
# on any figure that ran a statistical test; leave it NULL for purely descriptive figures.
scope_cap <- function(scope = NULL, method = NULL, rank = NULL, n = NULL, sig = NULL,
                      source = BEE_SOURCE, asof = bee_data_asof(), width = 110, control = NULL) {
  # `control` (named-only, placed last so positional calls are unaffected) = what a statistical figure
  # holds constant / its null model (e.g. "flight season + method (matched-cell null)"). Renders right
  # after Rank. Leave NULL on descriptive figures (no test -> no control).
  bits <- c(
    if (!is.null(scope))   paste0("Scope: ",   scope),
    if (!is.null(method))  paste0("Method: ",  method),
    if (!is.null(rank))    paste0("Rank: ",    rank),
    if (!is.null(control)) paste0("Control: ", control),
    if (!is.null(n))       paste0("n = ",      format(n, big.mark = ",", trim = TRUE)),
    if (!is.null(sig))     sig,
    paste0("Source: ", source, " (data as of ", asof, ")"))
  stringr::str_wrap(paste(bits, collapse = "  |  "), width)
}
bee_caption <- scope_cap   # descriptive alias for new call sites

# ---- crisp PNG output -------------------------------------------------------
# The base quartz/png device renders small labels with rough ("shaky") anti-aliasing;
# ragg's agg_png draws smooth text. Route ALL figure saves through these two helpers so
# every figure gets the same crisp output from one place.
#   - bee_ggsave(): ggplot figures. Inches + dpi, so we can safely lift dpi to 300 (more
#     pixels, same layout) for sharp small text. Falls back to the default device if ragg
#     is missing. Do NOT pass `device=`/`dpi=` at call sites -- the helper owns them.
#   - bee_png(): base-graphics figures. Their canvases are sized in PIXELS, so we keep the
#     caller's width/height/res (changing res would rescale text against a fixed canvas) and
#     only swap in ragg's smoother anti-aliasing.
# HD output: more pixels so figures stay sharp when enlarged/stretched on a screen or in slides.
BEE_HD <- 2   # base-graphics upscale: multiplies pixel width/height AND res together -> same layout, 2x resolution
bee_ggsave <- function(filename, plot = ggplot2::last_plot(), ..., dpi = 400) {   # 400 dpi (was 300) -- inches-based, so just more pixels, same layout
  dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL
  ggplot2::ggsave(filename, plot = plot, dpi = dpi, device = dev, ...)
}
bee_png <- function(filename, ...) {
  a <- list(...)
  if (!is.null(a$width))  a$width  <- a$width  * BEE_HD     # scale canvas + res by the SAME factor => pure
  if (!is.null(a$height)) a$height <- a$height * BEE_HD     # resolution bump, text/line proportions unchanged
  a$res <- (if (is.null(a$res)) 72 else a$res) * BEE_HD
  dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else grDevices::png
  do.call(dev, c(list(filename = filename), a))
}

# ---- ggplot house theme ----------------------------------------------------
theme_beescabr <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = BEE_INK$primary),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle    = ggplot2::element_text(colour = BEE_INK$secondary, size = base_size - 2, hjust = 0.5),   # takeaway line sits centred under the (centred) title
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
