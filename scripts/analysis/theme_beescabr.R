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
#   * EVIDENCE / ID-confidence -- a LAVENDER->plum ordinal ramp (voucher dark -> needs-ID faint), so
#     "less certain" literally looks fainter. Off every transect hue.
#   * SCOPE / SET / ID-status -- single-figure accents from one small set: dark = focus/primary,
#     light = background/shared, orchid = the third/other category. Plum-tinted jewel neutrals.
#   * MAGNITUDE (richness / counts) -- one crimson sequential ramp (pale -> deep wine).
#   * TEXT always wears ink tokens, never a series colour.
# Base-R helpers need no packages; the ggplot theme needs ggplot2 (lazy).
# =============================================================

# ---- categorical: TRANSECT is the colour identity -------------------------
# Okabe-Ito colour-blind-safe qualitative set (green / rose / blue / orange) -- keeps each
# transect's original superbloom identity but CVD-optimised: worst adjacent dE 53.6 normal,
# 18.1 under deuteranopia (up from 13.2 for the old superbloom hexes). These 4 hues appear
# ONLY as transects. OT orange is still sub-3:1 on white, so transect figures always ship
# with a legend + labels.
BEE_TRANSECT <- c(BST = "#009E73", UPMON = "#CC79A7", TP = "#0072B2", OT = "#E69F00")

# ---- METHOD: its own two colours (kept OFF the transect palette) -------------
# rose-red (lethal net) / periwinkle (non-lethal photo) -- softer jewel tones that sit with the purple &
# crimson ramps. Red/blue is still a CVD-robust nominal pair; periwinkle leans violet to stay clear of
# transect-TP's cyan-blue. Line style is the SECONDARY cue where a transect AND a method share a plot:
# lethal = SOLID, non-lethal = DASHED (pch: filled circle vs triangle).
BEE_METHOD_LTY   <- c(lethal = 1, nonlethal = 2)                 # solid = lethal net, dashed = non-lethal photo
BEE_METHOD_PCH   <- c(lethal = 16, nonlethal = 17)              # filled circle = net, triangle = photo
BEE_METHOD_COL   <- c(lethal = "#D8455F", nonlethal = "#6B6FCE") # rose-red = net (lethal), periwinkle = photo (non-lethal)
BEE_METHOD_LABEL <- c(lethal = "lethal (specimen net)", nonlethal = "non-lethal (iNat photo)")

# ---- EVIDENCE / ID-confidence: LAVENDER ordinal ramp (strong -> faint) ------
# Lavender -> deep plum (superbloom1 #9185AE + superbloom2 #61487E family), off every transect hue.
BEE_EVIDENCE       <- c(specimen = "#52357E", research = "#9078C0", needs_id = "#D0C6E8")  # deep-plum voucher -> pale-lavender needs-ID, --ordinal validated
BEE_EVIDENCE_LABEL <- c(specimen = "specimen voucher", research = "iNat research-grade",
                        needs_id = "iNat needs-ID")

# ---- NEUTRALS + ACCENT: ONE definition of the shared jewel greys + orchid ------
# Every neutral figure references THESE tokens (never a raw hex), so a tweak here updates the whole
# portfolio. Plum-tinted aubergine/lavender neutrals + an orchid accent -- warm jewel tones that sit
# with the purple (evidence) and crimson (magnitude) ramps.
BEE_NEUTRAL <- c(dark = "#3D3646", light = "#D1CBD8")   # focus (aubergine) / background (lavender-grey)
BEE_ACCENT  <- "#A857A0"                                # orchid -- the "third / other / actionable" pop (jewel family)

# ---- SCOPE: focus vs background --------------------------------------------
BEE_SCOPE <- c(`survey-only` = BEE_NEUTRAL[["dark"]], `all records` = BEE_NEUTRAL[["light"]])

# ---- LOCATION / SET OVERLAP: A-only / shared / B-only (on vs off-transect) ----
# dark = focal set (on-transect), light = shared core (background), orchid = the other set (off-transect).
# orchid (BEE_ACCENT) stays in the jewel/purple family and pops against the plum greys. It reads near the
# evidence-purple ramp + transect-rose, but never shares a chart with them. The method-overlap venn uses
# BEE_METHOD_COL, NOT this -- method has its own colours.
BEE_SET <- c(a_only = BEE_NEUTRAL[["dark"]], shared = BEE_NEUTRAL[["light"]], b_only = BEE_ACCENT)

# ---- ID PROGRESS: resolved / keyable / stuck (coverage_id_targets, Q7) -------
# dark = resolved to species (done), orchid = specimen-keyable (ACT here), light = photo/genus-only (stuck).
# 2-cat panels reuse resolved(dark) vs stuck(light). orchid shares BEE_SET's accent (same jewel palette).
BEE_IDSTATUS <- c(resolved = BEE_NEUTRAL[["dark"]], keyable = BEE_ACCENT, stuck = BEE_NEUTRAL[["light"]])

# ---- ink + chrome tokens (text never wears a series colour) -----------------
BEE_INK <- list(primary = "#0b0b0b", secondary = "#52514e", muted = "#898781",
                grid = "#e1e0d9", axis = "#c3c2b7", note = "#b2182b")   # note = scope-caption accent

# ---- sequential (magnitude): one crimson ramp (pale -> deep wine) -----------
BEE_SEQ <- c("#E2A6AB", "#CE6F79", "#B2404E", "#86202F", "#55121D")

# ---- INTERACTION WEBS: plant vs bee node colours (bipartite visitation figures) --------
# plants = forage green, bees = magenta (opposite of green -- the two trophic sides). label = the darker
# shade of each, link = neutral grey (nodes carry the colour). interactions_network.R + *_webs.R.
BEE_WEB <- c(plant = "#3E7D43", bee = "#A63D95",
             plant_label = "#2C5A31", bee_label = "#7A2A6D", link = "#a29e94")

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

# convenience ggplot scales keyed to the canonical palettes
scale_colour_transect <- function(...) ggplot2::scale_colour_manual(values = BEE_TRANSECT, name = "transect", ...)
scale_fill_evidence   <- function(...) ggplot2::scale_fill_manual(values = BEE_EVIDENCE,
                                        labels = BEE_EVIDENCE_LABEL, name = "evidence", ...)

# ---- base-R helper: consistent par + fonts for the non-ggplot figures ------
bee_base_par <- function(...) graphics::par(family = "sans", col.axis = BEE_INK$muted,
  col.lab = BEE_INK$secondary, col.main = BEE_INK$primary, fg = BEE_INK$axis, ...)
