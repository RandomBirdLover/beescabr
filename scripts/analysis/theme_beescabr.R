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
#     bright red (lethal) / bright royal-blue (non-lethal), CVD dE ~141. Line style is a SECONDARY cue for the one
#     figure where a transect AND a method share a plot (lethal = SOLID, non-lethal = DASHED).
#   * EVIDENCE / ID-confidence -- a LAVENDER->plum ordinal ramp (voucher dark -> needs-ID faint), so
#     "less certain" literally looks fainter. Lavender from superbloom1/2, off every transect hue.
#   * SCOPE / SET / ID-status -- single-figure accents from one small set: ink = focus/primary,
#     mist = background/shared, teal = the third/other category. Cool neutrals (no warm/brown cast).
#   * MAGNITUDE (richness / counts) -- one crimson sequential ramp (pale -> deep wine).
#   * TEXT always wears ink tokens, never a series colour. Nothing uses purple.
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
# bright red (lethal net) / bright royal-blue (non-lethal photo) -- internal CVD dE ~141 (red/blue is
# the most CVD-robust pair there is). The blue leans indigo to stay clear of transect-TP's teal-ish blue
# (dE 56 normal / 38 deuteranopia) so the two blues don't read alike across charts.
# Line style is a SECONDARY cue for the one figure where a transect AND a method share a plot:
# lethal = SOLID, non-lethal = DASHED (pch: filled circle vs triangle).
BEE_METHOD_LTY   <- c(lethal = 1, nonlethal = 2)                 # solid = lethal net, dashed = non-lethal photo
BEE_METHOD_PCH   <- c(lethal = 16, nonlethal = 17)              # filled circle = net, triangle = photo
BEE_METHOD_COL   <- c(lethal = "#E8000D", nonlethal = "#2E4FE0") # bright red = net (lethal), bright royal blue = photo (non-lethal)
BEE_METHOD_LABEL <- c(lethal = "lethal (specimen net)", nonlethal = "non-lethal (iNat photo)")

# ---- EVIDENCE / ID-confidence: LAVENDER ordinal ramp (strong -> faint) ------
# Lavender -> deep plum (superbloom1 #9185AE + superbloom2 #61487E family), off every transect hue.
BEE_EVIDENCE       <- c(specimen = "#52357E", research = "#9078C0", needs_id = "#D0C6E8")  # deep-plum voucher -> pale-lavender needs-ID, --ordinal validated
BEE_EVIDENCE_LABEL <- c(specimen = "specimen voucher", research = "iNat research-grade",
                        needs_id = "iNat needs-ID")

# ---- NEUTRALS + ACCENT: ONE definition of the shared cool greys + teal ------
# Every neutral figure references THESE tokens (never a raw hex), so a tweak here updates the whole
# portfolio. Cool charcoal/mist replaced the old warm greige #3C3B36/#C0BBB0, which read muddy brown.
BEE_NEUTRAL <- c(dark = "#343A3F", light = "#C6CCCF")   # focus (charcoal) / background (mist-grey)
BEE_ACCENT  <- "#0E8F82"                                # clean teal -- the "third / other / actionable" pop

# ---- SCOPE: focus vs background --------------------------------------------
BEE_SCOPE <- c(`survey-only` = BEE_NEUTRAL[["dark"]], `all records` = BEE_NEUTRAL[["light"]])

# ---- LOCATION / SET OVERLAP: A-only / shared / B-only (on vs off-transect) ----
# dark = focal set (on-transect), light = shared core (background), teal = the other set (off-transect).
# teal (BEE_ACCENT) pops against the cool greys, CVD-clear of every series colour (closest transect-rose
# dE 11.6); transect/method no longer use teal, so this lane is free. The method-overlap venn uses
# BEE_METHOD_COL, NOT this -- method has its own colours.
BEE_SET <- c(a_only = BEE_NEUTRAL[["dark"]], shared = BEE_NEUTRAL[["light"]], b_only = BEE_ACCENT)

# ---- ID PROGRESS: resolved / keyable / stuck (coverage_id_targets, Q7) -------
# dark = resolved to species (done), teal = specimen-keyable (ACT here), light = photo/genus-only (stuck).
# 2-cat panels reuse resolved(dark) vs stuck(light). teal shares BEE_SET's accent (same cool palette).
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
