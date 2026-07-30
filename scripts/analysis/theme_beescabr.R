# =============================================================
# analysis/theme_beescabr.R
# beescabr -- SHARED figure house style (the ONE source of truth).
#
# Every analysis script sources this so colours, fonts, and spacing are identical
# across all figures. Change a value HERE and every figure updates -- a transect is
# the same colour everywhere, forever.
#
# DESIGN (colour-blind validated -- transect palette worst adjacent CVD dE 15.5):
#   * COLOUR encodes ONE thing: TRANSECT (the primary spatial identity).
#   * METHOD (lethal net / non-lethal photo) has its OWN identity: line style AND colour.
#     Lines: lethal = SOLID, non-lethal = DASHED. Fills (venn, ID-completeness): two method
#     colours (purple = net, vermillion = photo), deliberately OUTSIDE the transect palette
#     so method never reads as a transect. COLOUR still means TRANSECT in the transect
#     figures; the method colours appear only where method itself is the subject.
#   * EVIDENCE / ID-confidence is an ORDINAL ramp (voucher dark -> needs-ID faint), so
#     "less certain" literally looks fainter.
#   * SCOPE (survey-only vs all-records) = accent vs grey (grey = background context).
#   * MAGNITUDE (richness / counts) = one blue sequential ramp.
#   * TEXT always wears ink tokens, never a series colour.
# Base-R helpers need no packages; the ggplot theme needs ggplot2 (lazy).
# =============================================================

# ---- categorical: TRANSECT is the colour identity -------------------------
# calecopal 'superbloom3' vibe -- grass / lupine-magenta / sky / poppy. CVD-validated
# (worst adjacent dE 25.4, all bands pass; orange is sub-3:1 on white so transects
# always ship with a legend + labels). Swap to exact cal_palette("superbloom3") hexes if wanted.
BEE_TRANSECT <- c(BST = "#3E7D43", UPMON = "#CB1F6A", TP = "#3B5EA0", OT = "#E69F00")

# ---- METHOD: its own line style + colour identity (kept OFF the transect palette) ----
# lethal = SOLID line, non-lethal = DASHED. Method-subject figures (venn, ID-completeness)
# use the two colours below -- purple (net) / vermillion (photo), CVD dE 96, no transect hue.
BEE_METHOD_LTY   <- c(lethal = 1, nonlethal = 2)                 # solid = lethal net, dashed = non-lethal photo
BEE_METHOD_PCH   <- c(lethal = 16, nonlethal = 17)              # filled circle = net, triangle = photo
BEE_METHOD_COL   <- c(lethal = "#762a83", nonlethal = "#D55E00") # purple = net, vermillion = photo
BEE_METHOD_LABEL <- c(lethal = "lethal (specimen net)", nonlethal = "non-lethal (iNat photo)")

# ---- EVIDENCE / ID-confidence: ordinal ramp (strong -> faint) --------------
BEE_EVIDENCE       <- c(specimen = "#0c5544", research = "#1e8b71", needs_id = "#46b39a")  # vivid teal ordinal ramp (dark voucher -> medium needs-ID), --ordinal validated
BEE_EVIDENCE_LABEL <- c(specimen = "specimen voucher", research = "iNat research-grade",
                        needs_id = "iNat needs-ID")

# ---- SCOPE: accent vs grey -------------------------------------------------
BEE_SCOPE <- c(`survey-only` = "#2166ac", `all records` = "#b8b8b8")

# ---- LOCATION / SET OVERLAP: A-only / shared / B-only (on vs off-transect) ----
# on-transect = focal blue, shared core = grey (background), off-transect = vermillion.
# (off-transect records are all non-lethal iNat, so vermillion reads consistently with method here.)
# The method-overlap venn uses BEE_METHOD_COL, NOT this -- method has its own colours now.
BEE_SET <- c(a_only = "#2166ac", shared = "#b8b8b8", b_only = "#D55E00")

# ---- ID PROGRESS: resolved / keyable / stuck (coverage_id_targets, Q7) -------
# Blue = resolved to species (done), purple = specimen-keyable (ACT here -- and purple is the
# net/lethal method colour, which is exactly what a keyable specimen IS), grey = photo/genus-only.
# 2-cat panels reuse resolved(blue) vs stuck(grey).
BEE_IDSTATUS <- c(resolved = "#2166ac", keyable = "#762a83", stuck = "#b8b8b8")

# ---- ink + chrome tokens (text never wears a series colour) -----------------
BEE_INK <- list(primary = "#0b0b0b", secondary = "#52514e", muted = "#898781",
                grid = "#e1e0d9", axis = "#c3c2b7", note = "#b2182b")   # note = scope-caption accent

# ---- sequential (magnitude): one blue ramp ---------------------------------
BEE_SEQ <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")

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
      plot.subtitle    = ggplot2::element_text(colour = BEE_INK$note, size = base_size - 2),
      plot.caption     = ggplot2::element_text(colour = BEE_INK$muted, size = base_size - 3, hjust = 0),
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
