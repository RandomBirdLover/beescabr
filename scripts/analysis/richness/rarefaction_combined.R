# =============================================================
# analysis/rarefaction_combined.R
# beescabr -- ONE figure and ONE table per rarefaction comparison, showing both
# estimators together instead of eight separate files.
#
# rarefaction_vegan.R and rarefaction_inext.R each wrote their own PNGs and CSVs
# for the same comparison, which produced 32 files in the fair-window folder for
# what is really 4 questions (method / observer, at genus / species rank). Nobody
# opens 32 files. This reads what both scripts already wrote and draws them on one
# pair of axes, so the cross-check is visible instead of being two folders apart.
#
# iNEXT leads: coverage-based standardisation is the right tool for comparing two
# sampling methods, because equal sample SIZE is not equal sample COMPLETENESS.
# vegan's individual-based rarefaction rides along as the cross-check.
#
# Runs AFTER both rarefaction scripts. Reads only their CSV outputs, so it recomputes
# nothing and cannot disagree with them.
# =============================================================
if (!exists("beescabr_require")) source("scripts/config.R")
beescabr_require()
suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/theme_beescabr.R")
if (!exists("rare_out_name")) source("scripts/analysis/rarefaction_names.R")

# each comparison reads and writes inside the folder its window declares
rc_dir <- function(dim) file.path(DIR_REPORT, "richness/rarefaction", rare_window_dir(dim))
rc_rd  <- function(dim, f) { p <- file.path(rc_dir(dim), f)
                        if (file.exists(p)) read.csv(p, stringsAsFactors = FALSE) else NULL }

# ONE figure per comparison, both ranks on it. Rank was a separate PNG until the
# folder held four near-identical figures for two questions; it is now a facet
# column, so genus and species are read side by side instead of by flipping files.
RC_RANKS <- c("species", "genus")
RC_QL    <- c("0" = "q = 0  richness", "1" = "q = 1  Shannon", "2" = "q = 2  Simpson")

# the curve data for one (comparison, rank), long, with the two standardisations
# stacked as a `panel` column
rc_curves <- function(dim, rank) {
  size <- rc_rd(dim, sprintf("rarefaction_by_%s_inext_curve_size_%s.csv",     dim, rank))
  cov  <- rc_rd(dim, sprintf("rarefaction_by_%s_inext_curve_coverage_%s.csv", dim, rank))
  if (is.null(size) || is.null(cov)) return(NULL)
  QL <- RC_QL
  bind_rows(
    size %>% transmute(group = Assemblage, x = m,  y = qD, lo = qD.LCL, hi = qD.UCL,
                       kind = Method, panel = "Standardised by sample size",
                       q = QL[as.character(Order.q)]),
    cov  %>% transmute(group = Assemblage, x = SC, y = qD, lo = qD.LCL, hi = qD.UCL,
                       kind = Method, panel = "Standardised by coverage",
                       q = QL[as.character(Order.q)])) %>%
    mutate(rank = rank)
}

rc_one <- function(dim) {
  w   <- rare_window(dim)
  dat <- bind_rows(lapply(RC_RANKS, function(r) rc_curves(dim, r)))
  if (!nrow(dat)) return(invisible(NULL))
  veg <- rc_rd(dim, rare_out_name(dim, kind = "rarefied"))

  # The figure draws the SAMPLE-SIZE curves only. The coverage-standardised curves
  # are unreadable as a picture -- every group is compressed against SC = 1, where the
  # interesting differences are a few pixels wide -- and the number a reader actually
  # wants from them is exact, not visual. It lives in the estimates table beside this
  # figure, as basis = "equal_coverage". Keeping only the sample-size panel also puts
  # vegan's cross-check point on the same axis as the curve it checks.
  dat <- dat[dat$panel == "Standardised by sample size", , drop = FALSE]
  dat$rank <- factor(dat$rank, levels = RC_RANKS, labels = c("Species", "Genus"))
  # ONE strip per panel so every panel gets its own y scale. facet_grid frees y only
  # per row, and q0 (richness, ~40) sits an order of magnitude above q2 (~5), so a
  # shared row scale flattens q1 and q2 into the axis.
  rc_lab <- function(rank, q) factor(paste0(rank, "   ", q),
              levels = as.vector(t(outer(c("Species", "Genus"), RC_QL, paste, sep = "   "))))
  dat$facet <- rc_lab(dat$rank, dat$q)

  # vegan's rarefied point rides on the sample-size panel as the cross-check
  pts <- if (!is.null(veg)) veg %>%
           mutate(rank = factor(rank, levels = RC_RANKS, labels = c("Species", "Genus"))) %>%
           transmute(group, rank, x = rarefied_to, y = rarefied_richness,
                     facet = rc_lab(rank, RC_QL[[1]])) else NULL

  # both windows group on a column whose levels are already colour-token keys
  cols <- if (identical(w$group, "surveyor")) BEE_OBSERVER_COL else BEE_METHOD_COL
  labs_for <- if (identical(w$group, "surveyor")) BEE_OBSERVER_LABEL else BEE_METHOD_LABEL

  g <- ggplot(dat, aes(x, y, colour = group, fill = group)) +
    geom_ribbon(aes(ymin = lo, ymax = hi, group = interaction(group, kind)),
                alpha = 0.15, colour = NA) +
    geom_line(aes(linetype = kind, group = interaction(group, kind)), linewidth = 0.7) +
    facet_wrap(~ facet, nrow = 2, scales = "free") +
    # iNEXT's convention, which reviewers expect: the rarefied part of the curve is
    # SOLID (it is measured) and the extrapolated part is dashed (it is estimated).
    scale_linetype_manual(values = c(Rarefaction = "solid", Observed = "solid",
                                     Extrapolation = "22"),
                          breaks = c("Rarefaction", "Extrapolation"), name = NULL) +
    scale_colour_manual(values = cols, labels = labs_for,
                        aesthetics = c("colour", "fill"), name = NULL) +
    labs(title = w$title,
         subtitle = if (!is.null(veg)) rare_takeaway(veg, dim) else NULL,
         x = "Records sampled", y = "Diversity (Hill numbers)", linetype = NULL,
         caption = scope_cap(scope = w$scope, method = w$method, rank = "Species + Genus",
                             sig = bee_test("iNEXT rarefaction/extrapolation, Hill q0/q1/q2",
                                            "vegan rarefaction as the cross-check"))) +
    theme_beescabr(11) +                    # theme_minimal + the house text conventions
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
          legend.position = "top")
  # the open circle is vegan's INDEPENDENT estimate at the same effort. Nothing on the
  # figure said so, so it gets its own legend key: a reader has to be told what a mark
  # means, and "it agrees with the curve" is the point of drawing it at all.
  if (!is.null(pts))
    g <- g + geom_point(data = pts, aes(x, y, colour = group, shape = "vegan cross-check"),
                        inherit.aes = FALSE, size = 2.2, stroke = 0.9, fill = "white") +
             scale_shape_manual(values = c("vegan cross-check" = 21), name = NULL)

  bee_ggsave(file.path(rc_dir(dim), rare_out_name(dim, "both_ranks", "figure")), g,
             width = 10.5, height = 6.4, bg = "white")

  # the TABLE keeps both standardisations; only the figure drops the coverage curves
  tbl <- bind_rows(lapply(RC_RANKS, function(r) rc_curves(dim, r))) %>%
           mutate(estimator = "iNEXT",
                  rank = factor(rank, levels = RC_RANKS, labels = c("Species", "Genus"))) %>%
           select(rank, panel, estimator, group, x, y, lo, hi)
  if (!is.null(veg)) tbl <- bind_rows(tbl, veg %>%
      mutate(rank = factor(rank, levels = RC_RANKS, labels = c("Species", "Genus"))) %>%
      transmute(rank, panel = "Standardised by sample size", estimator = "vegan", group,
                x = rarefied_to, y = rarefied_richness,
                lo = rarefied_richness - 1.96 * rarefied_se,
                hi = rarefied_richness + 1.96 * rarefied_se))
  write.csv(tbl, file.path(rc_dir(dim), rare_out_name(dim, "both_ranks", "table")),
            row.names = FALSE)
  invisible(TRUE)
}

n <- 0
for (d in sub("^by_", "", names(RARE_WINDOWS)))
  if (!is.null(rc_one(d))) n <- n + 1

# the *_curve_* CSVs are an INTERMEDIATE: rarefaction_inext.R writes them only so this
# script can draw the curves. Leaving them would trade 16 figures for 8 tables nobody
# reads. The numbers a person wants are in the combined table beside each figure.
for (d in names(RARE_WINDOWS)) unlink(Sys.glob(file.path(rc_dir(d), "*_inext_curve_*.csv")))
message(sprintf("Combined rarefaction: %d figure+table pairs (both ranks, iNEXT with vegan cross-check) across %d window folders", n, length(RARE_WINDOWS)))
