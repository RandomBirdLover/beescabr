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

RC_DIR <- file.path(DIR_REPORT, "richness/rarefaction/fair_method_2021_2023")
rc_rd  <- function(f) { p <- file.path(RC_DIR, f)
                        if (file.exists(p)) read.csv(p, stringsAsFactors = FALSE) else NULL }

# one combined figure + table per (comparison, rank)
rc_one <- function(dim, rank) {
  size <- rc_rd(sprintf("rarefaction_by_%s_inext_curve_size_%s.csv",     dim, rank))
  cov  <- rc_rd(sprintf("rarefaction_by_%s_inext_curve_coverage_%s.csv", dim, rank))
  veg  <- rc_rd(sprintf("rarefaction_by_%s_vegan_%s.csv",             dim, rank))
  if (is.null(size) || is.null(cov)) return(invisible(NULL))

  QL <- c("0" = "q = 0  richness", "1" = "q = 1  Shannon", "2" = "q = 2  Simpson")
  q0 <- function(d, panel) d %>%
          transmute(group = Assemblage, x = m, y = qD, lo = qD.LCL, hi = qD.UCL,
                    kind = Method, panel = panel, q = QL[as.character(Order.q)])
  covq <- cov %>%
            transmute(group = Assemblage, x = SC, y = qD, lo = qD.LCL, hi = qD.UCL,
                      kind = Method, panel = "Standardised by coverage",
                      q = QL[as.character(Order.q)])
  dat <- bind_rows(q0(size, "Standardised by sample size"), covq)
  dat$panel <- factor(dat$panel, levels = c("Standardised by sample size",
                                            "Standardised by coverage"))
  pts <- if (!is.null(veg)) veg %>%
           transmute(group, x = rarefied_to, y = rarefied_richness,
                     panel = factor("Standardised by sample size", levels = levels(dat$panel)),
                     q = "q = 0  richness") else NULL

  g <- ggplot(dat, aes(x, y, colour = group, fill = group)) +
    geom_ribbon(aes(ymin = lo, ymax = hi, group = interaction(group, kind)), alpha = 0.15, colour = NA) +
    geom_line(aes(linetype = kind, group = interaction(group, kind)), linewidth = 0.7) +
    facet_grid(q ~ panel, scales = "free", switch = "y") +
    labs(title = sprintf("Species richness at equal effort, by %s",
                         if (dim == "method") "survey method" else "surveyor type"),
         subtitle = paste("iNEXT curves with 95% intervals.",
                          if (!is.null(pts)) "Points are vegan's rarefied richness, as a cross-check." else ""),
         x = NULL, y = sprintf("%s diversity (Hill numbers)", rank),
         colour = NULL, fill = NULL, linetype = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top", plot.title = element_text(face = "bold"))
  if (!is.null(pts)) g <- g + geom_point(data = pts, aes(x, y, colour = group),
                                         inherit.aes = FALSE, size = 2.4, shape = 21, stroke = 0.9, fill = "white")

  bee_ggsave(file.path(RC_DIR, sprintf("rarefaction_by_%s_%s.png", dim, rank)), g,
             width = 10, height = 9.5, bg = "white")

  tbl <- dat %>% mutate(estimator = "iNEXT") %>% select(panel, estimator, group, x, y, lo, hi)
  if (!is.null(veg)) tbl <- bind_rows(tbl, veg %>%
      transmute(panel = "Standardised by sample size", estimator = "vegan", group,
                x = rarefied_to, y = rarefied_richness,
                lo = rarefied_richness - 1.96 * rarefied_se,
                hi = rarefied_richness + 1.96 * rarefied_se))
  write.csv(tbl, file.path(RC_DIR, sprintf("rarefaction_by_%s_%s.csv", dim, rank)), row.names = FALSE)
  invisible(TRUE)
}

n <- 0
for (d in c("method", "observer")) for (r in c("species", "genus"))
  if (!is.null(rc_one(d, r))) n <- n + 1

# the *_curve_* CSVs are an INTERMEDIATE: rarefaction_inext.R writes them only so this
# script can draw the curves. Leaving them would trade 16 figures for 8 tables nobody
# reads. The numbers a person wants are in the combined table beside each figure.
unlink(Sys.glob(file.path(RC_DIR, "*_inext_curve_*.csv")))
message(sprintf("Combined rarefaction: %d figure+table pairs (iNEXT with vegan cross-check) in %s", n, RC_DIR))
