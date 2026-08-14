# =============================================================
# analysis/utils_analysis.R
# beescabr pipeline -- package guard for the ANALYSIS scripts
# Created: 2026-07-21
#
# Installs the R packages the `scripts/analysis/` runs need, if they're missing.
# Kept SEPARATE from scripts/utils/utils.R on purpose: the core pipeline should
# never pull analysis-only packages (vegan / igraph / bipartite). Source this
# once before running the analysis scripts:
#
#   source("scripts/analysis/utils_analysis.R")
#
# ...or it is safe to source from the top of any analysis script (it no-ops when
# everything is already installed). Same requireNamespace + try(install) pattern
# as utils.R's pdftools guard, so an offline machine just warns and moves on.
# =============================================================

# Packages used across the analysis scripts:
#   dplyr / stringr  -- every script (data wrangling, string helpers)
#   vegan            -- genera_and_species_accumulation.R (specaccum / rarefy / specpool)
#   igraph           -- interactions_network.R (bipartite graph + projection)
#   bipartite        -- interactions_network.R (plotweb visitation webs)
#   ggplot2          -- interactions_network.R heatmaps + phenology figures
#   ggridges         -- phenology_activity.R (ridgeline density plots)
#   ggpattern        -- hatched/patterned fills: texture as a 2nd channel (e.g. method) so ggplot
#                       fills stay distinguishable in grayscale / print / for color-blind viewers
#                       (base-R figures hatch natively via density/angle -- no package needed)
ANALYSIS_PACKAGES <- c("dplyr", "stringr", "vegan", "igraph", "bipartite", "ggplot2", "ggridges", "sf", "iNEXT", "ggpattern")

for (pkg in ANALYSIS_PACKAGES) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing analysis package: ", pkg)
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
  }
}

# report what is / isn't available so a failed install is obvious
.installed <- vapply(ANALYSIS_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
if (all(.installed)) {
  message("All analysis packages ready: ", paste(ANALYSIS_PACKAGES, collapse = ", "))
} else {
  message("STILL MISSING (install by hand): ",
          paste(ANALYSIS_PACKAGES[!.installed], collapse = ", "))
}
rm(.installed)
