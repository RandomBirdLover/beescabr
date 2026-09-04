# =============================================================
# analysis/shared/utils_analysis.R
# beescabr pipeline -- package guard for the ANALYSIS scripts
# Created: 2026-07-21
#
# Installs the R packages the `scripts/analysis/` runs need, if they're missing.
# Kept SEPARATE from scripts/utils/utils.R on purpose: the core pipeline should
# never pull analysis-only packages (vegan / igraph / bipartite). Source this
# once before running the analysis scripts:
#
#   source("scripts/analysis/shared/utils_analysis.R")
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
# The analysis packages used to be listed again here, and installed on load. That was a
# second list to keep in sync with config.R (it held 10 of the 36 and had drifted), and it
# installed software as a side effect of sourcing a utility file. One list, one installer.
# Dependencies are CHECKED here, not installed: see beescabr_require() in config.R.
if (!exists("beescabr_require")) source("scripts/config.R")
beescabr_require()
message("All ", length(BEESCABR_PACKAGES), " analysis packages ready.")
