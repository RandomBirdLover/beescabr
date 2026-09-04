# =============================================================
# analysis/shared/inext_estimates.R
# beescabr -- tidy iNEXT's three result tables into one long table
# =============================================================

# Tidy the three iNEXT result tables into one long table --------------------------
#
# iNEXT hands back three tables per comparison: AsyEst (the extrapolated ceiling),
# estimateD(base = "size") and estimateD(base = "coverage"). Written straight to disk
# that is three files per rank, six per comparison, twelve for the fair window -- all
# with iNEXT's own column names (Order.q, qD.LCL, s.e.) and all at the SAME grain:
# one row per assemblage x Hill q. They are three answers to one question, so they
# belong in one table with a `basis` column saying which standardization produced
# the row, not in three files a reader has to line up by hand.
#
# Pure function, no I/O -- tested in tests/testthat/test-inext-estimates.R.

# AsyEst spells Hill q as an English phrase; the estimateD tables use the number.
# Map the phrase back so one `q` column serves every basis.
INEXT_Q_LABEL <- c("Species richness" = 0, "Shannon diversity" = 1, "Simpson diversity" = 2)

INEXT_EST_COLS <- c("rank", "basis", "group", "q", "n", "coverage",
                    "observed", "diversity", "se", "lcl", "ucl")

# one estimateD() table -> the common shape. `basis` names the standardization.
.inext_std_rows <- function(est, rank, basis) {
  data.frame(rank = rank, basis = basis, group = as.character(est$Assemblage),
             q = as.integer(est$Order.q), n = as.numeric(est$m),
             coverage = as.numeric(est$SC), observed = NA_real_,
             diversity = as.numeric(est$qD), se = NA_real_,
             lcl = as.numeric(est$qD.LCL), ucl = as.numeric(est$qD.UCL),
             stringsAsFactors = FALSE)
}

# asy: iNEXT()$AsyEst. est_size / est_cov: estimateD(base = "size" / "coverage").
# Asymptotic rows have no n or coverage (they are the limit, not a standardization),
# and only they carry `observed` -- the raw count the estimate extrapolates from.
inext_estimates_tidy <- function(asy, est_size, est_cov, rank) {
  a <- data.frame(
    rank = rank, basis = "asymptotic", group = as.character(asy$Assemblage),
    q = unname(INEXT_Q_LABEL[as.character(asy$Diversity)]),
    n = NA_real_, coverage = NA_real_, observed = as.numeric(asy$Observed),
    diversity = as.numeric(asy$Estimator), se = as.numeric(asy[["s.e."]]),
    lcl = as.numeric(asy$LCL), ucl = as.numeric(asy$UCL), stringsAsFactors = FALSE)
  out <- rbind(a,
               .inext_std_rows(est_size, rank, "equal_size"),
               .inext_std_rows(est_cov,  rank, "equal_coverage"))
  out[, INEXT_EST_COLS]
}
