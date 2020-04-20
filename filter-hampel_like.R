#
# Hampel-like outlier filter
# 1. run a moving quantile
# 2. run a moving absolute deviation from the quantile
# 3. detect as outliers all points that are beyond a constant * quantile deviation
# + run this in a repeated fashion
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

# remotes::install_github("jiho/castr")
library("castr")

#' Detect outliers based on a moving quantile
#'
#' @param k_tau order of the moving window for the quantile anomaly
#' @param tau target quantile
#' @param k_anom order of the moving window for the deviation from quantile
#' @param anom_mult multiplier of the anomaly to the quantile beyond which points are considered as outliers
#' @param n_max maximum number of iterations
hampel_q <- function(x, k_tau=1, tau=0.75, k_anom=5, anom_mult=5.3, n_max=10) {
  k_tau <- round(k_tau)
  k_anom <- round(k_anom)

  # initiate empty outliers
  outliers <- out <- rep(FALSE, times=length(x))

  for (i in 1:n_max) {
    # remove outliers
    outliers <- outliers | out
    x[outliers] <- NA
    # and iterate
    mv_q <- slide(x, k=k_tau, stats::quantile, p=tau, na.rm=TRUE, n=1)
    anom <- abs(x - mv_q)
    mv_mad <- slide(anom, k=k_anom, stats::median, na.rm=TRUE)
    out <- (anom > mv_mad * anom_mult)
    if (!any(out, na.rm = TRUE)) {
      break
    }
  }
  return(outliers)
}
