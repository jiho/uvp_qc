#' Detect outliers based on a moving quantile
#'
#' @param x vector of number of particles
#' @param k_tau order of the moving window for the quantile anomaly
#' @param tau target quantile
#' @param k_anom order of the moving window for the deviation from quantile
#' @param anom_mult multiplier of the anomaly to the quantile beyond which points are considered as outliers
f <- function(x, k_tau=1, tau=0.75, k_anom=5, anom_mult=5.3) {
  # remotes::install_github("jiho/castr")
  library("castr")

  k_tau <- round(k_tau)
  k_anom <- round(k_anom)

  # run a moving quantile
  mv_q <- slide(x, k=k_tau, stats::quantile, p=tau, na.rm=TRUE, n=1)

  # run a moving absolute deviation from the quantile
  anom <- abs(x - mv_q)
  mv_mad <- slide(anom, k=k_anom, stats::median, na.rm=TRUE)

  # detect as outliers all points that are beyond a constant * quantile deviation
  lim <- mv_q - (mv_mad * anom_mult)

  return(lim)
}
