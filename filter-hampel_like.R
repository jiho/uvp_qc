#' Detect outliers based on a moving quantile
#'
#' @param x vector of number of particles
#' @param k_tau order of the moving window for the quantile anomaly
#' @param tau target quantile
#' @param k_anom order of the moving window for the deviation from quantile
#' @param anom_mult multiplier of the anomaly to the quantile beyond which points are considered as outliers
f <- function(x, k_tau=1, tau=0.75, k_anom=5, anom_mult=5.3) {
  library("slider")

  k_tau <- round(k_tau)
  k_anom <- round(k_anom)

  # run a moving quantile
  mv_q <- slide_dbl(x, .f=stats::quantile, p=tau, .before=k_tau, .after=k_tau)

  # run a moving absolute deviation from the quantile
  anom <- abs(x - mv_q)
  mv_mad <- slide_dbl(anom, .f=stats::median, .before=k_anom, .after=k_anom)

  # detect as outliers all points that are beyond a constant * quantile deviation
  lim <- mv_q - (mv_mad * anom_mult)

  return(lim)
}
