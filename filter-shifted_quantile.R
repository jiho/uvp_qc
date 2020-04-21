#' Detect outliers as particularly low number of particles
#'
#' @param x vector of number of particles
#' @param tau quantile value
#' @param k order of the moving window in which the quantile is computed
#' @param mult multiplier for the moving quantile; under mult * mv quantiles, points are considered as outliers
f <- function(x, tau=0.7, k=10, mult=0.7) {
  library("slider")
  mv_q <- slide_dbl(x, .f=quantile, p=tau, .before=k, .after=k)
  thresh <- mv_q * mult
  return(thresh)
}
