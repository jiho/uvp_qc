#' Detect outliers as particularly low number of particles
#'
#' @param x vector of number of particles
#' @param tau quantile value
#' @param k order of the moving window in which the quantile is computed
#' @param mult multiplier for the moving quantile; under mult * mv quantiles, points are considered as outliers
f <- function(x, tau=0.7, k=10, min_mult=0.5, max_mult=0.7, min_n=20, max_n=800) {
  library("slider")
  mv_q <- slide_dbl(x, .f=quantile, p=tau, .before=k, .after=k)
  sig <- function(x, min=min_mult, max=max_mult, start=min_n, end=max_n) {
    min + stats::pbeta((x - start) / (end - start), 3, 3) * (max - min)
  }
  thresh <- mv_q * sig(mv_q)
  return(thresh)
}


