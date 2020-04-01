#
# Hampel-like outlier filter
# 1. run a moving quantile
# 2. run a moving absolute deviation from the quantile
# 3. detect as outliers all points that are beyond a constant * quantile deviation
# + run this in a repeated fashion
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

source("read_data.R")
source("lib_plot.R")

# remotes::install_github("jiho/castr")
library("castr")

#' Detect outliers based on a moving quantile
#'
#' @param k1 order of the moving window for the quantile anomaly
#' @param k2 order of the moving window for the deviation from quantile
#' @param tau quantile
#' @param mult multiplier of the anomaly to the quantile beyond which points are considered as outliers
#' @param n.max maximum number of iterations
hampel_q <- function(x, k1=1, k2=5, tau=0.75, mult=5.3, n.max=10) {
  k1 <- round(k1)
  k2 <- round(k2)
  # initiate empty outliers
  outliers <- rep(FALSE, times=length(x))

  for (i in 1:n.max) {
    mv_q <- slide(x, k=k1, stats::quantile, p=tau, na.rm=TRUE, n=1)
    anom <- abs(x - mv_q)
    mv_mad <- slide(anom, k=k2, stats::median, na.rm=TRUE)
    out <- (anom > mv_mad * mult)
    if (!any(out, na.rm = TRUE)) {
      break
    }
    # remove outliers
    outliers <- outliers | out
    x[outliers] <- NA
    # and iterate
  }
  return(outliers)
}

df <- d %>%
  # filter(i > 6000, i < 9000) %>%
  group_by(cast) %>%
  mutate(outlier=hampel_q(n, k1=10, k2=40, tau=0.7, mult=5, n.max=3)) %>%
  ungroup()

diag_plots(df, "filter-hampel_like")



