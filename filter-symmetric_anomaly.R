#
# Assume that the anomaly around a high value quantile should be ~symmetric
# and that anything well below this is faulty =>
# - compute a high quantile
# - compute the anomaly
# - estimate the positive part of the anomaly (above the quantile)
# - flag points below the symetric of this positive part (times a coef)
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

source("read_data.R")
source("lib_plot.R")

# remotes::install_github("jiho/castr")
library("castr")

df <- d %>%
  # subset a smaller region for tests
  # filter(i>6000, i<8000) %>%
  group_by(cast) %>%
  mutate(
    # fit a high quantile in a moving window
    mv_q = slide(n, quantile, k=12, p=0.7, na.rm=TRUE),

    # anomaly to quantile
    anom = n-mv_q,

    # compute some measure of the positive anomaly (in a moving window)

    # # mean
    # up=slide(anom, k=60, function(x) {
    #   x[which(x<=0)] <- NA
    #   mean(x, na.rm=T)
    # }, n=3),

    # # weighted mean
    # up = slide(anom, k=50, function(x) {
    #   x[which(x<=0)] <- NA
    #   weighted.mean(x, cweights((length(x)-1)/2), na.rm=T)
    # }),

    # smoothed median
    up = slide(anom, k=50, function(x) {median(x[which(x>0)], na.rm=T)}) %>% smooth(k=20),

    # # high quantile
    # up = slide(anom, k=20, function(x) {quantile(x[which(x>0)], p=0.8)}),

    # # max
    # up = slide(anom, k=10, max, na.rm=T),

    # detect outliers = those beyond variance x mutliplier
    limit = -8 *  up,
    outlier = anom < limit
  ) %>%
  ungroup()

# plot to check
ggplot(df) + facet_wrap(~cast, scale="free_x") +
    geom_point(aes(anom, i), shape=16, size=0.2) +
    geom_path(aes(up, i), colour="darkgreen") +
    geom_path(aes(limit, i), colour="red")
    # geom_point(aes(anom, i, colour=outlier), shape=16, size=0.1) +
    # geom_vline(xintercept=0, colour="red", alpha=0.5) +
    # geom_path(aes(-8*up, i), colour="blue")

diag_plots(df, "filter-symmetric_anomaly")
