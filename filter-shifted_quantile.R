#
# Assume that number of particles on the faulty iamges is ~0.5 of the one in
# correct images =>
# 1. compute the correct number as a high quantile (e.g. 0.75)
# 2. consider that incorrect values will be ~0.5 x this quantile
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
    # Fit a high quantile
    # moving quantile
    mv_q=slide(n, quantile, k=12, p=0.7, na.rm=TRUE),
    # local linear quantile regression
    # mv_q=quantreg::lprq(i, n, h=6, tau=0.75, m=n())$fv,
    # moving weighted quantile
    # mv_q=slide(n, k=12, Hmisc::wtd.quantile, p=0.75, na.rm=TRUE, weights=cweights(12), normwt=TRUE),

    # detect outliers = those below the quantile shifted by a proportion
    outlier = n < mv_q * 0.7
  ) %>%
  ungroup()

# plot to check
ggplot(df) + facet_wrap(~cast, scale="free_x") +
    geom_point(aes(n, i), shape=16, size=0.2) +
    geom_path(aes(mv_q, i), colour="blue") +
    geom_path(aes(mv_q*0.7, i), colour="red")
    # geom_point(aes(anom, i, colour=outlier), shape=16, size=0.1) +
    # geom_vline(xintercept=0, colour="red", alpha=0.5) +
    # geom_path(aes(-8*up, i), colour="blue")

diag_plots(df, "filter-shifted_quantile")
