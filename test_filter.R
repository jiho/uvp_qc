#
# Load filtering functions and test their efficacy on a subset of data
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

library("tidyverse")

## Prepare ----

# read test data
d <- read_csv("test_data.csv.gz", col_types=cols())

#' Homogenised diagnostic plots
#'
#' @param d data.frame with columns id, i, n, outlier
#' @param name basename of the file (without extension)
#' @param width,height dimension of the plot in inches
plot_diagnostics <- function(d, name, size=15) {
  library("ggplot2")
  library("patchwork")

  dir.create("test_plots", showWarnings=F)

  # outlier plot
  png(file=paste0("test_plots/", name, "-1.png"), width=size, height=size*1.5, units="in", res=150)
  print(
    ggplot(d) + facet_wrap(~id, scale="free_x") +
    geom_point(aes(n, -i, colour=outlier, size=outlier), shape=16, alpha=0.7) +
    scale_colour_manual(values=c("black", "red")) + scale_size_manual(values=c(0.1, 0.5)) +
    theme(legend.position="top")
  )
  dev.off()

  # before / after
  before <- ggplot(d) + facet_wrap(~id, scale="free_x") + geom_point(aes(n, -i), shape=16, alpha=0.5, size=0.2)
  after <- before %+% filter(d, !outlier)
  png(file=paste0("test_plots/",name, "-2.png"), width=size*2, height=size, units="in", res=150)
  print(before + after)
  dev.off()

  return(invisible(file))
}

## Run filter ----

filter <- "hampel_like"

source(str_c("filter-", filter, ".R"))

df <- d %>%
  group_by(id) %>%
  mutate(outlier=hampel_q(n, k_tau=10,tau=0.7, k_anom=30, anom_mult=4, n_max=3)) %>%
  ungroup()

plot_diagnostics(df, filter)



