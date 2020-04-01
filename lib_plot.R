# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

#' Homogenised diagnostic plots
#'
#' @param d data.frame with columns i, n, cast, outlier
#' @param name basename of the file (without extension)
#' @param width,height dimension of the plot in inches
diag_plots <- function(d, name, width=10, height=10) {
  library("ggplot2")
  library("patchwork")

  dir.create("plots", showWarnings=F)

  # outlier plot
  png(file=paste0("plots/", name, "-1.png"), width=width, height=height*0.7, units="in", res=150)
  print(
    ggplot(d) + facet_wrap(~cast, scale="free") +
    geom_point(aes(n, i, colour=outlier, size=outlier), shape=16, alpha=0.7) +
    scale_colour_manual(values=c("black", "red")) + scale_size_manual(values=c(0.1, 0.5)) +
    theme(legend.position="top")
  )
  dev.off()

  # before / after
  before <- ggplot(d) + facet_wrap(~cast, scale="free") + geom_point(aes(n, i), shape=16, alpha=0.5, size=0.2)
  after <- before %+% filter(d, !outlier)
  png(file=paste0("plots/",name, "-2.png"), width=width, height=height, units="in", res=150)
  print(before / after)
  dev.off()

  return(invisible(file))
}

