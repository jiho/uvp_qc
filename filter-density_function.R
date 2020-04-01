#
# Detect faulty images as a bimodality in the probability density function (PDF)
# 1. run a moving quantile with a high value (= normal data)
# 2. compute the anomaly
# 3. in a larger moving window, estimate the PDF
# 4. define some heuristics to detect bimodality and cut the first mode
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

source("read_data.R")
source("lib_plot.R")

library("slider")

df <- d %>%
  # filter(i>6000, i<9000) %>%
  group_by(cast) %>%
  mutate(
    # moving quantile
    mv_q=slide_dbl(n, quantile, prob=0.7, .before=8, .after=8),

    # anomaly
    anom=n-mv_q,

    # moving PDF on anomaly
    split=slide2_dbl(anom, mv_q, .before=50, .after=50, .f=function(x, y) {
      this_q <- y[51]

      # initialise the split points between the mode at NA
      split <- NA

      # fit density
      # (and make sure it is smooth enough that we do not have more than two local maxima)
      local_max_idx <- c(1,2,3)
      adjust <- 1
      while (length(local_max_idx) > 2) {
        # fit density
        dens <- density(x, n=128, adjust=adjust)

        # detect local maxima
        # https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
        dd <- diff(sign(diff(dens$y)))
        local_max_idx <- which(dd==-2)+1
        adjust <- adjust + 0.1
      }

      # only consider a split if there are two modes (otherwise: unimodal)
      if (length(local_max_idx) == 2) {
        # compute the minium between the two maxima
        local_min_idx <- which.min(dens$y[local_max_idx[1]:local_max_idx[2]]) + local_max_idx[1] - 1

        # only consider cases when the minimum is in a region of negative anomaly
        # otherwise, we are picking a positive outlier and we don't care about those
        if (dens$x[local_min_idx] < 0) {
          # get the corresponding density values
          local_max <- sort(dens$y[local_max_idx], decreasing=TRUE)
          local_min <- dens$y[local_min_idx]

          # decide whether data is bimodal based on the shape of the maxima/minima
          if (
            (
              (this_q + dens$x[local_max_idx[1]]) < (this_q * 0.75)
            ) & (
              # min is really close to zero = clear separation
              local_min < 10^-3
              |
              # ratio of largest / smallest max, relative to the min is small
              # = we have two significant peaks separated by a low point
              ((local_max[1]-local_min) / (local_max[2]-local_min)) < 4
            )
          ) {
            # then pick the value of the low point as the split
            split <- dens$x[local_min_idx]
          }
        }
      }
      return(split)
    }),

    # now define outliers
    outlier = ifelse(is.na(split) | anom > split, FALSE, TRUE)
  )
# looong...

ggplot(df) + facet_wrap(~cast, scale="free_x") +
  # geom_point(aes(n, i), size=0.5) +
  # geom_path(aes(mv_q, i), colour="dodgerblue", alpha=0.5)
  geom_point(aes(anom, -i, colour=outlier), size=0.1) +
  geom_point(aes(split, -i), colour="red", size=0.1)

diag_plots(df, "filter-density_function")
