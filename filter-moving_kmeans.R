k_outlier <- function(x, qwindow=8, swindow=qwindow*2, q_low=0.25, q_high=0.75, mult=1) {
  # plot(x)

  # message("foo1")
  nx <- length(x)

  mv_q <- slider::slide_dbl(x, quantile, probs=q_high, .before=qwindow, .after=qwindow)
  x_anom <- x - mv_q
  # plot(x_anom)

  # message("foo2")
  nw <- 2*swindow+1
  suppressMessages(res <- slider::slide_dfr(x_anom, function(piece) {
    np <- length(piece)

    q <- quantile(piece, q_low)
    k <- kmeans(piece, centers=c(0,q))

    # define when to make two groups

    # based on spread and difference among the centers
    # spread <- mad(piece) * mult
    # two_cluster <- abs(diff(k$centers)) > spread

    # based on sum of squares total va intra
    # two_clusters <- k$totss > (mean(k$withinss) * mult)

    # based on distance among cluster centers vs mad in first cluster
    two_clusters <- abs(diff(k$centers)) > (mad(piece[k$cluster == 1]) * mult)

    if (two_clusters) {
      out <- k$cluster
    } else {
      out <- rep(1, length=np)
    }
    # take care of the start and end
    if (np != nw) {
      out <- c(rep(NA, nw-np), out)
    }
    return(out)
  }, .before=swindow, .after=swindow))

  # message("foo3")
  # convert into the proper shape matrix to be able to extract diagonal
  res <- as.matrix(res[,nw:1])
  na_pad <- matrix(NA, nrow=swindow, ncol=nw)
  # pad start and end
  res <- rbind(na_pad, res, na_pad)

  # message("foo4")
  # extract the majority on the diagonal
  maj <- function(x) {which.max(tabulate(x))}
  sapply(1:nx, function(i) {maj(diag(res[i:(nw+i-1),1:nw]))}) %>% as.factor()
}

system.time(
dk <- d %>%
  filter(i>=5000, i<=5500) %>%
  group_by(cast) %>%
  mutate(outlier=k_outlier(n, qwindow=8, mult=6))
)

library("parallel")
system.time(
dk <- d %>%
  # filter(i>=5000, i<=8000) %>%
  split(.$cast) %>%
  # partition(cluster) %>%
  mclapply(function(p) { mutate(p, outlier=k_outlier(n, qwindow=8, q_high=0.7, mult=5.6)) }, mc.cores=3) %>%
  bind_rows(.id="cast")
)

ggplot(dk) + facet_wrap(~cast, scale="free") +
  geom_point(aes(n, i, colour=outlier, size=outlier), shape=16, alpha=0.7) +
  scale_colour_manual(values=c("black", "red")) + scale_size_manual(values=c(0.1, 0.5))
