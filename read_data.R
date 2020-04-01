#
# Prepare work environment
# = load common packages and read data
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

suppressMessages(library("tidyverse", warn.conflicts=FALSE))

# read all
va <- read_delim("data/cast_va_datfile.txt", delim=";", col_names=FALSE, col_types=cols(), trim_ws=T) %>% select(n=X15) %>% mutate(i=1:n(), cast="va")
vc <- read_delim("data/cast_vc_datfile.txt", delim=";", col_names=FALSE, col_types=cols(), trim_ws=T) %>% select(n=X15) %>% mutate(i=1:n(), cast="vc")
xb <- read_delim("data/cast_xb_datfile.txt", delim=";", col_names=FALSE, col_types=cols(), trim_ws=T) %>% select(n=X15) %>% mutate(i=1:n(), cast="xb")
# combine
d <- bind_rows(va, vc, xb)
# eliminate extremes that screw up plots and extremes which we know are wrong
d <- filter(d, n < 5000, n> 10)

# base_plot <- ggplot(d) + facet_wrap(~cast, scale="free") + geom_point(aes(n, i), shape=16, alpha=0.5, size=0.2)
# base_plot
