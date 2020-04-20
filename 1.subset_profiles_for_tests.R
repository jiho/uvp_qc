#
# Get a subset of profiles selected by Marc as representative of various conditions
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

library("tidyverse")

# list test profiles
test_names <- list.files("/plankton/local_plankton/uvp/uvp_b/plot_examples_for_dev/") %>%
  str_replace("UVP5_", "") %>% str_replace(".jpg", "")

# list all profiles
proj_files <- list.files("projs", full.names=TRUE)

profiles_record <- map_dfr(proj_files, function(f) {
  d <- read_rds(f)
  if ( length(d) > 0 ) {
    data.frame(proj=f, profiles=names(d))
  }
})

# now find the target profiles
test_records <- filter(profiles_record, profiles %in% test_names)

# and read them
test <- test_records %>% group_by(proj) %>%
  do({
    l <- read_rds(.$proj[1])
    d <- map_dfr(.$profiles, function(p) {l[[p]]}, .id="id")
    d$id <- .$profiles[as.numeric(d$id)]
    d
  }) %>% ungroup() %>%
  select(-proj)

# select only the top part because we already get all cases this way
test <- filter(test, i <= 5000)

# reorder columns
test <- select(test, id, i, press, n)

ggplot(test, aes(x=n, y=-i)) + facet_wrap(~id, scales="free_x") + geom_point(shape=".")

write_csv(test, "test_data.csv.gz")

