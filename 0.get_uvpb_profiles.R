#
# Get UVP profiles from the server plankton.obs-vlfr.fr
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

library("tidyverse")
library("data.table")

root <- "/plankton/local_plankton/uvp/uvp_b"
projs <- list.dirs(root, recursive=FALSE) %>%
  str_subset("/uvp5")

dir.create("projs", showWarnings=F)

ids <- 1:length(projs)
# ids <- 10:15
L <- parallel::mclapply(ids, function(i) {
# L <- lapply(ids, function(i) {
  p <- projs[i]
  p_name <- basename(p)

  rds_file <- str_c("projs/", p_name, ".rds")

  if (!file.exists(rds_file)) {
    message(i, " : ", p_name)


    # initialise empty list
    l <- list()

    # define meta file
    metafile <- str_c(p, "/meta/", str_replace(p_name, "uvp5_", "uvp5_header_"), ".txt")

    if (file.exists(metafile)) {
      # read it if it exists
      m <- read_delim(metafile, delim=";",
                      col_types=cols_only(profileid="c", firstimage="i"))

      # define datfiles and read existing ones
      m$datfile <- str_c(p, "/results/", m$profileid, "_datfile.txt")
      m <- m[file.exists(m$datfile),]

      if (nrow(m) > 0 ) {
        # read all datfiles
        l <- lapply(1:nrow(m), function(j) {
          # message(j)
          d <- fread(m$datfile[j], sep=";", strip.white=TRUE) %>%
            select(img=V1, press=V3, n=V15, mean_area=V16, mean_grey=V17, n_large=V18, mean_grey_large=V19) %>%
            mutate(press=as.numeric(press)/10)
          # d <- vroom(m$datfile[j], col_names=F, delim=";", col_types=cols()) %>%
          #   select(img=X1, press=X3, n=X15)
          # -> slower!

          # qplot(img, -press, data=d)

          d <- d[1:which.max(d$press),] %>%
            filter(img >= m$firstimage[j])

          n <- nrow(d)
          if (n>0) {d$i <- 1:n}

          return(d)
        })
        names(l) <- basename(m$profileid)
      }
    }
    saveRDS(l, file=rds_file)
  }
  return(invisible(NULL))
}, mc.cores=1)
# })
