# Subset of the example PRH data from Cade et al. 2018 JEB (2 dives)
# Downsamples acceleration to 200 Hz
library(prhdata)
library(dplyr)
library(usethis)
sub_start <- as.POSIXct("2016-07-27 14:21", tz = "US/Pacific")
sub_end <- as.POSIXct("2016-07-27 14:37", tz = "US/Pacific")

prh_expl <- filter(prh, between(time, sub_start, sub_end))
Araw_expl <- filter(Araw, between(time, sub_start, sub_end)) %>%
  slice(seq(1, nrow(Araw), by = 2))
attr(Araw_expl, "Afs") <- 200

use_data(prh_expl, Araw_expl, overwrite = TRUE)
