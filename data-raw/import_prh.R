# Import ncdf file
prh_path <- "data-raw/mn160727-11 10Hzprh.nc"
prh_list <- RNetCDF::read.nc(RNetCDF::open.nc(prh_path))

# Convert times to POSIXct
dn_to_posix <- function(dn, tz = "UTC") {
  dn <- as.vector(unlist(dn))
  as.POSIXct((dn - 719529) * 86400,
             origin = "1970-01-01",
             tz = tz)
}
prh_list$time <- dn_to_posix(prh_list$time)
prh_list$Atime <- dn_to_posix(prh_list$Atime)

# Collapse scalars
prh_list$fs <- prh_list$fs[[1]]
prh_list$Afs <- prh_list$Afs[[1]]

# Convert 1d arrays to vectors
array_to_vec <- function(a) {
  as.vector(unlist(a))
}
oned_arr <- names(prh_list)[sapply(prh_list, function(x) length(dim(x)) == 1)]
for (e in oned_arr) {
  prh_list[[e]] <- array_to_vec(prh_list[e])
}

# Create data frame with attributes for fs, Afs, and A
prh_raw <- prh_list[c("time", "depth", "pitch", "roll", "head")] %>%
  as.data.frame
prh_raw$Aw <- prh_list$Aw
prh_raw$Mw <- prh_list$Mw
prh_raw$Gw <- prh_list$Gw
attr(prh_raw, "fs") <- prh_list$fs
attr(prh_raw, "Afs") <- prh_list$Afs
attr(prh_raw, "A") <- prh_list$A

# Slice a PRH
slice_prh <- function(prh_, i) {
  result <- prh_[i, ]
  Astart <- ((min(i) - 1) * attr(prh_, "Afs") / attr(prh_, "fs")) + 1
  Aend <- (max(i) * attr(prh_, "Afs") / attr(prh_, "fs"))
  Ax <- Astart:Aend
  attr(result, "A") <- attr(result, "A")[Ax, ]
  result
}

# Start PRH where dives get deeper
prh_start <- as.POSIXct("2016-07-27 13:07", tz = "UTC")
start_i <- which.min(abs(prh_raw$time - prh_start))
prh_deep <- slice_prh(prh_raw, start_i:nrow(prh_raw))

# Keep only the first 3MB of data (for CRAN purposes)
get_prh_size <- function(nrows, prh_) {
  tmp_path <- tempfile(tmpdir = "data-raw", fileext = ".rda")
  tmp_prh <- slice_prh(prh_, 1:nrows)
  save(tmp_prh, file = tmp_path, compress = "bzip2")
  size_mb <- file.size(tmp_path) / 1e6
  file.remove(tmp_path)
  size_mb
}
nrows <- floor(seq(100, nrow(prh_deep), length.out = 4))
prh_sizes <- sapply(nrows, get_prh_size, prh_ = prh_deep)
size_lm <- lm(prh_sizes ~ nrows)
b <- coef(size_lm)[1]
m <- coef(size_lm)[2]
final_size <- 3
optim_nrows <- floor((final_size - b) / m)

prh <- slice_prh(prh_deep, 1:optim_nrows)

usethis::use_data(prh, overwrite = TRUE)
