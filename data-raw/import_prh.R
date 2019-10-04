# Import ncdf file
prh <- RNetCDF::read.nc(RNetCDF::open.nc("data-raw/mn160727-11 10Hzprh.nc"))

# Convert times to POSIXct
dn_to_posix <- function(dn, tz = "UTC") {
  dn <- as.vector(unlist(dn))
  as.POSIXct((dn - 719529) * 86400,
             origin = "1970-01-01",
             tz = tz)
}
prh$time <- dn_to_posix(prh$time)
prh$Atime <- dn_to_posix(prh$Atime)

# Collapse scalars
prh$fs <- prh$fs[[1]]
prh$Afs <- prh$Afs[[1]]

# Convert 1d arrays to vectors
array_to_vec <- function(a) {
  as.vector(unlist(a))
}
oned_arr <- names(prh)[sapply(prh, function(x) length(dim(x)) == 1)]
for (e in oned_arr) {
  prh[[e]] <- array_to_vec(prh[e])
}

usethis::use_data(prh, overwrite = TRUE)
