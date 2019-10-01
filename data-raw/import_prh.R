prh <- RNetCDF::read.nc(open.nc("data-raw/mn160727-11 10Hzprh.nc"))
prh$timestamp_local <- as.POSIXct((prh$timestamp_local - 719529) * 86400,
                                  origin = "1970-01-01",
                                  tz = "US/Pacific")
usethis::use_data(prh, overwrite = TRUE)
