context("test-speed_estimates")

test_that("speeds match Matlab script", {
  prh_desc <- jgl_desc(prhdata::prh)
  prh_ocdr <- jgl_ocdr(prh_desc)
  acc_filt <- jgl_filtacc(prhdata::Araw, 10, 90)
  prh_rms <- jgl_accrms(prh_ocdr, acc_filt)
  prh_spd <- jgl_fit(prh_rms)

  dplyr::filter(prh_spd,
                dplyr::between(time, min(prh_expl$time), max(prh_expl$time)))
  expect_equal(prh_spd$speed, speeds, tolerance = 0.2)
})
