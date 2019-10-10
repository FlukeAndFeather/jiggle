context("test-example_data")

# Adapted from prhdata's test-prh_expl.R

test_that("prh_expl size is correct", {
  expect_equal(nrow(prh_expl), 9600)
  expect_equal(ncol(prh_expl), 8)
})

# Custom expectation for vector domain
expect_between <- function(object, low, high) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")

  # 2. Call expect()
  expect(
    all(act$val >= low & act$val <= high, na.rm = TRUE),
    sprintf("Some elements of %s fall outside [%.2f, %.2f]", act$lab, low, high)
  )

  # 3. Invisibly return the value
  invisible(act$val)
}

test_that("prh_expl variables fall within domains", {
  expect_between(prh_expl$depth, -5, 500)
  expect_between(prh_expl$pitch, -pi / 2, pi / 2)
  expect_between(prh_expl$roll, -pi, pi)
  expect_between(prh_expl$head, -pi, pi)
  expect_between(prh_expl$Aw, -5, 5)
  expect_between(prh_expl$Mw, -75, 75)
  expect_between(prh_expl$Gw, -10, 10)
})

test_that("sizes of prh_expl and Araw align", {
  expect_equal(nrow(Araw_expl) / 40, nrow(prh_expl))
})

test_that("prh_expl$time is between 14:21 and 14:37 local time (US/Pacific)", {
  expect_s3_class(prh_expl$time, "POSIXct")
  expect_between(prh_expl$time,
                 as.POSIXct("2016-07-27 14:21", tz = "US/Pacific"),
                 as.POSIXct("2016-07-28 14:37", tz = "US/Pacific"))
})

test_that("time lines up in both data frames", {
  expect_equal(prh_expl$time[1], Araw_expl$time[1])
  expect_equal(max(Araw_expl$time) + 1 / attr(Araw_expl, "Afs"),
               max(prh_expl$time) + 1 / attr(prh_expl, "fs"))
})
