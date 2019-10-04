context("test-example_data")

test_that("sampling rates are scalars", {
  expect_equal(prh$fs, 10)
  expect_true(is.vector(prh$fs, mode = "numeric"))
  expect_equal(prh$Afs, 400)
  expect_true(is.vector(prh$fs, mode = "numeric"))
})

test_that("times are POSIXct", {
  expect_true(is(prh$time, "POSIXct"))
  expect_true(is(prh$Atime, "POSIXct"))
})

test_that("1d elements are vectors", {
  expect_true(is.vector(prh$depth, "double"))
  expect_true(is.vector(prh$pitch, "double"))
  expect_true(is.vector(prh$roll, "double"))
  expect_true(is.vector(prh$head, "double"))
})

test_that("3d elements are arrays", {
  expect_true(is.array(prh$A))
  expect_true(dim(prh$A)[2] == 3)
})
