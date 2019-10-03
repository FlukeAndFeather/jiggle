context("test-zerocross")

test_that("trivial zero-crossing events succeed", {
  expect_equal(find0cross(c(1, -1)), c(FALSE, TRUE))
  expect_equal(find0cross(c(-1, 1)), c(FALSE, TRUE))
})

test_that("vectors without zero-crossing events succeed", {
  expect_equal(find0cross(c(1, 1)), c(FALSE, FALSE))
  expect_equal(find0cross(c(-1, -1)), c(FALSE, FALSE))
  expect_equal(find0cross(c(1, 0)), c(FALSE, FALSE))
  expect_equal(find0cross(c(-1, 0)), c(FALSE, FALSE))
})

test_that("zero-crossing events in vectors with leading 0s succeed", {
  expect_equal(find0cross(rep(0, 5)), rep(FALSE, 5))
  expect_equal(find0cross(c(rep(0, 5), 1)), c(rep(FALSE, 6)))
  expect_equal(find0cross(c(rep(0, 5), 1, -1)), c(rep(FALSE, 6), TRUE))
})

test_that("zero-tangent events aren't crossings", {
  expect_equal(find0cross(c(1, 0, 1)), rep(FALSE, 3))
  expect_equal(find0cross(c(1, 0, 0, 1)), rep(FALSE, 4))
})

test_that("signs carry through zeros", {
  expect_equal(find0cross(c(1, 0, -1)), c(FALSE, FALSE, TRUE))
})
