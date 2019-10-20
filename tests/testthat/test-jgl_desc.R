context("test-jgl_desc")

test_that("descent phases are distinct", {
  prh_desc <- jgl_desc(prhdata::prh)
  # Descent phases 50/51 and 81/82 overlap
  overlaps <- prh_desc %>%
    tidyr::drop_na(desc_id) %>%
    dplyr::group_by(time) %>%
    dplyr::summarize(N = n()) %>%
    dplyr::filter(N > 1)
  expect_equal(nrow(overlaps), 0)
})
