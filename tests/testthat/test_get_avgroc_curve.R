library(testthat)

test_that("Testing get_avgroc_curve", {
  expect_error(get_avgroc_curve(c(0), 2, 2))
  expect_error(get_avgroc_curve(c(0,3,4), 5, 2))
  expect_vector(get_avgroc_curve(c(1,2),2, 2), size=2)
  expect_vector(get_avgroc_curve(c(1,2),2, 100), size=100)
  expect_vector(get_avgroc_curve(c(1,2,3),2, 0), size=0)
  expect_true(is.matrix(get_avgroc_curve(c(0,3,4), 3, 2)))
})
