library(testthat)

test_that("Testing get_roc_curve function", {
  expect_vector(get_roc_curve(c(0), c(0)), size=2)
  expect_vector(get_roc_curve(c(1,2,3), c(1,2,3)), size=4)
  expect_vector(get_roc_curve(c(1,2,3,4,5,6), c(0)), size=7)
  expect_true(is.matrix(get_roc_curve(c(1),c(1))))
})
