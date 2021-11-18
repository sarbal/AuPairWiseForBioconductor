library(testthat)

test_that("Testing get_approx_expression function", {
  expect_silent(get_approx_expression(0,0,0,0,0))
  expect_true(is.atomic(get_approx_expression(0,0,0,0,0)))
  expect_true(is.double(get_approx_expression(0,0,1,1,1)))
})
