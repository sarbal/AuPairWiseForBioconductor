library(testthat)

test_that("Testing get_value function", {
  expect_equal(get_value(1,2,3,4,5), 7)
  expect_equal(get_value(5,4,3,2,1), -1)
  expect_equal(get_value(0,0,0,0,0), NaN)
})
