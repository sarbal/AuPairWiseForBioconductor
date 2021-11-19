library(testthat)

test_that("Testing get_value_x function", {
  expect_equal(get_value_x(1,2,3,4,5), 3)
  expect_equal(get_value_x(5,4,3,2,1), 3)
})
