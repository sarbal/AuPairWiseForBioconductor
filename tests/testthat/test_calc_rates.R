library(testthat)

test_that("Testing calc_rates function", {
  expect_vector(calc_rates(1,2,3), size=1)
  expect_vector(calc_rates(c(1,2),c(1,2),3), size=1)
  expect_vector(calc_rates(c(1,2),c(1,2),c(2,3)), size=1)
  expect_error(calc_rates(c(1,a),c(1,2),c(2,3)))
})
