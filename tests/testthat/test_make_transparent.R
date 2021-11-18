library(testthat)

test_that("Testing make_transparent function", {
  expect_silent(makeTransparent(1))
})
