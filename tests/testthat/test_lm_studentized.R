library(testthat)

test_that("Testing lm.studentized function", {
  expect_silent(lm.studentized(1,1))
})
