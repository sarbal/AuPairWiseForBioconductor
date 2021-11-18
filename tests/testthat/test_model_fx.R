library(testthat)

test_that("Testing model_fx function", {
  expect_silent(model.fx(1, sum))
})
