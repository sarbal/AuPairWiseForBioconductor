library(testthat)

test_that("Testing plot_cumulative_counts function", {
  out = getwd()
  data("sampleBrainspanExpressionSet")
  X <- sampleBrainspanExpressionSet@assayData[["exprs"]]
  expect_silent(plot_cummulative_counts(out, X))
})
