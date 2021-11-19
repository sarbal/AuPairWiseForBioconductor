library(testthat)

test_that("Testing plot_cumulative_counts function", {
  out = paste(getwd(),"/Output/results",sep="")
  expect_silent(plot_cummulative_counts(out, ExpressionSet(assayData=exprs)@assayData[["exprs"]]))
})
