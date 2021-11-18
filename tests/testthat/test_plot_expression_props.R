library(testthat)

test_that("Testing plot_expression_props function", {
  out = paste(getwd(),"/Output/results",sep="")
  expect_output_file(plot_expression_props(out, 0, 0, c()), paste( out, ".exprs.png", sep=""))
})
