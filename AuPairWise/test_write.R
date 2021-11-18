library(testthat)

test_that("Testing write_out_summary function", {
  expect_output(write_out_summary("/test", list(0,1), 3, stoich.pairs, c(0,1,2), 3))
})
