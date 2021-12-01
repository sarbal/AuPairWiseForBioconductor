library(testthat)

test_that("Testing unique_all_pairs function", {
  data("samplePairs")

  expect_equal(length(unique_all_pairs(samplePairs)), length(samplePairs$V1) + length(samplePairs$V2))
  expect_true(is.matrix(unique_all_pairs(samplePairs)))
})
