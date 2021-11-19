library(testthat)

test_that("Testing all_pairs function", {
  data("samplePairs")
  expect_vector(all_pairs(samplePairs), size=length(samplePairs$V1))
  expect_vector(all_pairs(samplePairs), size=length(samplePairs$V2))
  expect_true(is.matrix(all_pairs(samplePairs)))
})
