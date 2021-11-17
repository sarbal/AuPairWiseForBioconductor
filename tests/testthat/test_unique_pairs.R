library(testthat)

test_that("Testing unique_all_pairs function", {
  expect_equal(length(unique_all_pairs(stoich.pairs)), length(stoich.pairs$V1) + length(stoich.pairs$V2))
  expect_equal(length(unique_all_pairs(ppin.pairs)), length(ppin.pairs$V1) + length(ppin.pairs$V2))
  expect_true(is.matrix(unique_all_pairs(stoich.pairs)))
})
