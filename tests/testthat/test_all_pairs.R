library(testthat)

test_that("Testing all_pairs function", {
  expect_vector(all_pairs(stoich.pairs), size=length(stoich.pairs$V1))
  expect_vector(all_pairs(stoich.pairs), size=length(stoich.pairs$V2))
  expect_vector(all_pairs(ppin.pairs), size=length(ppin.pairs$V1))
  expect_vector(all_pairs(ppin.pairs), size=length(ppin.pairs$V2))
  expect_true(is.matrix(all_pairs(stoich.pairs)))
})
