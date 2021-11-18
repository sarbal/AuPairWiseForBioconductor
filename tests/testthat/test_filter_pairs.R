library(testthat)

test_that("Testing filter_pairs function", {
  pairs = list()
  pairs$stoich = all_pairs(stoich.pairs)
  pairs$all    = unique_all_pairs( pairs )
  pairs$labels = labels.default
  length       = length(pairs$labels)
  indices =  get_indices_stoich_pairs(pairs$all, genes.list)
  indices.stoich = get_indices_stoich_pairs(pairs$stoich, genes.list)

  expect_vector(filter_pairs(pairs,indices,length))
  expect_silent(filter_pairs(pairs, indices, length))
})
