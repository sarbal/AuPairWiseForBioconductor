library(testthat)

test_that("Testing predict_sample works", {
  data("samplePairs")
  data("sampleBrainspanExpressionSet")
  X <- sampleBrainspanExpressionSet@assayData[["exprs"]]
  genes.list = rownames(X)

  pairs = list()
  pairs$stoich = all_pairs(samplePairs)
  pairs$all    = unique_all_pairs( pairs )
  pairs$labels = labels.default
  length       = length(pairs$labels)
  indices = get_indices_stoich_pairs(pairs$all, genes.list)
  k = cbind( indices$x1, indices$x2)
  indices.stoich = get_indices_stoich_pairs(pairs$stoich, genes.list)
  filter = filter_pairs(pairs,indices,length)

  nK = length(indices$x1)
  nS = dim(X)[2]
  n.factor = 1
  s = sample(nS, 2, replace=T)

  expect_vector(predict_sample(X, s[1], n.factor, k, nS, nK, filter))
})
