library(testthat)

test_that("Testing predict_sample works", {
  X <- ExpressionSet(assayData=exprs)@assayData[["exprs"]]
  nK = length(indices$x1)
  nS = dim(X)[2]
  n.factor = 1
  s = sample(nS, 2, replace=T)

  pairs = list()
  pairs$stoich = all_pairs(stoich.pairs)
  pairs$all    = unique_all_pairs( pairs )
  pairs$labels = labels.default
  length       = length(pairs$labels)
  indices =  get_indices_stoich_pairs(pairs$all, genes.list)
  k = cbind( indices$x1, indices$x2)
  indices.stoich = get_indices_stoich_pairs(pairs$stoich, genes.list)
  filter = filter_pairs(pairs,indices,length)

  expect_vector(predict_sample(X, s[1], n.factor, k, nS, nK, filter))
})
