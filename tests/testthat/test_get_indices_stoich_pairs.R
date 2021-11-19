library(testthat)

test_that("Testing get_indices_stoich_pairs function", {
  data("samplePairs")
  data("sampleBrainspanExpressionSet")
  X <- sampleBrainspanExpressionSet@assayData[["exprs"]]
  genes.list = rownames(X)

  expect_vector(get_indices_stoich_pairs(samplePairs[0,], genes.list)$p1, ptype = logical())
  expect_vector(get_indices_stoich_pairs(samplePairs, genes.list)$p2, ptype = logical())
  expect_vector(get_indices_stoich_pairs(samplePairs, genes.list)$x1, ptype = integer())
  expect_vector(get_indices_stoich_pairs(samplePairs, genes.list)$x2, ptype = integer())
  expect_vector(get_indices_stoich_pairs(samplePairs, genes.list), size = 4)
  expect_vector(get_indices_stoich_pairs(samplePairs[2,], genes.list[]), size = 4)
  expect_error(get_indices_stoich_pairs())
  expect_error(get_indices_stoich_pairs(samplePairs, genes.list[1,]))
  expect_vector(get_indices_stoich_pairs({}, {})$p1, ptype = logical(), size = 0)
  expect_vector(get_indices_stoich_pairs({}, {})$p2, ptype = logical(), size = 0)
  expect_vector(get_indices_stoich_pairs({}, {})$x1, ptype = integer(), size = 0)
  expect_vector(get_indices_stoich_pairs({}, {})$x2, ptype = integer(), size = 0)
})
