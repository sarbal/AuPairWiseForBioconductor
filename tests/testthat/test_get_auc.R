library(testthat)

test_that("Testing get_auc function", {
  expect_error(get_auc(1,a))
  expect_error(get_auc(a,a))
  expect_vector(get_auc(1.2, 2.2), size=1, ptype=double())
  expect_vector(get_auc(1000, 2000), size=1, ptype=double())
  expect_vector(get_auc(0, -2), size=1, ptype=double())
  expect_true(is.vector(get_auc(1,2)))
})
