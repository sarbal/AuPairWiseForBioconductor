library(testthat)

test_that("Testing roc_score function", {
  expect_vector(roc_score(c(0), c(0)), size=1)
  expect_vector(roc_score(c(1,34), c(0,12)), size=1, ptype=double())
  expect_vector(roc_score(c(1,34), c(0,-12,1,3,4)), size=1, ptype=double())
  expect_vector(roc_score(c(1,2,3,4), c(0)), size=1, ptype=double())
  expect_vector(roc_score(121, c(0)), size=1, ptype=double())
  expect_vector(roc_score(121, 132), size=1, ptype=double())
  expect_vector(roc_score(c(1,2,1), 132), size=1, ptype=double())
})
