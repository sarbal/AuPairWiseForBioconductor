library(testthat)

test_that("Testing get_approx_expression function", {
  expect_silent(get_approx_expression(0,0,0,0,0))
  expect_true(is.atomic(get_approx_expression(0,0,0,0,0)))
  expect_true(is.double(get_approx_expression(0,0,1,1,1)))

})

test_that("get_approx_expression works", {
  X <- ExpressionSet(assayData=exprs)@assayData[["exprs"]]

  nX1 = dim(X)[1]
  nS1 = dim(X)[2]
  X.r = t(apply ( X,1,rank, ties.method="random", na.last="keep") )

  i = 1 # arbitrary numbers for testing
  s = 272
  n.factor = 1

  noise = runif(nX1, min=-n.factor/100, max=n.factor/100)
  X.s = X.r[,s] + nS1*noise

  result = get_approx_expression(X[i,], X.r[i,], X.s[i], nS1, 1)
  testthat::expect_equal(typeof(result), "double")
  testthat::expect_error(get_approx_expression(NULL, X.r[i,], X.s[i], nS1, 1), "temp.x type is not double")
})
