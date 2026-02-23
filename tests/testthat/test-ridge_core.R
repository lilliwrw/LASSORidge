test_that("core estimator equals OLS when lamda = 0", {
  set.seed(1)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)

  beta_ridge <- ridge_core(X, y, lambda = 0)
  beta_ols <- solve(t(X) %*% X, t(X) %*% y)

  expect_equal(beta_ridge, beta_ols, tolerance = 1e-8)
})
