test_that("core estimator equals OLS when lamda = 0", {
  set.seed(1)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)

  beta_ridge <- ridge_core(X, y, lambda = 0)
  beta_ols <- as.vector(solve(crossprod(X), crossprod(X, y)))

  expect_equal(beta_ridge, beta_ols, tolerance = 1e-8)
})

test_that("core estimator returns correct dimension", {
  X <- matrix(rnorm(50), 10, 5)
  y <- rnorm(10)

  beta <- ridge_core(X, y, 1)

  expect_length(beta, 5)
})

test_that("ridge core estimator shrinks coefficients for lambda > 0", {
  set.seed(1)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)

  beta0 <- ridge_core(X, y, 0)
  beta1 <- ridge_core(X, y, 10)

  expect_true(sum(beta1^2) < sum(beta0^2))
})
