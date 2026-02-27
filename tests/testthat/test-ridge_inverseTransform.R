test_that("inverse transform recovers coefficients correctly", {
  beta_scaled <- c(2, 4)
  X_means <- c(1, 1)
  X_sds <- c(2, 2)
  y_mean <- 10

  res <- ridge_inverseTransform(beta_scaled, X_means, X_sds, y_mean)

  expect_equal(res$beta, c(1, 2))
})

test_that("inverse transform computes correct intercept", {
  beta_scaled <- c(2, 4)
  X_means <- c(1, 1)
  X_sds <- c(2, 2)
  y_mean <- 10

  res <- ridge_inverseTransform(beta_scaled, X_means, X_sds, y_mean)

  # beta = (1, 2)
  # intercept = 10 - (1*1 + 2*1) = 7
  expect_equal(res$intercept, 7)
})

test_that("standardize + inverse transform recovers OLS solution", {
  set.seed(1)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)

  std <- ridge_standardizeData(X, y)

  beta_scaled <- ridge_core(std$Xs, std$ys, lambda = 0)

  res <- ridge_inverseTransform(
    beta_scaled,
    std$X_means,
    std$X_sds,
    std$y_mean
  )

  X_ext <- cbind(1, X)
  beta_full <- solve(crossprod(X_ext), crossprod(X_ext, y))

  beta_ols <- beta_full[-1]
  intercept_ols <- beta_full[1]

  expect_equal(res$beta, beta_ols, tolerance = 1e-8)
  expect_equal(res$intercept, intercept_ols, tolerance = 1e-8)
})
