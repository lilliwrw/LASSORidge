test_that("ridge returns correct structure", {
  set.seed(1)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)

  fit <- ridge(X, y, lambda = 1)

  expect_s3_class(fit, "ridge")

  expect_true("coefficients" %in% names(fit))
  expect_true("(Intercept)" %in% names(fit$coefficients))
  expect_true("fitted.values" %in% names(fit))
  expect_true("residuals" %in% names(fit))
})

test_that("lambda = 0 approximates OLS", {
  set.seed(1)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)

  fit_ridge <- ridge(X, y, lambda = 0)

  fit_lm <- lm(y ~ X)

  expect_equal(fit_ridge$coefficients, coef(fit_lm), tolerance = 1e-6)
})

test_that("lambda = 0 without standardization equals OLS without intercept", {
  set.seed(1)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)

  fit_ridge <- ridge(X, y, lambda = 0, standardize = FALSE)

  fit_lm <- lm(y ~ X - 1)

  expect_equal(fit_ridge$coefficients[-1], coef(fit_lm), tolerance = 1e-6)
})

test_that("large lambda shrinks coefficients", {
  set.seed(1)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)

  fit_small <- ridge(X, y, lambda = 0.01)
  fit_large <- ridge(X, y, lambda = 1000)

  beta_small <- fit_small$coefficients[-1]
  beta_large <- fit_large$coefficients[-1]

  expect_true(sum(beta_large^2) < sum(beta_small^2))
})

test_that("ridge works without standardization", {
  set.seed(1)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)

  fit <- ridge(X, y, lambda = 1, standardize = FALSE)

  expect_s3_class(fit, "ridge")
})
