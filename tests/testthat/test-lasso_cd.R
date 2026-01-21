test_that("lasso_cd basic functionality", {
  set.seed(1)
  X <- matrix(rnorm(20*5), nrow=20)
  y <- rnorm(20)
  std <- standardize_data(X, y)
  fit <- lasso_cd(std$X, std$y, lambda = 0.1)
  expect_type(fit, "list")
  expect_true(all(c("beta", "iterations", "lambda") %in% names(fit)))
  expect_length(fit$beta, ncol(std$X))
  expect_true(fit$iterations <= 1000)
  expect_equal(fit$lambda, 0.1)
})

test_that("lasso_cd returns sparse coefficients for large lambda", {
  set.seed(2)
  X <- matrix(rnorm(20*5), nrow=20)
  y <- rnorm(20)
  std <- standardize_data(X, y)
  fit <- lasso_cd(std$X, std$y, lambda = 10)
  #bei sehr großem lambda sollte die Koeffizienten 0 sein
  expect_true(all(abs(fit$beta) < 1e-6))
})

test_that("lasso_cd errors for negative lambda", {
  X <- matrix(rnorm(10*3), nrow=10)
  y <- rnorm(10)
  std <- standardize_data(X, y)
  expect_error(lasso_cd(std$X, std$y, lambda = -1))
})
