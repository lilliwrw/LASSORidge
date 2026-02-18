test_that("coef_lasso extracts correct column", {
  set.seed(1)
  X <- matrix(rnorm(20*5), 20, 5)
  y <- rnorm(20)

  fit <- lasso_fit(X, y, n_lambda = 4)

  coefs_last <- coef_lasso(fit)
  expect_equal(dim(coefs_last), c(5, 1))

  coefs_second <- coef_lasso(fit, lambda_index = 2)
  expect_equal(dim(coefs_second), c(5, 1))

  expect_true(is.numeric(coefs_last))
})
