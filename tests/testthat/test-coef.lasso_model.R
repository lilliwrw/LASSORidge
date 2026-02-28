test_that("coef() for lasso_model extracts correct column", {
  set.seed(1)
  X <- matrix(rnorm(20*5), 20, 5)
  y <- rnorm(20)

  fit <- lasso(X, y, n_lambda = 4)

  coefs_last <- coef(fit)
  expect_equal(dim(coefs_last), c(5, 1))

  coefs_second <- coef(fit, lambda_index = 2)
  expect_equal(dim(coefs_second), c(5, 1))

  expect_true(is.numeric(coefs_last))
})
