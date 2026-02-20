test_that("lasso runs and returns correct structure", {
  set.seed(1)
  X <- matrix(rnorm(20*5), 20, 5)
  y <- rnorm(20)

  fit <- lasso(X, y, n_lambda = 5)

  expect_s3_class(fit, "lasso_model")
  expect_true(is.matrix(fit$beta))
  expect_equal(nrow(fit$beta), ncol(X))
  expect_equal(ncol(fit$beta), 5)
  expect_true(is.numeric(fit$lambda_seq))
  expect_equal(length(fit$lambda_seq), 5)
})

test_that("lasso works with standardize = FALSE", {
  set.seed(1)
  X <- matrix(rnorm(20*5), 20, 5)
  y <- rnorm(20)

  fit <- lasso(X, y, n_lambda = 3, standardize = FALSE)

  expect_s3_class(fit, "lasso_model")
  expect_equal(ncol(fit$beta), 3)
})
