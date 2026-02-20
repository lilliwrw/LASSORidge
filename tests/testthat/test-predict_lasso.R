test_that("predict_lasso returns correct length", {
  set.seed(1)
  X <- matrix(rnorm(20*5), 20, 5)
  y <- rnorm(20)

  fit <- lasso(X, y, n_lambda = 5)

  X_new <- matrix(rnorm(10*5), 10, 5)
  pred <- predict_lasso(fit, X_new)

  expect_equal(length(pred), nrow(X_new))
  expect_true(is.numeric(pred))

  pred2 <- predict_lasso(fit, X_new, lambda_index = 2)
  expect_equal(length(pred2), nrow(X_new))
})
