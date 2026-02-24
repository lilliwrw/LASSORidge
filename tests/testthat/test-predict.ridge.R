test_that("predict.ridge returns fitted values when newdata is NULL", {
  set.seed(1)
  X <- matrix(rnorm(50), 10, 5)
  y <- rnorm(10)

  fit <- ridge(X, y, lambda = 1)

  expect_equal(predict(fit), fitted(fit))
})

test_that("predict.ridge works for new data", {
  set.seed(1)
  X <- matrix(rnorm(50), 10, 5)
  y <- rnorm(10)

  fit <- ridge(X, y, lambda = 1)

  X_new <- matrix(rnorm(25), 5, 5)

  preds <- predict(fit, X_new)

  expect_equal(length(preds), 5)
})
