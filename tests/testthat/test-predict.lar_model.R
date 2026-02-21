test_that("predict.lar_model returns right dimensions", {
  set.seed(1)
  X <- matrix(rnorm(30 * 5), 30, 5)
  y <- rnorm(30)
  fit <- lar(X, y)

  #alle Schritte
  preds_all <- predict(fit, X)
  expect_true(is.matrix(preds_all))
  expect_equal(dim(preds_all), c(nrow(X), ncol(fit$beta)))

  #einzelner Schritt
  preds_step3 <- predict(fit, X, step = 3)
  expect_true(is.numeric(preds_step3))
  expect_equal(length(preds_step3), nrow(X))
})

test_that("predict.lar_model errors on wrong newx columns", {
  set.seed(1)
  X <- matrix(rnorm(30 * 5), 30, 5)
  y <- rnorm(30)
  fit <- lar(X, y)

  newx_wrong <- matrix(rnorm(30 * 4), 30, 4)
  expect_error(predict(fit, newx_wrong))
})

test_that("predict.lar_model errors with invalid step", {
  set.seed(1)
  X <- matrix(rnorm(30 * 5), 30, 5)
  y <- rnorm(30)
  fit <- lar(X, y)

  expect_error(predict(fit, X, step = 0))
  expect_error(predict(fit, X, step = 100))
})
