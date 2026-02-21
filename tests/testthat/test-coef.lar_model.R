test_that("coef.lar_model returns matrix pxK", {
  set.seed(1)
  X <- matrix(rnorm(30 * 5), 30, 5)
  y <- rnorm(30)
  fit <- lar(X, y)

  coef <- coef(fit)

  expect_true(is.matrix(coef))
  expect_equal(nrow(coef), ncol(X))  # p Reihen
  expect_equal(ncol(coef), ncol(fit$beta))  # K Steps
})

test_that("coef.lar_model returns certain step", {
  set.seed(1)
  X <- matrix(rnorm(30 * 5), 30, 5)
  y <- rnorm(30)
  fit <- lar(X, y)

  step3 <- coef(fit, step = 3)

  expect_true(is.numeric(step3))
  expect_equal(length(step3), ncol(X))
})

test_that("coef.lar_model invalid step errors", {
  set.seed(1)
  X <- matrix(rnorm(30 * 5), 30, 5)
  y <- rnorm(30)
  fit <- lar(X, y)

  expect_error(coef(fit, step = 0))
  expect_error(coef(fit, step = 100))
})

