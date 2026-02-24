test_that("fitted.ridge returns correct fitted values", {
  set.seed(1)
  X <- matrix(rnorm(50), 10, 5)
  y <- rnorm(10)

  fit <- ridge(X, y, lambda = 1)

  expect_equal(fitted(fit), fit$fitted.values)
})
