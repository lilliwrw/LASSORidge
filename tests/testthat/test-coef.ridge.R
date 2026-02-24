test_that("coef.ridge returns correct coefficients", {
  set.seed(1)
  X <- matrix(rnorm(50), 10, 5)
  y <- rnorm(10)

  fit <- ridge(X, y, lambda = 1)

  expect_equal(coef(fit), fit$coefficients)
  expect_true("(Intercept)" %in% names(coef(fit)))
})
