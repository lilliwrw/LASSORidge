test_that("plot.lar_model works", {
  set.seed(123)
  X <- matrix(rnorm(50 * 5), 50, 5)
  y <- rnorm(50)

  fit <- lar(X, y)

  expect_silent(plot(fit))
})
