test_that("inverse transform recovers coefficients correctly", {
  beta_scaled <- c(2, 4)
  X_means <- c(1, 1)
  X_sds <- c(2, 2)
  y_mean <- 10

  res <- ridge_inverseTransform(beta_scaled, X_means, X_sds, y_mean)

  expect_equal(res$beta, c(1, 2))
})
