test_that("standardization centers and scales X", {
  set.seed(1)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)

  std <- ridge_standardizeData(X, y)

  expect_equal(colMeans(std$Xs), rep(0, 5), tolerance = 1e-8)
  expect_equal(apply(std$Xs, 2, sd), rep(1, 5), tolerance = 1e-8)
})

test_that("standardization centers y", {
  set.seed(1)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)

  std <- ridge_standardizeData(X, y)

  expect_equal(mean(std$ys), 0, tolerance = 1e-8)
})
