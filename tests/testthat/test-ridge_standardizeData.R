test_that("standardization returns expected components", {
  X <- matrix(rnorm(20), 10, 2)
  y <- rnorm(10)

  std <- ridge_standardizeData(X, y)

  expect_named(std, c("Xs", "ys", "X_means", "X_sds", "y_mean"))
})

test_that("standardization preserves dimensions", {
  X <- matrix(rnorm(20), 10, 2)
  y <- rnorm(10)

  std <- ridge_standardizeData(X, y)

  expect_equal(dim(std$Xs), dim(X))
  expect_length(std$ys, length(y))
})

test_that("handles zero-variance columns", {
  X <- matrix(rnorm(20), 10, 2)
  X[,1] <- 5
  y <- rnorm(10)

  expect_warning(
    std <- ridge_standardizeData(X, y)
  )

  expect_true(all(is.finite(std$Xs)))
})

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
