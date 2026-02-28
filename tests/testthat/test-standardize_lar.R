test_that("standardize_lar centers and scales data correctly", {
  set.seed(1)
  X <- matrix(rnorm(12), 4, 3)
  y <- rnorm(4)

  std <- standardize_lar(X, y)

  # Check dimensions
  expect_equal(dim(std$X), dim(X))
  expect_equal(length(std$y), length(y))

  # Check that X is centered
  expect_true(all(abs(colMeans(std$X)) < 1e-12))
  # Check that y is centered
  expect_true(abs(mean(std$y)) < 1e-12)
  # Check that original means/scales are returned
  expect_equal(std$X_means, colMeans(X))
  expect_equal(std$y_mean, mean(y))
})

test_that("constant columns are handled correctly", {
  X <- matrix(c(1,1,1,1, 2,3,4,5), 4, 2)
  y <- c(1,2,3,4)
  std <- standardize_lar(X, y)
  expect_equal(std$X_scales[1], 1) # constant column sd = 1
  expect_equal(colMeans(std$X)[1], 0)
})
