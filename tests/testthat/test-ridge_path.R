test_that("ridge_path returns correct structure", {
  set.seed(1)
  X <- matrix(rnorm(50), 10, 5)
  y <- rnorm(10)

  path <- ridge_path(X, y, lambda = c(0.1, 1, 10))

  expect_s3_class(path, "ridge_path")
  expect_equal(length(path$lambda), 3)
  expect_true(is.matrix(path$coefficients))
  expect_equal(ncol(path$coefficients), 3)
})
