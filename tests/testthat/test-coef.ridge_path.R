test_that("coef.ridge_path returns full matrix", {
  set.seed(1)
  X <- matrix(rnorm(50), 10, 5)
  y <- rnorm(10)

  path <- ridge_path(X, y, lambda = c(0.1, 1))

  expect_equal(coef(path), path$coefficients)
})

test_that("coef.ridge_path works with lambda value", {
  set.seed(1)
  X <- matrix(rnorm(50), 10, 5)
  y <- rnorm(10)

  path <- ridge_path(X, y, lambda = c(0.1, 1))

  expect_equal(
    coef(path, step = 1),
    path$coefficients[, 1]
  )
})
