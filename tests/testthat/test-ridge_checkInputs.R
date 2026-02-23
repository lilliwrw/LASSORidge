test_that("checks work for valid input", {
  set.seed(1)
  X <- matrix(rnorm(20), 10, 2)
  y <- rnorm(10)

  expect_true(ridge_checkInputs(X, y, 1))
})

test_that("invalid checks throw errors", {
  set.seed(1)
  X <- matrix(rnorm(20), 10, 2)
  y <- rnorm(5)

  expect_error(ridge_checkInputs(X, y, 1))
  expect_error(ridge_checkInputs(X, rnorm(10), -1))
})
