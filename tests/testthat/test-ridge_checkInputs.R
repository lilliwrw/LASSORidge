test_that("checks work for valid input", {
  set.seed(1)
  X <- matrix(rnorm(20), 10, 2)
  y <- rnorm(10)

  expect_true(ridge_checkInputs(X, y, 1))
})

test_that("X must be numeric", {
  X <- matrix(letters[1:10], 5, 2)
  y <- rnorm(5)

  expect_error(ridge_checkInputs(X, y, 1))
})

test_that("X must not contain NA or Inf", {
  X <- matrix(rnorm(20), 10, 2)
  y <- rnorm(10)

  X[1,1] <- NA
  expect_error(ridge_checkInputs(X, y, 1))

  X[1,1] <- Inf
  expect_error(ridge_checkInputs(X, y, 1))
})

test_that("X must have positive dimensions", {
  X <- matrix(numeric(0), 0, 0)
  y <- numeric(0)

  expect_error(ridge_checkInputs(X, y, 1))
})

test_that("y must be numeric", {
  X <- matrix(rnorm(20), 10, 2)
  y <- letters[1:10]

  expect_error(ridge_checkInputs(X, y, 1))
})

test_that("y must not contain NA or Inf", {
  X <- matrix(rnorm(20), 10, 2)
  y <- rnorm(10)

  y[1] <- NA
  expect_error(ridge_checkInputs(X, y, 1))

  y[1] <- Inf
  expect_error(ridge_checkInputs(X, y, 1))
})

test_that("X and y dimensions must match", {
  X <- matrix(rnorm(20), 10, 2)
  y <- rnorm(5)

  expect_error(ridge_checkInputs(X, y, 1))
})

test_that("lambda must be single numeric value", {
  X <- matrix(rnorm(20), 10, 2)
  y <- rnorm(10)

  expect_error(ridge_checkInputs(X, y, c(1,2)))
})

test_that("lambda must be finite and non-negative", {
  X <- matrix(rnorm(20), 10, 2)
  y <- rnorm(10)

  expect_error(ridge_checkInputs(X, y, -1))
  expect_error(ridge_checkInputs(X, y, Inf))
  expect_error(ridge_checkInputs(X, y, NaN))
})
