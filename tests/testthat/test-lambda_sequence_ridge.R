test_that("lambda_sequence_ridge returns correct length and order", {
  set.seed(1)
  X <- matrix(rnorm(20 * 5), nrow = 20)
  y <- rnorm(20)

  std <- ridge_standardizeData(X, y)
  lam <- lambda_sequence_ridge(std$Xs)

  expect_length(lam, 100)
  expect_type(lam, "double")
  expect_true(all(lam >= 0))
  expect_true(all(diff(lam) < 0))
})

test_that("lambda_sequence_ridge respects lambda_min_ratio", {
  set.seed(1)
  X <- matrix(rnorm(20 * 5), nrow = 20)
  y <- rnorm(20)

  std <- ridge_standardizeData(X, y)
  lam <- lambda_sequence_ridge(std$Xs, n_lambda = 10, lambda_min_ratio = 0.05)

  expect_equal(min(lam), max(lam) * 0.05, tolerance = 1e-8)
})

test_that("lambda_sequence_ridge fails for invalid X input", {
  X <- rnorm(9)

  expect_error(
    lambda_sequence_ridge(X),
    "X must be a matrix",
    fixed = TRUE
  )
})

test_that("lambda_sequence_ridge handles near-zero X robustly", {
  X0 <- matrix(0, nrow = 20, ncol = 5)
  lam <- lambda_sequence_ridge(X0, n_lambda = 5)

  expect_length(lam, 5)
  expect_true(all(lam > 0))
  expect_true(all(diff(lam) <= 0))
  expect_true(min(lam) >= 1e-4)
})

test_that("lambda_sequence_ridge works for n_lambda = 1", {
  set.seed(1)
  X <- matrix(rnorm(30), 10, 3)

  lam <- lambda_sequence_ridge(X, n_lambda = 1)

  expect_length(lam, 1)
  expect_true(lam > 0)
})

test_that("lambda_sequence_ridge validates additional inputs", {
  X <- matrix(rnorm(20), 10, 2)

  expect_error(
    lambda_sequence_ridge(matrix(as.character(1:20), 10, 2)),
    "X must be numeric.",
    fixed = TRUE
  )

  X_na <- X
  X_na[1, 1] <- NA_real_
  expect_error(
    lambda_sequence_ridge(X_na),
    "X must not contain missing values.",
    fixed = TRUE
  )

  expect_error(
    lambda_sequence_ridge(X, n_lambda = 0),
    "n_lambda must be a positive integer.",
    fixed = TRUE
  )

  expect_error(
    lambda_sequence_ridge(X, n_lambda = 2.5),
    "n_lambda must be a positive integer.",
    fixed = TRUE
  )

  expect_error(
    lambda_sequence_ridge(X, lambda_min_ratio = 0),
    "lambda_min_ratio must be a positive number.",
    fixed = TRUE
  )
})
