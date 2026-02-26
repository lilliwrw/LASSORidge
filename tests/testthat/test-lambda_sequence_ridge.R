test_that("lambda_sequence_ridge returns correct length", {
  set.seed(1)
  X <- matrix(rnorm(20*5), nrow=20)
  y <- rnorm(20)
  std <- standardize_data(X, y)
  seq <- lambda_sequence_ridge(std$X)
  expect_length(seq, 100)
  expect_true(all(seq >= 0))
  expect_type(seq, "double")
  expect_true(all(diff(seq) < 0)) #descending
})

test_that("lambda_sequence_ridge respects lambda_min_ratio", {
  set.seed(1)
  X <- matrix(rnorm(20*5), nrow=20)
  y <- rnorm(20)
  std <- standardize_data(X, y)
  seq <- lambda_sequence_ridge(std$X, n_lambda = 10, lambda_min_ratio = 0.05)
  expect_equal(seq[10], max(seq)*0.05, tolerance = 1e-10)
})

test_that("lambda_sequence_ridge fails for invalid input", {
  X <- rnorm(9)
  expect_error(lambda_sequence(X),
               "X must be a matrix")
})

test_that("lambda_sequence_ridge handles near-zero X robustly", {
  X0 <- matrix(0, nrow = 20, ncol = 5)
  lam <- lambda_sequence_ridge(X0, n_lambda = 5)

  expect_length(lam, 5)
  expect_true(all(lam > 0))
  expect_true(all(diff(lam) <= 0))
  expect_true(min(lam) >= 1e-4)
})

test_that("lambda_sequence_ridge works for n_lambda = 1 (marginal case)", {
  set.seed(1)
  X <- matrix(rnorm(30), 10, 3)
  lam <- lambda_sequence_ridge(X, n_lambda = 1)
  expect_length(lam, 1)
  expect_true(lam > 0)
})
