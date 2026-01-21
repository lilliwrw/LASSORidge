test_that("lambda_sequence returns correct length", {
  set.seed(1)
  X <- matrix(rnorm(20*5), nrow=20)
  y <- rnorm(20)
  std <- standardize_data(X, y)
  seq <- lambda_sequence(std$X, std$y)
  expect_length(seq, 100)
  expect_true(all(diff(seq) < 0)) # absteigend
})

test_that("lambda_sequence respects lambda_min_ratio", {
  set.seed(1)
  X <- matrix(rnorm(20*5), nrow=20)
  y <- rnorm(20)
  std <- standardize_data(X, y)
  seq <- lambda_sequence(std$X, std$y, n_lambda = 10, lambda_min_ratio = 0.05)
  expect_equal(seq[10], max(seq)*0.05, tolerance = 1e-10)
})

test_that("lambda_sequence errors for invalid input", {
  X <- matrix(rnorm(10*3), nrow=10)
  y <- rnorm(9)
  expect_error(lambda_sequence(X, y))
})
