test_that("lambda_cv returns correct structure for lasso", {
  set.seed(1)
  n <- 60; p <- 8
  X <- matrix(rnorm(n * p), n, p)
  beta_true <- c(2, -1.5, rep(0, p - 2))
  y <- as.numeric(X %*% beta_true + rnorm(n, sd = 0.5))

  res <- lambda_cv(X, y, M = 5, method = "lasso")

  expect_true(is.list(res))
  expect_true(all(c("lambda_opt", "cv_values", "lambda_seq") %in% names(res)))
  expect_true(is.numeric(res$lambda_seq))
  expect_true(is.numeric(res$cv_values))
  expect_true(is.numeric(res$lambda_opt))
  expect_length(res$cv_values, length(res$lambda_seq))

  # lambda_opt has to be the minimum of the cv_values
  expect_equal(res$lambda_opt, res$lambda_seq[which.min(res$cv_values)])

  # lambda has to be >= 0
  expect_true(all(res$lambda_seq >= 0))
})

test_that("lambda_cv returns correct structure for ridge", {
  set.seed(2)
  n <- 60; p <- 8
  X <- matrix(rnorm(n * p), n, p)
  beta_true <- c(2, -1.5, rep(0, p - 2))
  y <- as.numeric(X %*% beta_true + rnorm(n, sd = 0.5))

  res <- lambda_cv(X, y, M = 5, method = "ridge")

  expect_true(is.list(res))
  expect_true(all(c("lambda_opt", "cv_values", "lambda_seq") %in% names(res)))
  expect_length(res$cv_values, length(res$lambda_seq))
  expect_equal(res$lambda_opt, res$lambda_seq[which.min(res$cv_values)])
  expect_true(all(res$lambda_seq >= 0))
})

test_that("lambda_cv fails for invalid inputs", {
  set.seed(1)
  n <- 20
  p <- 3
  X <- matrix(rnorm(n * p), n, p)
  y <- rnorm(n)

  for (method in c("lasso", "ridge")) {

    # X as data frame could be converted to a matrix
    expect_silent(lambda_cv(as.data.frame(X), y, M = 5, method = method))

    # X must be numeric
    X_chr <- matrix(as.character(seq_len(n * p)), n, p)
    expect_error(
      lambda_cv(X_chr, y, M = 5, method = method),
      "X must be numeric",
      fixed = TRUE,
      info = method)

    # NA in X
    X_na <- X
    X_na[1, 1] <- NA_real_
    expect_error(
      lambda_cv(X_na, y, M = 5, method = method),
      "NA in Data not allowed",
      fixed = TRUE,
      info = method
    )

    # length of y
    expect_error(
      lambda_cv(X, y[-1], M = 5, method = method),
      'Length of y must match number of rows of X',
      fixed = TRUE,
      info = method)

    # M <= n
    expect_error(
      lambda_cv(X, y, M = n + 1, method = method),
      "M must be smaller or equal (leave-one-out cross validation) to nrow(X)",
      fixed = TRUE,
      info = method)

    # M positive
    expect_error(
      lambda_cv(X, y, M = 0, method = method),
      "M must be a positive number.",
      fixed = TRUE,
      info = method)

    expect_error(
      lambda_cv(X, y, M = 2.5, method = method),
      "M must be a positive number.",
      fixed = TRUE,
      info = method)
  }

    # method
    expect_error(
      lambda_cv(X, y, M = 5, method = "hi"),
      "method must bei lasso or ridge",
      fixed = TRUE,
      info = method)

})
