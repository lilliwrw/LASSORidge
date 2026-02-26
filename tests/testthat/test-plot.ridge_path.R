test_that("plot.ridge_path runs without error (default)", {
  set.seed(1)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)
  lambda_seq <- exp(seq(-2, 2, length.out = 10))

  path <- ridge_path(X, y, lambda_seq)

  expect_no_error(plot(path))
})

test_that("plot.ridge_path returns invisible NULL", {
  set.seed(1)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)
  lambda_seq <- exp(seq(-2, 2, length.out = 5))

  path <- ridge_path(X, y, lambda_seq)

  result <- plot(path)
  expect_null(result)
})

test_that("plot.ridge_path accepts custom graphical arguments", {
  set.seed(1)
  X <- matrix(rnorm(60), 20, 3)
  y <- rnorm(20)
  lambda_seq <- exp(seq(-1, 1, length.out = 5))

  path <- ridge_path(X, y, lambda_seq)

  expect_no_error(
    plot(path,
         col = rainbow(3),
         lwd = 2,
         main = "Custom Title",
         xlab = "Regularization Strength",
         ylab = "Beta")
  )
})

test_that("plot.ridge_path works without log scale", {
  set.seed(1)
  X <- matrix(rnorm(80), 20, 4)
  y <- rnorm(20)
  lambda_seq <- exp(seq(-2, 2, length.out = 6))

  path <- ridge_path(X, y, lambda_seq)

  expect_no_error(plot(path, log.lambda = FALSE))
})
