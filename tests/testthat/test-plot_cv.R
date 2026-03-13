test_that("plot_cv works for valid input", {
  set.seed(123)
  n <- 100
  p <- 4
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y <- rnorm(n)

  cv_result <- lambda_cv(X, y, method = "lasso")

  expect_silent(
    plot_cv(
      cv_result$lambda_seq,
      cv_result$cv_values,
      cv_result$lambda_opt
    )
  )
})

test_that("plot_cv validates inputs correctly", {
  expect_error(
    plot_cv("a", c(1, 2), 1),
    "must be numeric",
    fixed = TRUE
  )

  expect_error(
    plot_cv(c(1, 2), c(1), 1),
    "same length",
    fixed = TRUE
  )

  expect_error(
    plot_cv(c(1, NA), c(1, 2), 1),
    "must not contain NA",
    fixed = TRUE
  )

  expect_error(
    plot_cv(c(1, 2), c(1, 2), c(1, 2)),
    "single numeric value",
    fixed = TRUE
  )

  expect_error(
    plot_cv(c(-1, 1), c(1, 2), 1, log_scale = TRUE),
    "must be > 0",
    fixed = TRUE
  )
})
