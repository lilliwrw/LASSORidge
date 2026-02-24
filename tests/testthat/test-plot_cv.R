test_that("plot_cv works", {
  set.seed(123)
  n <- 100; p <- 4
  X <- matrix(rnorm(n*p), nrow=n, ncol=p)
  y <- rnorm(n)
  cv_result <- lambda_cv(X, y, method = "lasso")
  expect_silent(
    plot_cv(cv_result$lambda_seq,
            cv_result$cv_values,
            cv_result$lambda_opt))
})
test_that("plot_cv fails for wrong inputs", {
  expect_error(plot_cv("a", c(1, 2), 1), "must be numeric")
  expect_error(plot_cv(c(1, 2), c(1), 1), "same length")
  expect_error(plot_cv(c(1, NA), c(1, 2), 1), "must not contain NA")
  expect_error(plot_cv(c(-1, 1), c(1, 2), 1, log_scale = TRUE), "must be > 0")
})
