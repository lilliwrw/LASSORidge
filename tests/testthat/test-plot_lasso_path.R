test_that("plot_lasso_path runs without error", {
  set.seed(1)
  X <- matrix(rnorm(50*5), nrow=50)
  y <- rnorm(50)
  std <- standardize_data(X, y)
  path <- lasso_path(std$X, std$y, n_lambda=10)

  expect_silent(plot_lasso_path(path$beta, path$lambda_seq))
})
