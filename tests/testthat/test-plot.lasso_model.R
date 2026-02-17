test_that("plot.lasso_model runs without error for multiple lambdas", {
  set.seed(1)
  X <- matrix(rnorm(50*5), 50, 5)
  y <- rnorm(50)

  fit <- lasso_fit(X, y, n_lambda = 5)

  expect_silent(plot(fit))
})

test_that("plot.lasso_model runs without error for n_lambda = 1", {
  set.seed(1)
  X <- matrix(rnorm(20*5), 20, 5)
  y <- rnorm(20)

  fit <- lasso_fit(X, y, n_lambda = 1)

  expect_silent(plot(fit))
})

test_that("plot.lasso_model fails for wrong input class", {
  not_fit <- list(beta = matrix(1, 5, 5), lambda_seq = 1:5)
  #keine lasso_model Klasse
  expect_error(plot.lasso_model(not_fit), "x must be a lasso_model object")
})
