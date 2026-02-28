test_that("print.lasso_model works without errors", {
  set.seed(123)
  X <- matrix(rnorm(30*4), 30, 4)
  y <- rnorm(30)

  #lasso_model erstellen
  beta_mat <- matrix(runif(4*5), nrow=4, ncol=5)
  lambda_seq <- seq(0.1, 1, length.out=5)
  lasso_fit <- structure(
    list(beta = beta_mat, lambda_seq = lambda_seq),
    class = "lasso_model"
  )

  # Prüfen, dass print() läuft und etwas ausgibt
  expect_output(print(lasso_fit))
})

test_that("print.lasso_model shows correct dimensions", {
  set.seed(42)
  X <- matrix(rnorm(20*3), 20, 3)
  y <- rnorm(20)
  beta_mat <- matrix(runif(3*4), 3, 4)
  lambda_seq <- seq(0.05, 0.5, length.out=4)
  lasso_fit <- structure(
    list(beta = beta_mat, lambda_seq = lambda_seq),
    class = "lasso_model"
  )

  output <- capture.output(print(lasso_fit))
  expect_true(any(grepl("Number of predictors: 3", output)))
  expect_true(any(grepl("Number of lambda values: 4", output)))
})
