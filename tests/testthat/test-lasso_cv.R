test_that("cv_lasso basic functionality", {
  set.seed(1)
  X <- matrix(rnorm(50*5), nrow=50)
  y <- rnorm(50)
  std <- standardize_data(X, y)
  cv <- lasso_cv(std$X, std$y, n_folds=5, n_lambda=10)

  expect_true(all(c("lambda_seq","cv_err","lambda_opt","beta_opt") %in% names(cv)))
  expect_length(cv$lambda_seq, 10)
  expect_length(cv$cv_err, 10)
  expect_length(cv$beta_opt, ncol(std$X))
  expect_true(cv$lambda_opt %in% cv$lambda_seq)
})
