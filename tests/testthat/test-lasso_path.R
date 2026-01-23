test_that("lasso_path returns correct dimensions", {
  set.seed(1)
  X <- matrix(rnorm(50*5), nrow=50)
  y <- rnorm(50)
  std <- standardize_data(X, y)

  path <- lasso_path(std$X, std$y, n_lambda=10)

  expect_equal(length(path$lambda_seq), 10)
  expect_equal(dim(path$beta), c(ncol(std$X), 10))

  # Prüfen: größtes λ, dann alle beta etwa 0
  expect_true(all(abs(path$beta[,1]) < 1e-6))

  # Prüfen: kleinstes λ, nicht alle beta null
  expect_true(any(abs(path$beta[,10]) > 0))
})
