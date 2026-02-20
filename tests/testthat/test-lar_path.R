test_that("lar_path returns correct structure", {
  set.seed(1)
  X <- scale(matrix(rnorm(50), 10, 5))
  y <- scale(rnorm(10), center = TRUE, scale = FALSE)

  fit <- lar_path(X, y)

  expect_true(is.matrix(fit$beta_path))
  expect_true(is.list(fit$active_sets))
})
