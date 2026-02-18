test_that("correlations equal matrix multiplication", {
  set.seed(1)
  X <- matrix(rnorm(20), 5, 4)
  r <- rnorm(5)

  expect_equal(
    compute_correlations(X, r),
    drop(t(X) %*% r)
  )
})
