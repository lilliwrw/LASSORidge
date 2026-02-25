test_that("print.ridge_path returns object invisibly", {
  set.seed(1)
  X <- matrix(rnorm(50), 10, 5)
  y <- rnorm(10)

  path <- ridge_path(X, y, lambda = c(0.1, 1, 10))

  expect_invisible(print(path))
})
