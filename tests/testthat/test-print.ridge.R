test_that("print.ridge returns object invisibly", {
  set.seed(1)
  X <- matrix(rnorm(50), 10, 5)
  y <- rnorm(10)

  fit <- ridge(X, y, lambda = 1)

  expect_invisible(print(fit))
})
