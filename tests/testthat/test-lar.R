test_that("beta matrix has correct dimensions", {
  set.seed(1)
  n <- 40
  p <- 4
  X <- matrix(rnorm(n * p), n, p)
  y <- rnorm(n)

  fit <- lar(X, y)

  expect_true(is.matrix(fit$beta))
  expect_equal(nrow(fit$beta), p)
  expect_equal(length(fit$active_sets), ncol(fit$beta))
})

test_that("active set grows monotonically", {
  set.seed(1)
  X <- matrix(rnorm(50 * 5), 50, 5)
  y <- rnorm(50)

  fit <- lar(X, y)

  sizes <- sapply(fit$active_sets, length)

  expect_true(all(diff(sizes) >= 0))
})

test_that("lar has same output for same input", {
  set.seed(42)
  X <- matrix(rnorm(50 * 4), 50, 4)
  y <- rnorm(50)

  fit1 <- lar(X, y)
  fit2 <- lar(X, y)

  expect_equal(fit1$beta, fit2$beta)
})

test_that("standardize argument works", {
  set.seed(123)
  X <- matrix(rnorm(40 * 4), 40, 4)
  y <- rnorm(40)

  fit_std  <- lar(X, y, standardize = TRUE)
  fit_nstd <- lar(X, y, standardize = FALSE)

  expect_equal(dim(fit_std$beta), dim(fit_nstd$beta))
  expect_equal(length(fit_std$active_sets),
               length(fit_nstd$active_sets))
})
