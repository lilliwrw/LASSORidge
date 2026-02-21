test_that("plot_lar_path runs without error", {
  set.seed(1)
  X <- matrix(rnorm(40 * 4), 40, 4)
  y <- rnorm(40)

  fit <- lar(X, y)

  expect_silent(
    plot_lar_path(fit$beta)
  )
})

test_that("plot_lar_path works with step axis", {
  set.seed(1)
  X <- matrix(rnorm(40 * 3), 40, 3)
  y <- rnorm(40)

  fit <- lar(X, y)

  expect_silent(
    plot_lar_path(fit$beta, x_axis = "step")
  )
})

test_that("invalid x_axis throws error", {
  set.seed(123)
  X <- matrix(rnorm(30 * 3), 30, 3)
  y <- rnorm(30)

  fit <- lar(X, y)

  expect_error(
    plot_lar_path(fit$beta, x_axis = "invalid")
  )
})
