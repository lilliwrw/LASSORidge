test_that("coefficients update correctly", {
  beta <- c(0, 0, 0)
  active <- c(2)
  w <- c(1)
  gamma <- 0.5
  r <- c(1, 1)
  u <- c(0.2, 0.2)

  out <- update_coefficients(beta, active, w, gamma, r, u)

  expect_equal(out$beta[2], 0.5)
  expect_equal(out$beta[c(1,3)], c(0,0))
})

test_that("residual updates correctly", {
  beta <- c(0,0)
  active <- c(1)
  w <- c(1)
  gamma <- 0.2
  r <- c(1, 2)
  u <- c(0.5, 0.5)

  out <- update_coefficients(beta, active, w, gamma, r, u)

  expect_equal(out$r, r - gamma * u)
})
