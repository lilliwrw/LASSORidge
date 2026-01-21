test_that("soft_threshold works for scalar inputs", {
  expect_equal(soft_threshold(3, 1), 2)
  expect_equal(soft_threshold(-3, 1), -2)
  expect_equal(soft_threshold(0.5, 1), 0)
  expect_equal(soft_threshold(-0.5, 1), 0)
})

test_that("soft_threshold is vectorized", {
  z <- c(-2, -0.5, 0, 0.5, 2)
  res <- soft_threshold(z, 1)

  expect_equal(res, c(-1, 0, 0, 0, 1))
})

test_that("soft_threshold with lambda = 0 returns input", {
  z <- rnorm(10)
  expect_equal(soft_threshold(z, 0), z)
})

test_that("soft_threshold errors for negative lambda", {
  expect_error(soft_threshold(1, -1))
})
