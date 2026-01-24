test_that("simulate_lasso_data returns correct dimensions", {
  data <- simulate_lasso_data(n=20, p=5, n_active=2, seed=1)

  expect_equal(dim(data$X), c(20,5))
  expect_equal(length(data$y), 20)
  expect_equal(length(data$beta), 5)
  expect_equal(sum(data$beta != 0), 2)
})
