test_that("standardize_data returns a list with expected elements", {
  X <- matrix(1:6, nrow = 3)
  y <- c(1, 2, 3)

  std <- standardize_data(X, y)

  expect_type(std, "list")
  expect_true(all(c("X", "y", "X_means", "X_scales", "y_mean") %in% names(std)))
})
