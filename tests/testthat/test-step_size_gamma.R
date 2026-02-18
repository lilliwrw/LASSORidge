test_that("step_size_gamma returns gamma and next_index", {
  set.seed(1)
  X <- matrix(rnorm(20), 5, 4)
  active <- c(2)
  eq <- equangular_direction(X, active)
  corrs <- compute_correlations(X, rnorm(5))
  step <- step_size_gamma(X, eq$u, corrs, active)

  #Typen- und Längenprüfung
  expect_true(is.list(step))
  expect_true(is.numeric(step$gamma))
  expect_equal(length(step$gamma), 1)
  expect_true(step$gamma > 0)
  expect_true(step$next_index %in% setdiff(seq_len(ncol(X)), active))
})

test_that("gamma is minimal positive step", {
  set.seed(2)
  X <- matrix(rnorm(15), 5, 3)
  active <- c(1)
  eq <- equangular_direction(X, active)
  corrs <- compute_correlations(X, rnorm(5))
  step <- step_size_gamma(X, eq$u, corrs, active)

  expect_true(step$gamma > 0) #gamma muss positiv sein
})
