test_that("step_size_gamma returns gamma and next_index", {
  set.seed(1)
  X <- matrix(rnorm(20), 5, 4)
  active <- c(2)
  r <- rnorm(5)
  c_vec <- compute_correlations(X, r)
  eq <- equiangular_direction(X, active,c_vec)
  step <- step_size_gamma(X, eq$u, c_vec, active)

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
  corrs <- compute_correlations(X, rnorm(5))
  eq <- equiangular_direction(X, active, corrs)
  step <- step_size_gamma(X, eq$u, corrs, active)

  expect_true(step$gamma > 0) #gamma muss positiv sein
})
