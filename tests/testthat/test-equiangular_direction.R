test_that("equiangular_direction returns correct lengths", {
  set.seed(1)
  X <- matrix(rnorm(20), 5, 4)
  active <- c(2, 3)
  r <- rep(1, nrow(X)) # Beispiel Rest
  c_vec <- as.vector(crossprod(X, r))
  eq <- equiangular_direction(X, active,c_vec)

  expect_equal(length(eq$u), nrow(X)) #u hat Länge n

  expect_equal(length(eq$w), length(active)) #w hat Länge = Anzahl aktiver Variablen
})
