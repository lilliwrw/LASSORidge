test_that("equangular_direction returns correct lengths", {
  set.seed(1)
  X <- matrix(rnorm(20), 5, 4)
  active <- c(2, 3)
  eq <- equangular_direction(X, active)

  expect_equal(length(eq$u), nrow(X)) #u hat Länge n

  expect_equal(length(eq$w), length(active)) #w hat Länge = Anzahl aktiver Variablen

  expect_equal(length(eq$A), 1) #A skalar
})
