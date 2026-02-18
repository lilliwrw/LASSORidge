test_that("active_set selects the correct predictor", {
  corrs <- c(0.5, -0.8, 0.3)

  #genau ein Max-Wert
  expect_equal(active_set(corrs), 2)

  #mehere Max-Werte
  corrs2 <- c(0.9, -0.9, 0.1)
  result <- active_set(corrs2)
  expect_true(all(result %in% c(1,2)))
  expect_equal(length(result), 2)

  #ausschliesen aktiver Variablen
  corrs3 <- c(0.4, 0.6, -0.6)
  result <- active_set(corrs3, already_active = 2)
  expect_equal(result, 3)

  #alle maximalen Kandidaten zurückgeben
  corrs4 <- c(0.7, -0.7, 0.2)
  result <- active_set(corrs4)
  expect_true(all(result %in% c(1,2)))
  expect_equal(length(result), 2)
})
