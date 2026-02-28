test_that("print.lar_model works and shows output", {
  set.seed(123)
  beta_mat <- matrix(runif(4*5), nrow=4, ncol=5)
  active_sets <- list(c(1), c(1,2), c(1,2,3), c(1,2,3,4), c(1,2,3,4))
  lar_fit <- structure(
    list(beta = beta_mat,
         active_sets = active_sets),
    class = "lar_model"
  )

  # Prüfen, dass print() läuft und etwas ausgibt
  expect_output(print(lar_fit))
})

test_that("print.lar_model shows correct dimensions", {
  beta_mat <- matrix(runif(3*4), 3, 4)
  active_sets <- list(c(1), c(1,2), c(1,2,3), c(1,2,3))
  lar_fit <- structure(
    list(beta = beta_mat,
         active_sets = active_sets),
    class = "lar_model"
  )

  output <- capture.output(print(lar_fit))
  expect_true(any(grepl("Number of predictors: 3", output)))
  expect_true(any(grepl("Number of steps: 4", output)))
})
