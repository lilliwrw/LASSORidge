test_that("forward selection basic functionality", {
  set.seed(18645)
  linear_coefficients <- 10^(1:10) * rnorm(10, 1, 0.1)
  data <- matrix(runif(1000, 0, 100), ncol=10)
  colnames(data) <- letters[1:10]
  data <- as.data.frame(data)
  data$output <- rowSums(t(t(data)*linear_coefficients))
  data$output <- data$output * rnorm(100, 1, 0.00001)
  lm(output ~ a+b+c+d+e+f+g+h+i+j, data)
  res <- forward_stepwise_selection(data, input=letters[1:10], output="output",nparam = 10)
  expect_true(all(res[[1]] == letters[c(10,9,8,7,6,5,2,1,4,3)]))
})
