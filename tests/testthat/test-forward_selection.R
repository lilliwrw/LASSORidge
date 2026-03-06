test_that("forward selection basic functionality", {
  # Generate testing data
  set.seed(18645)
  linear_coefficients <- 10^(1:10) * rnorm(10, 1, 0.1)
  data <- matrix(runif(1000, 0, 100), ncol=10)
  colnames(data) <- letters[1:10]
  data <- as.data.frame(data)
  data$output <- rowSums(t(t(data)*linear_coefficients))
  data$output <- data$output * rnorm(100, 1, 0.00001)

  # Calculate coefficient order
  res <- forward_stepwise_selection(data, input=letters[1:10], output="output",nparam = 10)

  # Check, if the result is correct
  expect_true(all(res[[1]] == letters[c(10,9,8,7,6,5,2,1,4,3)]))
})

test_that("forward selection subset", {
  # Calculate example as in basic test with lower number of coefficients
  set.seed(18645)
  linear_coefficients <- 10^(1:3) * rnorm(3, 1, 0.1)
  data <- matrix(runif(300, 0, 100), ncol=3)
  colnames(data) <- letters[1:3]
  data <- as.data.frame(data)
  data$output <- rowSums(t(t(data)*linear_coefficients))
  data$output <- data$output * rnorm(100, 1, 0.0001)
  res <- forward_stepwise_selection(data, input=letters[1:3], output="output",nparam = 3, return_lm = TRUE)

  # Add 100 random rows to the data frame
  data2 <- matrix(runif(400, 0, 100), ncol=4)
  colnames(data2) <- c(letters[1:3], "output")
  data2 <- as.data.frame(data2)
  data <- rbind(data, data2)

  # Calculate the models for only the first 100 rows using logical mask or numerical indices
  res2 <- forward_stepwise_selection(data, input=letters[1:3], output="output",nparam = 3, return_lm = TRUE, subset = 1:100)
  res3 <- forward_stepwise_selection(data, input=letters[1:3], output="output",nparam = 3, return_lm = TRUE, subset = c(rep(TRUE, 100), rep(FALSE, 100)))

  # Check if all three models use the same data
  expect_true(identical(res$model, res2$model))
  expect_true(identical(res$model, res3$model))
})

test_that("forward selection input validation", {
  # Generate testing data
  set.seed(18645)
  linear_coefficients <- 10^(1:3) * rnorm(3, 1, 0.1)
  data <- matrix(runif(300, 0, 100), ncol=3)
  colnames(data) <- letters[1:3]
  data <- as.data.frame(data)
  data$output <- rowSums(t(t(data)*linear_coefficients))
  data$output <- data$output * rnorm(100, 1, 0.0001)

  # Test for malformed input in data
  expect_error(forward_stepwise_selection(data = environment()), regexp = "data is not a data frame and cannot be coerced.")
  expect_error(forward_stepwise_selection(data = data.frame(c(NA, NA))), regexp = "'data' contains NA")

  # Test for malformed input in input
  expect_error(forward_stepwise_selection(data, c(1,2)), regexp = "is not a character vector \\(of the column names containing the input data related to that coefficient\\)")
  expect_error(forward_stepwise_selection(data, c("a", "e")), regexp = "contains coeffiecients that are not present as columns in")

  # Test for malformed input in output
  expect_error(forward_stepwise_selection(data, c("a", "b"), output = 1:10), regexp = "is not a character vextor \\(of the column name containing the output to predict\\)")
  expect_error(forward_stepwise_selection(data, c("a", "b"), output = c("c", "output")), regexp = "'output' must have length 1 \\(only one-dimensional outputs are supported\\)")
  expect_error(forward_stepwise_selection(data, c("a", "b"), output = c("d")), regexp = "the column specified by 'output' doesn't exist in 'data'")
  expect_warning(forward_stepwise_selection(data, c("a", "b"), output = c("a")), regexp = "is used as input and output data")

  # Test for malformed input in weights
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = "a", nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "'weights' must be numeric")
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = 1:99, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "'weights' must have length equal to the number of rows in 'data")

  # Test for malformed input in subset
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", subset = environment(), weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "'subset' must be logical or numeric")
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", subset = TRUE, weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "If 'subset' is provided as logical, it must have length equal to the number of rows in 'data'")
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", subset = -5, weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "If 'subset' is provided as numeric, it cannot contain nonpositive numbers")
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", subset = Inf, weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "If 'subset' is provided as numeric, it cannot contain numbers greater than the number of rows in 'data'")
  expect_warning(forward_stepwise_selection(data, letters[1:3], "output", subset = c(1,1,1), weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "'subset' was provided as numeric vector, but contains duplicate values. Duplicate values will be dropped.")

  # Test for malformed input in interactions
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = list(5,5), intercept = TRUE, return_lm = FALSE), regexp = "'interactions' must be logical")
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = c(TRUE, FALSE), intercept = TRUE, return_lm = FALSE), regexp = "'interactions' must have length 1")

  # Test for malformed input in intercept
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = list(5,5), return_lm = FALSE), regexp = "'intercept' must be logical")
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = c(TRUE, FALSE), return_lm = FALSE), regexp = "'intercept' must have length 1")

  # Test for malformed input in return_lm
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = list(5,5)), regexp = "'return_lm' must be logical")
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = c(TRUE, FALSE)), regexp = "'return_lm' must have length 1")

  # Test for malformed input in unlist_return_value
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = list(5,5), interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "'unlist_return_value' has to be logical")
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = c(TRUE, FALSE), interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "'unlist_return_value' must have length 1")
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = TRUE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "'unlist_return_value' cannot be TRUE if nparam has more than one subset size to return")

  # Test for malformed input in nparam
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = TRUE, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "'nparam' must be numeric")
  expect_warning(forward_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = c(1,1,1,1), unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "duplicate subset size present in 'nparam', dropping duplicates")
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = -1:5, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "'nparam' contains negative subset size")
  expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = 1:5, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "'nparam' contains subset size greater than the number of coefficients")

  # expect_error(forward_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = )
})
