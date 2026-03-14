test_that("optimal selection input validation", {
  # Generate testing data
  set.seed(18645)
  linear_coefficients <- 10^(1:3) * rnorm(3, 1, 0.1)
  data <- matrix(runif(300, 0, 100), ncol=3)
  colnames(data) <- letters[1:3]
  data <- as.data.frame(data)
  data$output <- rowSums(t(t(data)*linear_coefficients))
  data$output <- data$output * rnorm(100, 1, 0.0001)

  # Test for malformed input in data
  expect_error(optimal_stepwise_selection(data = environment()), regexp = "data is not a data frame and cannot be coerced.")
  expect_error(optimal_stepwise_selection(data = data.frame(c(NA, NA))), regexp = "'data' contains NA")

  # Test for malformed input in input
  expect_error(optimal_stepwise_selection(data, c(1,2)), regexp = "is not a character vector \\(of the column names containing the input data related to that coefficient\\)")
  expect_error(optimal_stepwise_selection(data, c("a", "e")), regexp = "contains coeffiecients that are not present as columns in")

  # Test for malformed input in output
  expect_error(optimal_stepwise_selection(data, c("a", "b"), output = 1:10), regexp = "is not a character vextor \\(of the column name containing the output to predict\\)")
  expect_error(optimal_stepwise_selection(data, c("a", "b"), output = c("c", "output")), regexp = "'output' must have length 1 \\(only one-dimensional outputs are supported\\)")
  expect_error(optimal_stepwise_selection(data, c("a", "b"), output = c("d")), regexp = "the column specified by 'output' doesn't exist in 'data'")
  expect_error(optimal_stepwise_selection(data, c("a", "b"), output = c("a")), regexp = "is used as input and output data")

  # Test for malformed input in subset
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", subset = environment(), weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "'subset' must be logical or numeric")
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", subset = TRUE, weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "If 'subset' is provided as logical, it must have length equal to the number of rows in 'data'")
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", subset = -5, weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "If 'subset' is provided as numeric, it cannot contain nonpositive numbers")
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", subset = Inf, weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "If 'subset' is provided as numeric, it cannot contain numbers greater than the number of rows in 'data'")

  # Test for malformed input in interactions
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = list(5,5), intercept = TRUE, return_lm = FALSE), regexp = "'interactions' must be logical")
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = c(TRUE, FALSE), intercept = TRUE, return_lm = FALSE), regexp = "'interactions' must have length 1")

  # Test for malformed input in intercept
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = list(5,5), return_lm = FALSE), regexp = "'intercept' must be logical")
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = c(TRUE, FALSE), return_lm = FALSE), regexp = "'intercept' must have length 1")

  # Test for malformed input in nparam
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = TRUE, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "'nparam' must be numeric")
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = -1:5, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "'nparam' contains negative subset size")
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = 1:5, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = "'nparam' contains subset size greater than the number of coefficients")

  # Test for malformed input in use_backward_selection_by_default
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE, use_backward_selection_by_default = 5), regexp = "'use_backward_selection_by_default' must be logical")
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE, use_backward_selection_by_default = c(TRUE, FALSE)), regexp = "'use_backward_selection_by_default' must have length 1")

  # Test for malformed input in only_return_faster_function
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE, only_return_faster_function = 5), regexp = "'only_return_faster_function' must be logical")
  expect_error(optimal_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE, only_return_faster_function = c(TRUE, FALSE)), regexp = "'only_return_faster_function' must have length 1")

  # expect_error(optimal_stepwise_selection(data, letters[1:3], "output", weights = NULL, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE), regexp = )
})
