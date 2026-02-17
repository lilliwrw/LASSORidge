#' Forward stepwise selection of coefficients
#'
#' Returns a sequence of models for nested subsets of coefficients with increasing size by forward-stepwise selection.
#'
#' @param data data frame containing the data to be used for the linear regression
#' @param input character vector: names of the coefficients
#' @param output character vector of length 1: name of the column containing the output data
#'
#'
#' @return A list of models indexed by the number of parameters used in the model
#'
#' @export
#'
#' @examples
#' # TBD
#'
forward_stepwise_selection <- function(data, input, output, subset, weights, nparam = NULL) {
  # Check variable data for malformed input
  # TBD

  # Check variable weights for malformed input
  # TBD

  # Check variable nparam for malformed input and correct if possible
  # TBD

  # Check other variables for malformed input
  # TBD

  # Set maximum number of parameters, which should be used
  # default: full subset of parameters
  max_params <- max(nparam)

  # Initialize the list to return
  res <- as.list(rep(NA, times = max_params+1))
  names(res) <- as.character(0:max_params)

  # Create variables for storing the list of parameters already used in models with smaller k and a helper string for formula generation
  used_params <- character(0)
  formula_string <- paste0(output, " ~ ")

  for (i in 1:max_params) {

    # Create variables to store the best next param to used and its RSS
    next_param <- NULL
    best_rss <- Inf

    # For every parameter not used calculate the regression including that parameter, then test against the previous best parameter to add an overwrite, if it has lower RSS
    for (param in (input[!input %in% used_params])) {
      model <- lm(as.formula(paste0(formula_string, param)), data)
      rss <- sum(residuals(model)^2)
      if (rss < best_rss) {
         best_rss <- rss
         next_param <- param
      }
    }

    # Add best parameter to result and prepare formula_string for the next iteration
    used_params <- c(used_params, next_param)
    res[[i+1]] <- used_params
    formula_string <- paste0(formula_string, " + ", next_param, " + ")
  }

  # Remove unwanted parts of the return list
  res <- res[as.character(nparam)]

  return(res)
}
