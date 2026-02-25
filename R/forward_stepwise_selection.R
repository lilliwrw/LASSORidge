#' Forward stepwise selection of coefficients
#'
#'
#' Returns a sequence of models for nested subsets of coefficients with increasing
#' size by forward-stepwise selection.
#'
#' @param data data frame containing the data to be used for the linear regression
#' @param input character vector: names of the coefficients
#' @param output character vector of length 1: name of the column containing the output data
#' @param subset TBD
#' @param weights TBD
#' @param nparam Numeric vector of the sizes of the subsets, that should be returned, default: all subset sizes are returned
#' @param unlist_return_value If set to TRUE and nparam only contains one subset size, return result as character vector
#' @param interactions Use interaction terms for the coefficients. (default: FALSE)
#' @param intercept Toggle, if an intercept term should be used in the linear model. (default: TRUE)
#' @param return_lm If TRUE, return a list of models for each specified number of coefficients instead. (default: FALSE)
#'
#'
#' @return A list of character vectors indexed by the number of coefficients used
#' in the model. The character vectors contain the names of the coefficients used.
#' If the toggle 'return_lm' is used, the function returns a list of models (lm-objects) instead.
#'
#' @export
#'
#' @examples
#' # TBD
#'
forward_stepwise_selection <- function(data, input, output, subset, weights, nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE) {
  ##############################################################################
  # Input validation                                                           #
  ##############################################################################

  # Check variable data for malformed input
  tryCatch(data <- as.data.frame(data), error = function(e) stop("data is not a data frame and cannot be coerced."))
  stopifnot("'data' contains NA" = !any(is.na.data.frame(data)))

  # Check variable input for malformed input
  stopifnot("'input' is not a character vector (of the column names containing the input data related to that coefficient)" = is.character(input),
            "'input' contains coeffiecients that are not present as columns in 'data'" = input %in% colnames(data)
            )

  # Check variable output for malformed output
  stopifnot("'output' is not a character vextor (of the column name containing the output to predict)" = is.character(output),
            "'output' must have length 1 (only one-dimensional outputs are supported)" = length(output)==1,
            "the column specified by 'output' doesn't exist in 'data'" = output %in% colnames(data)
            )
  if(output %in% input) warning(paste0("Column ", output, "is used as input and output data"))

  # Check variable weights for malformed input
  # TBD

  # Check variable interactions for malformed input
  stopifnot("'interactions' must be logical" = is.logical(interactions),
            "'interactions' must have length 1" = length(interactions) == 1)

  # Check variable intercept for malformed input
  stopifnot("'intercept' must be logical" = is.logical(intercept),
            "'intercept' must have length 1" = length(intercept) == 1)

  # Check variable return_lm for malformed input
  stopifnot("'return_lm' must be logical" = is.logical(return_lm),
            "'return_lm' must have length 1" = length(return_lm) == 1)

  # Check variable nparam for malformed input and correct if possible, sort by subset size
  # default: return subset of coefficients for every size of subsets
  if(is.null(nparam)) nparam <- 1:length(input)
  stopifnot("'nparam' must be numeric" = is.numeric(nparam))
  nparam <- as.integer(nparam)
  if (length(unique(nparam))!=length(nparam)) {
    warning("duplicate subset size present in 'nparam', dropping duplicates")
    nparam <- unique(nparam)
  }
  nparam <- sort(nparam)
  stopifnot("'nparam' contains negative subset size" = nparam >-1L,
            "'nparam' contains subset size greater than the number of coefficients" = nparam <= length(input)
            )

  # Check unlist_return_value for malformed input
  stopifnot("'unlist_return_value' has to be logical" = is.logical(unlist_return_value),
            "'unlist_return_value' must have length 1" = length(unlist_return_value)==1,
            "'unlist_return_value' cannot be TRUE if nparam has more than size to return" = !unlist_return_value || length(nparam)==1)

  # Check other variables for malformed input
  # TBD


  ##############################################################################
  # Computational part                                                         #
  ##############################################################################

  # Set maximum number of parameters, which should be used
  # default: full subset of parameters
  max_params <- max(nparam)

  # Initialize the list to return
  res <- as.list(rep(NA, times = max_params+1))
  names(res) <- as.character(0:max_params)

  # Create variables for storing the list of parameters already used in models with smaller k and a helper string for formula generation
  used_params <- character(0)
  if (intercept) {
    formula_string <- paste0(output, " ~ ")
  } else {
    formula_string <- paste0(output, " ~ 0 + ")
  }


  for (i in 1:max_params) {

    # Create variables to store the best next param to used and its RSS
    next_param <- NULL
    best_rss <- Inf
    best_model <- NULL

    # For every parameter not used calculate the regression including that parameter, then test against the previous best parameter to add an overwrite, if it has lower RSS
    for (param in (input[!input %in% used_params])) {
      model <- stats::lm(stats::as.formula(paste0(formula_string, param)), data)
      rss <- sum(stats::residuals(model)^2)
      if (rss < best_rss) {
         best_rss <- rss
         next_param <- param
         best_model <- model
      }
    }

    # Add best parameter to result and prepare formula_string for the next iteration
    used_params <- c(used_params, next_param)
    if (return_lm) {
      res[[i+1]] <- best_model
    } else {
      res[[i+1]] <- used_params
    }
    if (interactions) {
      formula_string <- paste0(formula_string, next_param, " * ")
    } else {
      formula_string <- paste0(formula_string, next_param, " + ")
    }
  }

  ##############################################################################
  # Format return value                                                        #
  ##############################################################################

  # Remove unwanted parts of the return list
  res <- res[as.character(nparam)]

  # If unlist_return_value is set to TRUE, unlist res
  if(unlist_return_value) res <- res[[1]]

  return(res)
}
