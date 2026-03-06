#' Backward stepwise selection of coefficients
#'
#'
#' \code{backward_stepwise_selection} is used to calculate, which coefficients in
#' a linear regression can be dropped with least influence on the residual square
#' sum. The function returns either a sequence of character vectors of the coefficient
#' names or a sequence of \code{lm}-obejects for nested subsets of coefficients
#' with increasing size. The result is calculated by backward-stepwise selection
#' (i.e. subsequently removing coefficients from the model).
#'
#' @param data data frame containing the data to be used for the linear regression
#' @param input character vector: names of the coefficients
#' @param output character vector of length 1: name of the column containing the output data
#' @param subset Optionally specify, which rows of the data frame should be used
#' for the calculation. This argument is passed directly to \code{lm()}, details can be
#' found in its man page. \code{subset} can be given as logical vector with a value
#' for each row or a numerical vector with the number of the rows, that should be
#' used (default: Use all rows)
#' @param weights Optionally use weights for the rows in the data frame. This
#' argument is passed directly to \code{lm()}, details can be found in its man page. If
#' provided as non-NULL, \code{weights} must be a numerical vector containing the weights.
#' (default: \code{NULL}, i.e. all weights equal \eqn{1})
#' @param nparam Numeric vector of the sizes of the subsets, that should be returned,
#' (default: \code{NULL}, i.e. all subset sizes are returned)
#' @param unlist_return_value If set to TRUE and \code{nparam} only contains one
#' subset size, return result as character vector. (default: \code{FALSE})
#' @param interactions Use interaction terms for the coefficients. (default: \code{FALSE})
#' @param intercept Toggle, if an intercept term should be used in the linear model.
#' (default: \code{TRUE})
#' @param return_lm If \code{TRUE}, return a list of models for each specified number of coefficients instead. (default: FALSE)
#' @param ... Additional arguments to be passed to the calls of \code{lm()}.
#'
#'
#' @return A list of character vectors indexed by the number of coefficients used
#' in the model. The character vectors contain the names of the coefficients not dropped.
#' If the argument 'return_lm' is true, the function returns a list of models (lm-objects) instead.
#'
#' @export
#'
#' @examples
#' ## Generate test data (10 coefficients, each about 10 times as large as the previous one)
#' set.seed(18645)
#' linear_coefficients <- 10^(1:10) * rnorm(10, 1, 0.1)
#' data <- matrix(runif(1000, 0, 100), ncol=10)
#' colnames(data) <- letters[1:10]
#' data <- as.data.frame(data)
#' # Synthesize test outputs
#' data$output <- rowSums(t(t(data)*linear_coefficients))
#' data$output <- data$output * rnorm(100, 1, 0.00001)
#' # Calculate which coeffiecients can be dropped with least influence on residual square sum
#' backward_stepwise_selection(data, input=letters[1:10], output="output",nparam = 1:10)
#'
backward_stepwise_selection <- function(
    data, input, output, subset, weights = NULL,
    nparam = NULL, unlist_return_value = FALSE, interactions = FALSE, intercept = TRUE, return_lm = FALSE, ...
) {
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
  if (!is.null(weights)) {
    stopifnot("'weights' must be numeric" = is.numeric(weights),
              "'weights' must have length equal to the number of rows in 'data" = length(weights)==nrow(data))
  }

  # Check variable subset for malformed input
  if (!missing(subset)) {
    stopifnot("'subset' must be logical or numeric" = is.logical(subset) || is.numeric(subset),
              "If 'subset' is provided as logical, it must have length equal to the number of rows in 'data'" = is.numeric(subset) || length(subset)==nrow(data),
              "If 'subset' is provided as numeric, it cannot contain nonpositive numbers" = is.logical(subset) || min(subset)>0,
              "If 'subset' is provided as numeric, it cannot contain numbers greater than the number of rows in 'data'" = is.logical(subset) || max(subset)<=nrow(data))
    if (is.numeric(subset) && length(unique(as.integer(subset))) != length(subset)) {
      warning("'subset' was provided as numeric vector, but contains duplicate values. Duplicate values will be dropped.")
    }
  } else {
    subset <- 1:nrow(data)
  }

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
            "'unlist_return_value' cannot be TRUE if nparam has more than one subset size to return" = !unlist_return_value || length(nparam)==1)


  ##############################################################################
  # Computational part                                                         #
  ##############################################################################

  # Set minimum number of parameters, which should be used
  # default: full subset of parameters
  min_params <- min(nparam)

  # Initialize the list to return
  res <- as.list(rep(NA, times = length(input)))
  names(res) <- as.character(1:length(input))

  # Create variables for storing the list of parameters not dropped already in models with
  # smaller k and a helper string for formula generation
  undropped_params <- input
  if (intercept) {
    formula_string <- paste0(output, " ~ ")
  } else {
    formula_string <- paste0(output, " ~ 0 + ")
  }

  # Initialize output for full model
  if (return_lm) {
    res[[length(input)]] <- stats::lm(stats::as.formula(paste0(formula_string, generate_formula_arguments(undropped_params, interactions = interactions))),
                                      data, subset = subset, weights = weights, ...)
  } else {
    res[[length(input)]] <- undropped_params
  }

  if (min_params<length(input)) {
    for (i in (length(input)-1):min_params) {

      # Create variables to store the best next param to drop and its RSS
      next_param <- NULL
      best_rss <- Inf
      best_model <- NULL

      # For every parameter not already dropped calculate the regression without that parameter,
      # then test against the previous best parameter to drop an overwrite, if it has lower RSS
      for (param in 1:length(undropped_params)) {
        model <- stats::lm(stats::as.formula(paste0(formula_string, generate_formula_arguments(undropped_params[-param], interactions = interactions))),
                           data, subset = subset, weights = weights, ...)
        if (is.null(weights)) {
          rss <- sum(stats::residuals(model)^2)
        } else {
          rss <- sum(weights * stats::residuals(model)^2)
        }

        if (rss < best_rss) {
          best_rss <- rss
          next_param <- param
          best_model <- model
        }
      }

      # Add best parameter to result and prepare formula_string for the next iteration
      undropped_params <- undropped_params[-next_param]
      if (return_lm) {
        res[[i]] <- best_model
      } else {
        res[[i]] <- undropped_params
      }
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


#' Helper function to generate modula forulae
#'
#' \code{generate_formula_arguments} concatenates the arguments using \code{+}
#' as a seperator.
#'
#' @param arguments character vector with the arguments to be used
#' @param interactions If set to \code{TRUE} the arguments are combined using
#' the \code{*} operator instead of \code{+}. (default: \code{FALSE})
#'
#' @return A character vector of length 1 containing the concatenated string
#'
#' @examples
#' LASSORidge:::generate_formula_arguments(c("a","b"))
#'
generate_formula_arguments <- function(arguments, interactions = FALSE) {
  if(interactions) {
    return(paste(arguments, collapse = " * "))
  } else {
    return(paste(arguments, collapse = " + "))
  }
}
