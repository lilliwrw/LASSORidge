#' Choose forward or backward stepwise selection
#'
#' \code{optimal_stepwise_selection} performs a short approximation of the
#' computational complexity of forward and backward selection for the number of
#' coefficients, that should be used in the final models. Then it chooses the
#' appropriate (probably faster) function out of \code{forward_stepwise_selection}
#' and \code{backward_stepwise_selection} and uses it to calculate the result. If
#' both theoretical complexities are equal, backward selection is used.
#' As the speed for small design matrices is influenced largely by the slow R- and
#' C-wrappers, the speed difference can be negligible or even opposite for small
#' problems. Therefore use this function only for large problems.
#'
#' @param use_backward_selection_by_default If set to \code{FALSE} the function
#' uses forward selection, if the theoretical complexities are approximatly equal.
#' (default: \code{TRUE})
#' @param only_return_faster_function Only returns the function, that is faster
#' for the given problem without carrying out the calculation.
#' @inheritParams backward_stepwise_selection
#'
#' @return A list of character vectors indexed by the number of coefficients used
#' in the model. The character vectors contain the names of the coefficients not dropped.
#' If the argument 'return_lm' is true, the function returns a list of models (lm-objects) instead.
#' If the argument 'only_return_faster_function' is true, the function returns
#' one of \code{backward_stepwise_selection} or \code{forward_stepwise_selection} as
#' function.
#'
#' @references
#' Drury, Matthew (2016, July 20). A Deep Dive Into How R Fits a Linear Model.
#' Scatterplot Smoothers. https://madrury.github.io/jekyll/update/statistics/2016/07/20/lm-in-R.html
#'
#' Winckelman, Thomas (2023, October 12). QR decomposition computational efficiency. CrossValidated.
#' https://stats.stackexchange.com/questions/393691/qr-decomposition-computational-efficiency
#'
#' @export
#'
#' @examples
#' set.seed(18645)
#' linear_coefficients <- 10^(1:10) * rnorm(10, 1, 0.1)
#' data <- matrix(runif(1000, 0, 100), ncol=10)
#' colnames(data) <- letters[1:10]
#' data <- as.data.frame(data)
#' # Synthesize test outputs
#' data$output <- rowSums(t(t(data)*linear_coefficients))
#' data$output <- data$output * rnorm(100, 1, 0.00001)
#' # Using forward stepwise selection
#' optimal_stepwise_selection(data, input=letters[1:10], output="output",nparam = 2)
#' # Using backward stepwise selection
#' optimal_stepwise_selection(data, input=letters[1:10], output="output",nparam = 9)
#'
optimal_stepwise_selection <- function(data, input, output, subset, weights = NULL,
                                       nparam = NULL, unlist_return_value = FALSE,
                                       interactions = FALSE, intercept = TRUE,
                                       return_lm = FALSE, use_backward_selection_by_default = TRUE,
                                       only_return_faster_function = FALSE,
                                       ...) {
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
  if(output %in% input) stop(paste0("Column ", output, "is used as input and output data"))

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

  # Check variable use_backward_selection_by_default for malformed input
  stopifnot("'use_backward_selection_by_default' must be logical" = is.logical(use_backward_selection_by_default),
            "'use_backward_selection_by_default' must have length 1" = length(use_backward_selection_by_default) == 1)

  # Check variable only_return_faster_function for malformed input
  stopifnot("'only_return_faster_function' must be logical" = is.logical(only_return_faster_function),
            "'only_return_faster_function' must have length 1" = length(only_return_faster_function) == 1)

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

  use_backward_selection <- use_backward_selection_by_default

  ##############################################################################
  ## Calculation
  ##############################################################################

  # Calculate the number of rows of the design matrices
  if (is.logical(subset)) {
    D <- sum(subset)
  } else {
    D <- length(subset)
  }

  # Calculate minimum, maximum and total number of coefficients
  min_param <- min(nparam)
  max_param <- max(nparam)
  total_params <- length(input)

  # Computational complexity of a singular lm call:
  # O(d²D) with d smaller dimension of row and column
  #  (i.e. number of coefficients, as the problem has more degrees of freedom than equations otherwise)
  #  for QR decomposition,
  # O(2*d²-d) for matrix-vector multiplication,
  # O(d²) for backward substitution
  # In conclusion complexity is crudely approximated by O((3+D)*d^2-d) for
  #  d = number of coefficients
  #  D = number of data rows in the data set
  if (interactions) {
    # Forward selection with interactions
    # Each step to add an i-th coefficient has to check total_params - (i-1)
    # coefficients with d = 2^i-1; each step therefore takes (total_params-(i-1))*((3+D)(2^i-1)²-(2^i-1)) computations
    fs_steps <- 0
    for (i in 1:max_param) {
      fs_steps <- fs_steps + (total_params-(i-1))*((3+D)*(2^i-1)^2-(2^i-1))
    }

    # Backward selection
    # Each step to remove a coefficient has to check i+1 possibilities, where i is
    # the new number of coefficients
    # the lm-call has size d = i; each step therefore takes (i+1)*((3+D)(2^i-1)²-(2^i-1)) computations
    bs_steps <- 0
    for (i in min_param:(total_params-1)) {
      bs_steps <- bs_steps + (i+1)*((3+D)*(2^i-1)^2-(2^i-1))
    }

    if (fs_steps<bs_steps) {
      use_backward_selection <- FALSE
    } else {
      use_backward_selection <- TRUE
    }
  } else {
    # Forward selection
    # Each step to add an i-th coefficient has to check total_params - (i-1)
    # coefficients with d = i; each step therefore takes (total_params-(i-1))*((3+D)i²-i) computations
    fs_steps <- 0
    for (i in 1:max_param) {
      fs_steps <- fs_steps + (total_params-(i-1))*((3+D)*i*i-i)
    }

    # Backward selection
    # Each step to remove a coefficient has to check i+1 possibilities, where i is
    # the new number of coefficients
    # the lm-call has size d = i; each step therefore takes (i+1)*((3+D)i²-i) computations
    bs_steps <- 0
    for (i in min_param:(total_params-1)) {
      bs_steps <- bs_steps + (i+1)*((3+D)*i*i-i)
    }

    if (fs_steps<bs_steps) {
      use_backward_selection <- FALSE
    } else {
      use_backward_selection <- TRUE
    }
  }


  ##############################################################################
  #  Function Call                                                             #
  ##############################################################################
  if (only_return_faster_function) {
    if (use_backward_selection) return(backward_stepwise_selection)
    else return(forward_stepwise_selection)
  }

  if (use_backward_selection) {
    return(backward_stepwise_selection(data, input, output, subset, weights = weights,
                                       nparam = nparam, unlist_return_value = unlist_return_value,
                                       interactions = interactions, intercept = intercept,
                                       return_lm = return_lm, ...))
  } else {
    return(forward_stepwise_selection(data, input, output, subset, weights = weights,
                                      nparam = nparam, unlist_return_value = unlist_return_value,
                                      interactions = interactions, intercept = intercept,
                                      return_lm = return_lm, ...))
  }
}
