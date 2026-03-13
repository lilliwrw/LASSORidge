#' Validate Inputs for Ridge Regression
#'
#' Internal helper function. Performs basic consistency checks for the ridge regression estimator.
#'
#' @param X A numeric design matrix of dimension \eqn{n \times d}.
#' @param y A numeric response vector of length \eqn{n}.
#' @param lambda Non-negative regularization parameter.
#'
#' @returns Invisible \code{TRUE} if all checks pass, else an informative error.
ridge_checkInputs <- function(X, y, lambda){
  #Assume Input coerced to matrix/vector/numeric

  #Check X
  if (!is.numeric(X)) stop("X must be numeric.")

  if (anyNA(X) || !all(is.finite(X))) {
    stop("X must not contain NA, NaN or Inf.")
  }

  if (nrow(X) == 0 || ncol(X) == 0) {
    stop("X must have positive dimensions.")
  }

  #Check y
  if (!is.numeric(y)) stop("y must be numeric.")

  if (anyNA(y) || !all(is.finite(y))) {
    stop("y must not contain NA, NaN or Inf.")
  }

  #Check compatibility
  if (nrow(X) != length(y)) {
    stop("dimensions of X and y do not fit.")
  }

  #Check lambda
  if (length(lambda) != 1) {
    stop("lambda must be a single value.")
  }

  if (!is.finite(lambda) || lambda < 0) stop("lambda must be a finite non-negative value.")

  invisible(TRUE)
}
