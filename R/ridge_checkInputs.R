#' Validate Inputs for Ridge Regression
#'
#' Performs basic consistency checks for the ridge regression estimator.
#'
#' @param X A numeric design matrix of dimension n x d.
#' @param y A numeric response vector of length n.
#' @param lambda Non-negative regularization parameter.
#'
#' @returns TRUE if all checks pass, else an informative error.
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20), 10, 2)
#' y <- rnorm(10)
#' ridge_checkInputs(X, y, lambda = 1)
ridge_checkInputs <- function(X, y, lambda){
  if (!is.matrix(X)) stop("X must be a matrix")

  if (!is.numeric(X)) stop("X must be numeric.")
  if (!is.numeric(y)) stop("y must be numeric.")

  if (nrow(X) != length(y)) {
    stop("dimensions of X and y do not fit.")
  }

  if (!is.numeric(lambda) || length(lambda) != 1) {
    stop("lambda must be a single numeric value.")
  }

  if (lambda < 0) stop("lambda must be non-negative.")

  #TODO ...

  invisible(TRUE)
}
