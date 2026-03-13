#' Ridge Coefficient Path
#'
#' Computes ridge regression estimates over a sequence of regularization parameters.
#'
#' @param X A numeric design matrix of dimension \eqn{n \times d}.
#' @param y A numeric response vector of length \eqn{n}.
#' @param lambda A numeric vector of non-negative regularization parameters.
#' @param standardize Logical; if \code{TRUE} (default), the predictors are standardized before fitting, else only \code{X} is centered.
#'
#' @returns An object of class \code{"ridge_path"} containing:
#'  \itemize{
#'   \item \code{lambda} — vector of lambda values
#'   \item \code{coefficients} — matrix of coefficients
#'   \item \code{call} — matched function call
#' }
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(50), 10, 5)
#' y <- rnorm(10)
#' #Fit the ridge model
#' ridge_path(X, y, lambda = c(0.1, 1, 10))
ridge_path <- function(X, y, lambda, standardize = TRUE){
  #Check X and y
  X <- as.matrix(X)
  y <- as.vector(y)
  ridge_checkInputs(X, y, lambda = 0)

  #Check lambda vector
  if (!is.numeric(lambda) || any(!is.finite(lambda)) ||any(lambda < 0)) {
    stop("lambda must be a finite non-negative numeric vector.")
  }

  lambda <- sort(unique(lambda))

  #Calculate path via ridge
  fits <- lapply(lambda, function(l) {
    ridge(X, y, l, standardize = standardize)
  })

  coef_mat <- sapply(fits, function(f) f$coefficients)

  #If vector, set dimension
  if (is.vector(coef_mat)) {
    coef_mat <- matrix(coef_mat, ncol = 1)
  }

  #Set dimnames
  colnames(coef_mat) <- paste0("lambda=", lambda)
  rownames(coef_mat) <- names(fits[[1]]$coefficients)

  #Build S3 "ridge_path" object
  result <- list(
    lambda = lambda,
    coefficients = coef_mat,
    call = match.call()
  )

  class(result) <- "ridge_path"

  return(result)
}
