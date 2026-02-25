#' Ridge Coefficient Path
#'
#' Computes ridge regression estimates over a sequence of regularization parameters.
#'
#' @param X A numeric design matrix of dimension n x d.
#' @param y A numeric response vector of length n.
#' @param lambda A numeric vector of non-negative regularization parameters.
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
#' ridge_path(X, y, lambda = c(0.1, 1, 10))
ridge_path <- function(X, y, lambda){
  ridge_checkInputs(X, y, lambda = 0)

  if (!is.numeric(lambda) || any(lambda < 0)) {
    stop("lambda must be a non-negative numeric vector.")
  }

  lambda <- sort(unique(lambda))

  coef_mat <- sapply(lambda, function(l) {
    ridge(X, y, l)$coefficients
  })

  if (is.vector(coef_mat)) {
    coef_mat <- matrix(coef_mat, ncol = 1)
  }

  colnames(coef_mat) <- paste0("lambda=", lambda)

  result <- list(
    lambda = lambda,
    coefficients = coef_mat,
    call = match.call()
  )

  class(result) <- "ridge_path"

  return(result)
}
