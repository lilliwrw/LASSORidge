#' Closed-form Ridge Estimator
#'
#' Computes the ridge regression estimator in closed form.
#' \deqn{\hat{\beta}_\lambda =
#' (X^\top X + \lambda I_d)^{-1} X^\top y}
#'
#' @param X A numeric design matrix of dimension \eqn{n \times d}.
#' @param y A numeric response vector of length \eqn{n}.
#' @param lambda Non-negative regularization parameter.
#'
#' @returns A numeric vector of length \eqn{d} containing the ridge coefficients
#' corresponding to the columns of \code{X}.
#' @export
#'
#' @seealso [ridge()] user-facing interface for ridge regression.
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(100), 20, 5)
#' y <- rnorm(20)
#' ridge_core(X, y, lambda = 1)
ridge_core <- function(X, y, lambda){
  d <- ncol(X)

  XtX <- crossprod(X)
  Xty <- crossprod(X, y)

  beta <- solve(XtX + lambda * diag(d), Xty)
  beta <- as.vector(beta)

  beta
}
