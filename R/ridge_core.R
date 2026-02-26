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
#' @returns A numeric vector of length d containing the ridge coefficients.
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(100), 20, 5)
#' y <- rnorm(20)
#' ridge_core(X, y, lambda = 1)
ridge_core <- function(X, y, lambda){
  d <- ncol(X)
  beta <- solve(t(X) %*% X + lambda * diag(d),
                t(X) %*% y)
  return(beta)
}
