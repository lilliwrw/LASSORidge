#' Ridge Regression Estimator
#'
#' Computes the ridge regression estimator using standardized data
#' and returns the results transformed back to the original scale.
#'
#'
#' @param X A numeric design matrix of dimension n x d.
#' @param y A numeric response vector of length n.
#' @param lambda A non-negative regularization parameter.
#'
#' @returns An object of class \code{"ridge"} containing:
#' \itemize{
#'   \item \code{coefficients} — regression coefficients
#'   \item \code{intercept} — intercept term
#'   \item \code{lambda} — regularization parameter
#'   \item \code{fitted.values} — fitted values
#'   \item \code{residuals} — residuals
#'   \item \code{call} — matched function call
#' }
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(100), 20, 5)
#' y <- rnorm(20)
#' fit <- ridge(X, y, lambda = 1)
#' fit$coefficients
ridge <- function(X, y, lambda){
  ridge_checkInputs(X, y, lambda)

  std <- ridge_standardizeData(X, y)

  beta_scaled <- ridge_core(
    std$Xs,
    std$ys,
    lambda
  )

  recovered <- ridge_inverseTransform(
    beta_scaled,
    std$x_means,
    std$x_sds,
    std$y_mean
  )

  beta <- recovered$beta
  intercept <- recovered$intercept

  fitted <- intercept + X %*% beta
  residuals <- y - fitted

  result <- list(
    coefficients = as.vector(beta),
    intercept = intercept,
    lambda = lambda,
    fitted.values = as.vector(fitted),
    residuals = as.vector(residuals),
    call = match.call()
  )

  class(result) <- "ridge"

  return(result)
}
