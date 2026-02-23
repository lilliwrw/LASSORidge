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
#'   \item \code{coefficients} — named vector of regression coefficients including intercept
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

  coef_names <- colnames(X)
  if (is.null(coef_names)) {
    coef_names <- paste0("X", seq_len(ncol(X)))
  }

  std <- ridge_standardizeData(X, y)

  beta_scaled <- ridge_core(
    std$Xs,
    std$ys,
    lambda
  )

  recovered <- ridge_inverseTransform(
    beta_scaled,
    std$X_means,
    std$X_sds,
    std$y_mean
  )

  beta <- as.vector(recovered$beta)
  names(beta) <- coef_names
  intercept <- as.numeric(recovered$intercept)

  coef_full <- c("(Intercept)" = intercept, beta)

  fitted <- as.vector(intercept + X %*% beta)
  residuals <- y - fitted

  result <- list(
    coefficients = coef_full,
    lambda = lambda,
    fitted.values = fitted,
    residuals = residuals,
    call = match.call()
  )

  class(result) <- "ridge"

  return(result)
}
