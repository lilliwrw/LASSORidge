#' Ridge Regression Estimator
#'
#' Computes the ridge regression estimator using standardized data
#' and returns the results transformed back to the original scale.
#'
#' @param X A numeric design matrix of dimension \eqn{n \times d}.
#' @param y A numeric response vector of length \eqn{n}.
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
  #Standardize als default TRUE
  'X <- as.matrix(X)
  y <- as.vector(y)
  lambda <- as.numeric(lambda)'

  #Input validation
  ridge_checkInputs(X, y, lambda)

  #Store column names
  coef_names <- colnames(X)
  if (is.null(coef_names)) {
    coef_names <- paste0("X", seq_len(ncol(X)))
  }

  #Standardize data
  std <- ridge_standardizeData(X, y)

  #Closed form solution on standardized data
  beta_scaled <- ridge_core(
    std$Xs,
    std$ys,
    lambda
  )

  #Back-transform coefficients
  recovered <- ridge_inverseTransform(
    beta_scaled,
    std$X_means,
    std$X_sds,
    std$y_mean
  )

  beta <- as.vector(recovered$beta)
  names(beta) <- coef_names
  intercept <- as.numeric(recovered$intercept)

  #Full coefficient vector
  coef_full <- c("(Intercept)" = intercept, beta)

  #Compute fitted values and residuals
  fitted <- as.vector(intercept + X %*% beta)
  residuals <- y - fitted

  #Build S3 "ridge" object
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
