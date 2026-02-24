#' Predict Method for Ridge Model
#'
#' Predicts responses for new data using a fitted ridge regression model.
#'
#' @param object An object of class \code{"ridge"}.
#' @param newdata Optional new design matrix.
#' @param ... Additional arguments (ignored).
#'
#' @returns A numeric vector of predictions.
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(50), 10, 5)
#' y <- rnorm(10)
#' fit <- ridge(X, y, lambda = 1)
#' X_new <- matrix(rnorm(25), 5, 5)
#' predict(fit, X_new)
predict.ridge <- function(object, newdata = NULL, ...){
  if (is.null(newdata)) {
    return(object$fitted.values)
  }
  X <- as.matrix(newdata)

  beta <- object$coefficients
  intercept <- beta[1]
  slopes <- beta[-1]

  if (ncol(X) != length(slopes)) {
    stop("Dimensions of newdata do not match model.")
  }

  as.vector(intercept + X %*% slopes)
}
