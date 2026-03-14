#' Extract Fitted Values from Ridge Model
#'
#' Returns the fitted values of a ridge regression model.
#'
#' @param object An object of class \code{"ridge"}.
#' @param ... Additional arguments (ignored).
#'
#' @returns A numeric vector of fitted values.
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(50), 10, 5)
#' y <- rnorm(10)
#' fit <- ridge(X, y, lambda = 1)
#' fitted(fit)
fitted.ridge <- function(object, ...) {
  object$fitted.values
}
