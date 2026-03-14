#' Extract Coefficients from Ridge Model
#'
#' Returns the estimated coefficients including the intercept.
#'
#' @param object An object of class \code{"ridge"}.
#' @param ... Additional arguments (ignored).
#'
#' @returns A named numeric vector of coefficients.
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(50), 10, 5)
#' y <- rnorm(10)
#' fit <- ridge(X, y, lambda = 1)
#' coef(fit)
coef.ridge <- function(object, ...) {
  object$coefficients
}
