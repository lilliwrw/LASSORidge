#' Print Method for Ridge Objects (S3 Method)
#'
#' Prints a summary of a ridge regression model.
#'
#' @param x An object of class \code{"ridge"}.
#' @param ... Additional arguments (ignored).
#'
#' @returns Provides a summary and returns the object \code{x} invisibly.
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(50), 10, 5)
#' y <- rnorm(10)
#' fit <- ridge(X, y, lambda = 1)
#' print(fit)
print.ridge <- function(x, ...){
  cat("Call:\n")
  print(x$call)

  cat("\nRidge Regression\n")
  cat("Lambda:", x$lambda, "\n\n")

  cat("Coefficients:\n")
  print(x$coefficients)

  invisible(x)
}
