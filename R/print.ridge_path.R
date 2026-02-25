#' Print Method for Ridge Path (S3 Method)
#'
#' Prints a summary of a ridge regression path.
#'
#' @param x An object of class \code{"ridge_path"}.
#' @param ... Additional arguments (ignored).
#'
#' @returns The object \code{x} invisibly.
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(50), 10, 5)
#' y <- rnorm(10)
#' path <- ridge_path(X, y, lambda = c(0.1, 1, 10))
#' print(path)
print.ridge_path <- function(x, ...){
  cat("Call:\n")
  print(x$call)

  cat("\nRidge Coefficient Path\n")
  cat("Number of lambda values:", length(x$lambda), "\n")
  cat("Lambda range:", range(x$lambda), "\n")

  invisible(x)
}
