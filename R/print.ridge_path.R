#' Print Method for Ridge Path (S3 Method)
#'
#' Prints a summary of a ridge regression path.
#'
#' @param x An object of class \code{"ridge_path"}.
#' @param ... Additional arguments (ignored).
#'
#' @returns Prints path and return the object \code{x} invisibly.
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(50), 10, 5)
#' y <- rnorm(10)
#' path <- ridge_path(X, y, lambda = c(0.1, 1, 10))
#' print(path)
print.ridge_path <- function(x, ...){
  #Call
  cat("Call:\n")
  print(x$call)

  #Path information
  cat("\nRidge Coefficient Path\n")
  cat("Number of lambda values:", length(x$lambda), "\n")
  cat("Lambda range:", range(x$lambda), "\n")

  #Coefficient matrix
  cat("Coefficient Matrix (first rows and columns if large):\n")

  coef_mat <- x$coefficients

  #Only first part if large
  max_rows <- min(6, nrow(coef_mat))
  max_cols <- min(6, ncol(coef_mat))

  print(coef_mat[1:max_rows, 1:max_cols, drop = FALSE])

  invisible(x)
}
