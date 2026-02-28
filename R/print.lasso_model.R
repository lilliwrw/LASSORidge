#' Print Method for LASSO Model Objects (S3 Method)
#'
#' Prints a summary of a LASSO regression model fit via the `lasso()` function.
#' Displays the number of predictors, number of lambda values, lambda range,
#' and a preview of the coefficient matrix.
#'
#' @param x An object of class \code{"lasso_model"}.
#' @param ... Additional arguments (ignored).
#'
#' @returns Invisibly returns the object \code{x}.
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(50*5), 50, 5)
#' y <- rnorm(50)
#' fit <- lasso(X, y, n_lambda=10)
#' print(fit)
print.lasso_model <- function(x, ...) {
  cat("Call:\n")
  if(!is.null(x$call)) {
    print(x$call)
  } else {
    cat("<original call not stored>\n")
  }

  cat("\nLASSO Regression Path\n")
  cat("Number of predictors:", nrow(x$beta), "\n")
  cat("Number of lambda values:", length(x$lambda_seq), "\n")
  cat("Lambda range: [", min(x$lambda_seq), ", ", max(x$lambda_seq), "]\n\n", sep="")

  cat("Coefficient Matrix (showing first 6 rows and columns if large):\n")
  coef_mat <- x$beta
  max_rows <- min(6, nrow(coef_mat))
  max_cols <- min(6, ncol(coef_mat))
  print(coef_mat[1:max_rows, 1:max_cols, drop = FALSE])

  invisible(x)
}
